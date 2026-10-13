########## FUNÇÕES DE GEOPROCESSAMENTO DE RELATÓRIOS DO SINAN - DF - V44
#
# Este arquivo contém SÓ definições de função — sourcing seguro (sem efeitos
# colaterais) a partir de qualquer script: RUN_GEORREF.R, PREP_CNEFE.R, ou
# futuramente o app.R do Shiny.
#
# Pré-requisito: working directory = pasta GEO kit (contém CNEFE.duckdb e
# GEOCODING_DF_CORRIDO.sql). Rode setwd() antes de chamar georreferenciar_DF().
#
### PACOTES
library(DBI)
library(duckdb)
library(dplyr)

# --------------------------------------------------------------------------
# AUXILIAR: split_sql_rapido
# Separa um script SQL em statements por ';', respeitando aspas simples
# (não quebra dentro de literais como '[,;()]'). Vetorizado via substring(),
# evita o custo O(n²) de um parser char-a-char com paste0.
# --------------------------------------------------------------------------
split_sql_rapido <- function(sql) {
  sql <- gsub("--[^\n]*", "", sql)

  chars <- strsplit(sql, "")[[1]]
  n <- length(chars)
  dentro_aspas <- FALSE
  split_pos <- integer(0)

  for (i in seq_len(n)) {
    ch <- chars[i]
    if (ch == "'") {
      dentro_aspas <- !dentro_aspas
    } else if (ch == ";" && !dentro_aspas) {
      split_pos <- c(split_pos, i)
    }
  }

  starts <- c(1L, split_pos + 1L)
  ends   <- c(split_pos - 1L, n)

  stmts <- trimws(substring(sql, starts, ends))
  stmts[nchar(stmts) > 4]
}

# --------------------------------------------------------------------------
# DIRETÓRIO TEMPORÁRIO DO DUCKDB (spill em disco)
#
# O temp_directory é uma configuração da INSTÂNCIA do banco, não da conexão, e
# o DuckDB recusa trocá-la depois que o diretório atual já foi usado:
# "Not implemented Error: Cannot switch temporary directory after the current
# one has been used". Como o duckdb do R reaproveita a instância por caminho de
# arquivo, isso acontece sempre que uma segunda execução pega a mesma instância
# ainda viva -- por exemplo quando uma execução anterior morreu no meio e
# deixou a conexão aberta, ou quando duas sessões do Shiny rodam no mesmo
# processo R.
#
# Por isso o diretório é um só por processo R, criado na primeira vez e mantido
# (o R limpa o tempdir() da sessão ao encerrar). Antes, cada execução criava um
# diretório novo e o apagava ao final, o que quebrava de duas formas: a troca
# era recusada e, pior, o diretório que a instância ainda usava tinha sido
# apagado -- daí o "IO Error: Failed to create directory" que já aparecia em
# produção.
# --------------------------------------------------------------------------
.spill_duckdb <- new.env()

configurar_temp_duckdb <- function(con) {
  if (is.null(.spill_duckdb$caminho) || !dir.exists(.spill_duckdb$caminho)) {
    .spill_duckdb$caminho <- file.path(tempdir(), "duckdb_spill")
    dir.create(.spill_duckdb$caminho, showWarnings = FALSE, recursive = TRUE)
  }
  caminho <- gsub("\\\\", "/", .spill_duckdb$caminho)

  tryCatch(
    dbExecute(con, sprintf("SET temp_directory = '%s'", caminho)),
    error = function(e) {
      if (!grepl("temporary directory", conditionMessage(e))) stop(e)
      # A instância já está usando um diretório: mantém o dela, apenas garante
      # que ele exista (uma execução antiga pode tê-lo apagado ao terminar).
      em_uso <- tryCatch(
        dbGetQuery(con, "SELECT current_setting('temp_directory') AS d")$d,
        error = function(e2) NA_character_
      )
      if (!is.na(em_uso) && nzchar(em_uso) && !dir.exists(em_uso)) {
        dir.create(em_uso, showWarnings = FALSE, recursive = TRUE)
      }
      message("DuckDB manteve o diretório temporário já em uso nesta sessão R: ", em_uso)
    }
  )
  invisible(.spill_duckdb$caminho)
}

# --------------------------------------------------------------------------
# FALLBACK POR CENTROIDE DE POLÍGONO (usado pelo DF e pelas demais UFs)
#
# Casos sem match em nenhuma fase recebem um ponto representativo do polígono
# da menor unidade territorial conhecida: RA (ou o DF inteiro) no DF;
# município (ou a UF inteira) nas demais UFs. Usa o centroide geométrico do
# polígono, e não a média das coordenadas dos endereços do CNEFE -- a média é
# puxada para onde há mais endereços cadastrados e distorce a posição.
#
# Quando o centroide cai FORA do próprio polígono (unidades em forma de C ou
# recortadas por lago ou mar), usa st_point_on_surface(), que garante um ponto
# interno. Sem isso o caso iria parar em outra unidade ou dentro d'água -- o
# que acontece em 39 dos 4.454 polígonos das malhas em "Shapes UFs" (0,9%),
# entre eles Lago Sul, Vitória, Angra dos Reis e Santo André.
#
# O cálculo é feito em coordenadas planas (SIRGAS 2000 / Brazil Polyconic,
# EPSG:5880): na esfera (s2) a união da UF de Minas Gerais levava ~40 s,
# contra ~4 s no plano, e a diferença de posição fica entre 1 e 122 m no maior
# município de cada UF -- irrelevante para um ponto que representa a unidade
# inteira.
#
# Casos de fallback NÃO recebem setor censitário: o ponto representa a
# unidade inteira, e atribuir a ele o setor onde por acaso cai concentraria
# artificialmente todos os casos da unidade num único setor.
# --------------------------------------------------------------------------

# Cache por chave (UF) no processo R: os pontos de uma malha são calculados uma
# vez e reaproveitados pelas execuções seguintes -- inclusive por outras sessões
# do Shiny no mesmo processo e por cada lote do DF. Se um shapefile for
# substituído, reinicie o processo R para recalcular.
.cache_pontos_poligonos <- new.env()

# Centroide de cada polígono de `geom` (sfc em CRS plano), trocado por
# st_point_on_surface() quando cai fora do próprio polígono.
ponto_representativo <- function(geom) {
  pontos <- sf::st_centroid(geom)
  dentro <- mapply(function(i, hits) i %in% hits,
                   seq_along(pontos), sf::st_intersects(pontos, geom))
  if (any(!dentro)) {
    pontos[!dentro] <- sf::st_point_on_surface(geom[!dentro])
  }
  pontos
}

# Pontos representativos de cada feição de `shape` (identificada pela coluna
# `id`, com nome opcional na coluna `nome`) e da união de todas elas, em graus
# WGS84 -- a mesma referência do LATITUDE/LONGITUDE do CNEFE. Shapefiles sem
# CRS (.prj ausente) são tratados como SIRGAS 2000, padrão das malhas do IBGE.
pontos_poligonos <- function(shape, id, chave, nome = NULL) {
  if (!is.null(.cache_pontos_poligonos[[chave]])) {
    return(.cache_pontos_poligonos[[chave]])
  }

  if (is.na(sf::st_crs(shape))) sf::st_crs(shape) <- 4674
  geom <- sf::st_make_valid(sf::st_transform(sf::st_geometry(shape), 5880))

  em_graus <- function(pontos) sf::st_coordinates(sf::st_transform(pontos, 4326))
  xy       <- em_graus(ponto_representativo(geom))
  xy_uniao <- em_graus(ponto_representativo(sf::st_union(geom)))

  unidades <- data.frame(
    ID        = as.character(shape[[id]]),
    LATITUDE  = unname(xy[, "Y"]),
    LONGITUDE = unname(xy[, "X"]),
    stringsAsFactors = FALSE
  )
  if (!is.null(nome)) unidades$NOME <- as.character(shape[[nome]])

  resultado <- list(
    unidades = unidades,
    uniao    = c(LATITUDE = unname(xy_uniao[1, "Y"]), LONGITUDE = unname(xy_uniao[1, "X"]))
  )
  assign(chave, resultado, envir = .cache_pontos_poligonos)
  resultado
}

# DF: ponto fixo do complexo penitenciário da Papuda (INSTITUCIONAL_FIXO) para
# o caso sem match de endereço -- sem nada, ou só com o centroide da RA --
# cujo logradouro ou complemento cita presídio. Tem prioridade sobre o
# centroide da RA: a posição do presídio é mais precisa que o centro da RA
# informada. O ponto cai na RA Jardim Botânico (conferido na malha
# DF_subdistritos_CD2022), grafada como no CNEFE e no shapefile. Termos de
# REGEX_DETENTO, em palavra inteira -- como substring, CIR levava "RUA
# CIRCULAR" para a Papuda. O complemento entra porque o SQL o usa quando o
# logradouro está vazio. Roda antes de aplicar_centroides_DF().
PONTO_FIXO_PAPUDA <- c(LATITUDE = -15.8647, LONGITUDE = -47.8203)

aplicar_ponto_fixo_DF <- function(sinan_geo) {
  texto <- paste(texto_normalizado(sinan_geo, "NM_LOGRADO"),
                 texto_normalizado(sinan_geo, "NM_COMPLEM"), sep = " | ")
  sem_endereco <- is.na(sinan_geo$FASE_MATCH) | sinan_geo$FASE_MATCH == "CENTROIDE_RA"
  idx <- which(sem_endereco & grepl(REGEX_DETENTO, texto, perl = TRUE))

  sinan_geo$LATITUDE[idx]         <- PONTO_FIXO_PAPUDA[["LATITUDE"]]
  sinan_geo$LONGITUDE[idx]        <- PONTO_FIXO_PAPUDA[["LONGITUDE"]]
  sinan_geo$NM_SUBDIST[idx]       <- "Jardim Botânico"
  sinan_geo$FASE_MATCH[idx]       <- "INSTITUCIONAL_FIXO"
  sinan_geo$GEORREFERENCIADO[idx] <- 1L
  sinan_geo
}

# DF: troca as coordenadas dos casos de fallback pelos pontos dos polígonos.
#   - CENTROIDE_RA: o SQL decide QUAL RA (pelo nome informado) e grava a
#     média dos endereços do CNEFE dela; aqui a coordenada é trocada pelo
#     ponto do polígono da RA. Os nomes de NM_SUBDIST no CNEFE e no shapefile
#     são idênticos (33 RAs, conferidos na base de produção).
#   - Sem match nenhum (GEORREFERENCIADO 0 no SQL): ponto do polígono do DF
#     inteiro, rotulado CENTROIDE_DF. O GEORREFERENCIADO final é definido
#     depois, em classificar_georreferenciado().
# Sem o shapefile das RAs, mantém o comportamento anterior (média do CNEFE
# para a RA, ponto fixo no centro de Brasília para o DF), com aviso.
aplicar_centroides_DF <- function(sinan_geo,
                                  caminho_shp = file.path("Shapes UFs", "DF_subdistritos_CD2022.shp")) {
  pontos <- tryCatch(
    pontos_poligonos(sf::st_read(caminho_shp, quiet = TRUE), id = "NM_SUBDIST", chave = "DF"),
    error = function(e) {
      warning("Fallback do DF sem o shapefile das RAs (", conditionMessage(e),
              "): mantendo média do CNEFE por RA e ponto fixo para o DF.")
      NULL
    }
  )

  if (!is.null(pontos)) {
    idx <- which(sinan_geo$FASE_MATCH == "CENTROIDE_RA")
    m <- match(stringi::stri_trans_nfc(sinan_geo$NM_SUBDIST[idx]),
               stringi::stri_trans_nfc(pontos$unidades$ID))
    if (anyNA(m)) {
      warning("RA sem polígono no shapefile (mantida a média do CNEFE): ",
              paste(unique(sinan_geo$NM_SUBDIST[idx[is.na(m)]]), collapse = ", "))
    }
    ok <- !is.na(m)
    sinan_geo$LATITUDE[idx[ok]]  <- pontos$unidades$LATITUDE[m[ok]]
    sinan_geo$LONGITUDE[idx[ok]] <- pontos$unidades$LONGITUDE[m[ok]]
    ponto_df <- pontos$uniao
  } else {
    ponto_df <- c(LATITUDE = -15.7801, LONGITUDE = -47.9292)
  }

  sem_match <- which(sinan_geo$GEORREFERENCIADO == 0L)
  sinan_geo$LATITUDE[sem_match]         <- ponto_df[["LATITUDE"]]
  sinan_geo$LONGITUDE[sem_match]        <- ponto_df[["LONGITUDE"]]
  sinan_geo$FASE_MATCH[sem_match]       <- "CENTROIDE_DF"
  sinan_geo
}

# Demais UFs: casos sem coordenada depois da última fase recebem o ponto do
# polígono do município informado (CENTROIDE_MUN) ou, se o município estiver
# ausente, "Ignorado" ou fora da malha da UF, o da UF inteira (CENTROIDE_UF).
# `pontos` vem de pontos_poligonos() sobre a malha municipal da UF, com
# nome = "NM_MUN". Os demais campos do match (ENDERECO_CNEFE, COD_SETOR,
# similarity, DISTANCIA_NUM) seguem vazios; NM_MUN_CNEFE recebe o nome do
# município no CENTROIDE_MUN, como o NM_SUBDIST no CENTROIDE_RA do DF, para o
# caso contar no seu município nas métricas.
#
# Um nível de bairro (CENTROIDE_BAIRRO) entraria antes do município, ligando
# NM_BAIRRO a uma malha de bairros -- que ainda não está em "Shapes UFs".
aplicar_fallback_centroide_UF <- function(resultado, pontos) {
  sem_match <- which(is.na(resultado$LATITUDE) | is.na(resultado$LONGITUDE))
  if (length(sem_match) == 0) return(resultado)

  # MUNIRESAT tem 6 dígitos; o CD_MUN da malha do IBGE tem 7 (com o dígito
  # verificador no fim).
  mun <- if ("MUNIRESAT" %in% names(resultado)) {
    substr(trimws(as.character(resultado$MUNIRESAT[sem_match])), 1, 6)
  } else {
    rep(NA_character_, length(sem_match))
  }
  m <- match(mun, substr(pontos$unidades$ID, 1, 6))

  i_mun <- sem_match[!is.na(m)]
  i_uf  <- sem_match[is.na(m)]

  resultado$LATITUDE[i_mun]  <- pontos$unidades$LATITUDE[m[!is.na(m)]]
  resultado$LONGITUDE[i_mun] <- pontos$unidades$LONGITUDE[m[!is.na(m)]]
  resultado$FASE[i_mun]      <- "CENTROIDE_MUN"
  if ("NM_MUN_CNEFE" %in% names(resultado) && !is.null(pontos$unidades$NOME)) {
    resultado$NM_MUN_CNEFE[i_mun] <- pontos$unidades$NOME[m[!is.na(m)]]
  }

  resultado$LATITUDE[i_uf]  <- pontos$uniao[["LATITUDE"]]
  resultado$LONGITUDE[i_uf] <- pontos$uniao[["LONGITUDE"]]
  resultado$FASE[i_uf]      <- "CENTROIDE_UF"

  resultado
}

# --------------------------------------------------------------------------
# CLASSIFICAÇÃO FINAL DE CADA CASO (DF e demais UFs): GEORREFERENCIADO e
# INDICADOR
#
# GEORREFERENCIADO -- o caso tem posição de endereço?
#   1: casou numa fase de endereço: determinística, ou probabilística com
#      similaridade >= limite. Nas demais UFs o limite escolhido no painel já
#      é exigido dentro das fases 4 e 5 -- abaixo dele a fase não casa e o
#      caso segue para o centroide. No DF valem os limites fixos do SQL
#      (fases 12, 15 e 18; a 13 só desempata), e as 18 fases contam como 1, assim como o
#      ponto fixo do complexo penitenciário (INSTITUCIONAL_FIXO).
#   0: centroide de qualquer tipo (RA, DF, município, UF), sem coordenada, ou
#      endereço vazio.
# A decisão vem do rótulo da fase, e não de recalcular similarity >= limite
# sobre a coluna final: na fase 4.2 a similaridade é reduzida pela distância
# do número DEPOIS do corte, e um caso aceito com 0,50 aparece como 0,494.
#
# INDICADOR -- o endereço informado é de residência?
#   0: detento ou pessoa em situação de rua;
#   1: os demais. Todo caso tem ao menos a referência da UF processada.
# Independente do GEORREFERENCIADO: um detento cujo endereço casou numa fase
# continua com GEORREFERENCIADO 1.
# --------------------------------------------------------------------------

# Textos que, sozinhos no campo, dizem que não há endereço.
TEXTOS_SEM_ENDERECO <- c("SEM ENDERECO", "NAO INFORMADO", "IGNORADO", "NA")

# Palavras inteiras (\b): como substring, CIR casava com "RUA CIRCULAR".
# REGEX_DETENTO também decide o ponto fixo da Papuda (aplicar_ponto_fixo_DF).
# Os mesmos termos estão na macro nao_residencial() do
# GEOCODING_DF_CORRIDO.sql, que as fases 13 e 18 usam para pular esses
# endereços -- manter as listas juntas.
REGEX_DETENTO <- paste0(
  "\\b(PAPUDA|PRESIDIO|PENITENCIARI[AO]|PRISIONAL|PRISAO|CADEIA PUBLICA|",
  "CENTRO DE DETENCAO|DETENTO|APENADO|PRESO|CDP|CPP|CIR|PDF)\\b"
)
REGEX_SITUACAO_RUA <- paste0(
  "\\b(SITUACAO DE RUA|MORADORA? (DE|NA) RUA|POPULACAO DE RUA|POP RUA|",
  "ALBERGUE|ALBERGADO|ANDARILHO)\\b"
)

# Maiúsculas e sem acento, com "" no lugar de NA (e de coluna ausente).
texto_normalizado <- function(base, col) {
  if (!col %in% names(base)) return(rep("", nrow(base)))
  x <- toupper(stringi::stri_trans_general(trimws(as.character(base[[col]])), "Latin-ASCII"))
  x[is.na(x)] <- ""
  x
}

# Campo de população especial da ficha do SINAN (POP_LIBER, POP_RUA):
# 1 = Sim, 2 = Não, 9 = Ignorado; arquivos exportados com rótulo trazem "Sim".
campo_sinan_sim <- function(base, col) {
  if (!col %in% names(base)) return(rep(FALSE, nrow(base)))
  texto_normalizado(base, col) %in% c("1", "SIM")
}

# `logradouro`/`complemento`: vetores do endereço informado. Quem declarou só
# o endereço completo (padronizar_colunas_usuario) tem logradouro e
# complemento NA -- nesse caso passe o endereço declarado como logradouro.
classificar_georreferenciado <- function(fase, latitude, logradouro, complemento) {
  vazio <- function(x) {
    x <- toupper(stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))
    is.na(x) | x == "" | x %in% TEXTOS_SEM_ENDERECO
  }
  fase <- as.character(fase)
  por_endereco <- !is.na(fase) & !startsWith(fase, "CENTROIDE_") & !is.na(latitude)
  as.integer(por_endereco & !(vazio(logradouro) & vazio(complemento)))
}

# Detento: POP_LIBER = Sim, ou o endereço cita presídio/penitenciária.
# Situação de rua: POP_RUA = Sim, ou o endereço diz "situação de rua",
# "morador de rua", albergue etc. Os campos POP_* existem na ficha de
# tuberculose do SINAN; sem eles (outras fichas, planilhas próprias), só o
# texto decide.
classificar_indicador <- function(base) {
  texto <- do.call(paste, c(
    lapply(c("NM_LOGRADO", "NM_COMPLEM", "NM_BAIRRO", "address"),
           function(col) texto_normalizado(base, col)),
    sep = " | "
  ))
  detento <- campo_sinan_sim(base, "POP_LIBER") | grepl(REGEX_DETENTO, texto, perl = TRUE)
  rua     <- campo_sinan_sim(base, "POP_RUA")   | grepl(REGEX_SITUACAO_RUA, texto, perl = TRUE)
  as.integer(!(detento | rua))
}

# Tabela de cobertura (Métricas Detalhadas do DF) a partir do GEORREFERENCIADO.
resumo_georreferenciado <- function(georreferenciado) {
  n <- c(sum(georreferenciado == 1L, na.rm = TRUE),
         sum(georreferenciado == 0L, na.rm = TRUE))
  data.frame(
    GEORREFERENCIADO = c(1L, 0L),
    DESCRICAO = c("Georreferenciado (endereço)",
                  "Não georreferenciado (centroide ou endereço vazio)"),
    N   = n,
    PCT = round(100 * n / length(georreferenciado), 2)
  )
}

# --------------------------------------------------------------------------
# PIPELINE PRINCIPAL: georreferenciar_DF
# Processa um data.frame de notificações SINAN contra o CNEFE.duckdb.
#
# Pré-requisito: rode PREP_CNEFE.R uma vez antes do primeiro uso (materializa
# as tabelas estáticas do CNEFE como VIEWs sobre tabelas persistentes — sem
# isso, GEOCODING_DF_CORRIDO.sql falha ao referenciar cnefe.CNEFE_AUX etc.)
#
# progress_callback: função opcional chamada a cada fase concluída, recebe
# list(fase = "MATCH_FASE8", n = 15). Usada pelo Shiny para atualizar a barra
# de progresso; sem efeito quando NULL (uso standalone via RUN_GEORREF.R).
# --------------------------------------------------------------------------
georreferenciar_DF <- function(doenca, progress_callback = NULL) {

    begin <- Sys.time()

    doenca$NU_NOTIFIC <- as.character(doenca$NU_NOTIFIC)
    quantidade_de_casos <- nrow(doenca)

    print(paste0("Iniciando georreferenciamento DF | Registros: ", quantidade_de_casos))

    # ------------------------------------------------------------------------
    # CONEXÃO ISOLADA: banco de trabalho descartável + CNEFE.duckdb somente leitura.
    # CNEFE.duckdb nunca é aberto para escrita — múltiplos usuários/lotes podem
    # anexá-lo simultaneamente sem conflito de lock (essencial em produção/Shiny
    # com múltiplas sessões concorrentes). O banco de trabalho é um arquivo
    # temporário único por chamada: descartado ao final via on.exit, sem
    # necessidade de DROP TABLE manual e sem risco de colisão entre execuções
    # concorrentes ou entre lotes do batching.
    # ------------------------------------------------------------------------
    work_db    <- tempfile(pattern = "geo_work_", fileext = ".duckdb")
    duckdb_tmp <- tempfile(pattern = "geo_spill_")
    dir.create(duckdb_tmp, showWarnings = FALSE, recursive = TRUE)

    con <- dbConnect(duckdb(), dbdir = work_db, read_only = FALSE)

    on.exit({
      try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
      try(unlink(work_db), silent = TRUE)
      try(unlink(duckdb_tmp, recursive = TRUE), silent = TRUE)
    }, add = TRUE)

    cnefe_path <- normalizePath("CNEFE.duckdb", mustWork = TRUE)
    dbExecute(con, sprintf("ATTACH '%s' AS cnefe (READ_ONLY)", gsub("\\\\", "/", cnefe_path)))

    # Limita RAM para evitar crash de sistema operacional.
    # 4 GB é seguro para máquinas com 8 GB+.
    dbExecute(con, "SET memory_limit = '4GB'")
    dbExecute(con, sprintf("SET temp_directory = '%s'", gsub("\\\\", "/", duckdb_tmp)))
    dbExecute(con, "SET threads = 2")
    dbExecute(con, "SET preserve_insertion_order = false")

    doenca[] <- lapply(doenca, function(x) {
      if (is.character(x)) iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
      else x
    })

    dbWriteTable(con, "SINAN_TEMP", doenca, temporary = TRUE, overwrite = TRUE)
    print(paste0("SINANDF escrito: ", nrow(doenca), " registros"))

    sql_path <- "GEOCODING_DF_CORRIDO.sql"

    if (!file.exists(sql_path)) {
      stop("GEOCODING_DF_CORRIDO.sql não encontrado em: ", getwd())
    }

    sql_completo <- paste(
      readLines(sql_path, encoding = "UTF-8", warn = FALSE),
      collapse = "\n"
    )

    # CNEFEDF é a única tabela bruta persistente lida pelo pipeline — vive no
    # banco anexado somente-leitura ("cnefe"), não no banco de trabalho.
    sql_completo <- gsub("\\bCNEFEDF\\b", "cnefe.CNEFEDF", sql_completo)

    statements <- split_sql_rapido(sql_completo)

    eh_executavel <- function(s) {
      s_limpo <- trimws(gsub("--[^\n]*", "", s))
      if (nchar(s_limpo) < 5) return(FALSE)
      s_upper <- toupper(s_limpo)
      if (grepl("^CREATE|^DROP|^DELETE|^INSERT|^UPDATE", s_upper)) return(TRUE)
      if (grepl("^SELECT", s_upper)) return(TRUE)
      return(FALSE)
    }

    statements_exec <- statements[sapply(statements, eh_executavel)]
    print(paste0("Pipeline SQL: ", length(statements_exec), " statements"))

    fases_concluidas <- list()

    for (i in seq_along(statements_exec)) {
      stmt       <- statements_exec[[i]]
      stmt_upper <- toupper(trimws(stmt))

      tryCatch({

        if (grepl("^SELECT", stmt_upper)) {

          res <- dbGetQuery(con, stmt)

          if (grepl("GEORREF_COMPLETO|total_georreferenciados|sem_georref", stmt_upper)) {
            print(paste0("  ", names(res)[1], ": ", res[[1]][1]))

          } else if (grepl("FASE_MATCH", stmt_upper) && nrow(res) > 1) {
            fases_concluidas[["resumo"]] <- res

          } else if (nrow(res) > 0) {
            n_col <- if ("n" %in% names(res)) res$n[1] else res[[1]][1]
            fase  <- regmatches(stmt_upper,
                                regexpr("MATCH_FASE\\d+|MATCH_POSICIONAL|CENTROIDE_RA",
                                        stmt_upper))
            if (length(fase) > 0) {
              print(paste0("  ", fase, ": ", n_col))
              fases_concluidas[[fase]] <- n_col
              if (!is.null(progress_callback)) {
                progress_callback(list(fase = fase, n = n_col))
              }
            }
          }

        } else {

          dbExecute(con, stmt)

          if (grepl("CREATE.*(SINAN\\b|CNEFE_AUX|MATCH_POSICIONAL|MATCH_FASE\\d+|CENTROIDE_RA|GEORREF_COMPLETO|SINAN_GEO|GEORREF_FALHA)",
                    stmt_upper)) {
            tb <- regmatches(stmt_upper, regexpr("(TABLE|VIEW)\\s+\\w+", stmt_upper))
            tb <- trimws(gsub("TABLE|VIEW", "", trimws(tb)))
            if (length(tb) > 0) print(paste0("  Criado: ", tb))
          }
        }

      }, error = function(e) {
        msg <- conditionMessage(e)
        if (!grepl("already exists|unsuccessful or closed|pending query", msg, ignore.case = TRUE)) {
          warning(paste0("[stmt ", i, "] ", substr(msg, 1, 300)))
        }
      })
    }

    tabelas_existentes <- dbGetQuery(con, "SHOW TABLES")
    print(tabelas_existentes)

    print("Coletando resultado final...")

    sinan_geo <- tryCatch(
      dbGetQuery(con, "SELECT * FROM SINAN_GEO"),
      error = function(e) {
        warning("SINAN_GEO não disponível: ", conditionMessage(e))
        NULL
      }
    )

    georref_falha <- tryCatch(
      dbGetQuery(con, "SELECT * FROM GEORREF_FALHA"),
      error = function(e) {
        warning("GEORREF_FALHA não disponível: ", conditionMessage(e))
        data.frame()
      }
    )

    contagem_por_fase <- tryCatch(
      dbGetQuery(con, "
        SELECT FASE_MATCH, COUNT(*) AS N,
               ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS PCT
        FROM SINAN_GEO
        WHERE FASE_MATCH IS NOT NULL
        GROUP BY FASE_MATCH
        ORDER BY N DESC
      "),
      error = function(e) data.frame()
    )

    contagem_por_ra <- tryCatch(
      dbGetQuery(con, "
        SELECT NM_SUBDIST, COUNT(*) AS N,
               ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS PCT
        FROM SINAN_GEO
        WHERE NM_SUBDIST IS NOT NULL
        GROUP BY NM_SUBDIST
        ORDER BY N DESC
      "),
      error = function(e) data.frame()
    )

    # Conexão e arquivos temporários (work_db, duckdb_tmp) são limpos
    # automaticamente pelo on.exit() registrado no início da função.
    print("Resultado coletado. Conexão será encerrada automaticamente.")

    if (is.null(sinan_geo) || nrow(sinan_geo) == 0) {
      stop("Pipeline não produziu resultados. Verifique os dados de entrada.")
    }

    # Ponto fixo do complexo penitenciário -- ver aplicar_ponto_fixo_DF().
    sinan_geo <- aplicar_ponto_fixo_DF(sinan_geo)

    # Centroide do polígono da RA (CENTROIDE_RA) e do DF (CENTROIDE_DF) para
    # os casos sem match -- ver aplicar_centroides_DF().
    sinan_geo <- aplicar_centroides_DF(sinan_geo)

    # Classificação final: até aqui GEORREFERENCIADO era o estado do SQL
    # (1 match, 2 centroide da RA, 0 sem nada), usado acima para achar quem
    # recebe o centroide do DF.
    sinan_geo$GEORREFERENCIADO <- classificar_georreferenciado(
      sinan_geo$FASE_MATCH, sinan_geo$LATITUDE, sinan_geo$NM_LOGRADO, sinan_geo$NM_COMPLEM
    )
    sinan_geo$INDICADOR <- classificar_indicador(sinan_geo)
    resumo_cobertura <- resumo_georreferenciado(sinan_geo$GEORREFERENCIADO)

    end   <- Sys.time()
    total <- end - begin

    n_georref_end <- sum(sinan_geo$GEORREFERENCIADO == 1L, na.rm = TRUE)
    n_nao_georref <- sum(sinan_geo$GEORREFERENCIADO == 0L, na.rm = TRUE)

    cat("\n")
    cat("================================================================\n")
    cat("  GEORREFERENCIAMENTO DF — RELATÓRIO FINAL\n")
    cat("================================================================\n")
    cat(sprintf("  Total de casos processados : %d\n", quantidade_de_casos))
    cat(sprintf("  Tempo de execução          : %.2f minutos\n",
                round(as.numeric(total, units = "mins"), 2)))
    cat("----------------------------------------------------------------\n")
    cat("  COBERTURA\n")
    cat("----------------------------------------------------------------\n")
    if (nrow(resumo_cobertura) > 0) {
      for (i in seq_len(nrow(resumo_cobertura))) {
        cat(sprintf("  %-52s %5d  (%5.2f%%)\n",
                    resumo_cobertura$DESCRICAO[i],
                    resumo_cobertura$N[i],
                    resumo_cobertura$PCT[i]))
      }
    }
    cat("----------------------------------------------------------------\n")
    cat("  MATCHES POR FASE\n")
    cat("----------------------------------------------------------------\n")
    if (nrow(contagem_por_fase) > 0) {
      for (i in seq_len(nrow(contagem_por_fase))) {
        cat(sprintf("  %-32s %5d  (%5.2f%%)\n",
                    contagem_por_fase$FASE_MATCH[i],
                    contagem_por_fase$N[i],
                    contagem_por_fase$PCT[i]))
      }
    }
    cat("----------------------------------------------------------------\n")
    cat("  CASOS POR REGIÃO ADMINISTRATIVA\n")
    cat("----------------------------------------------------------------\n")
    if (nrow(contagem_por_ra) > 0) {
      for (i in seq_len(nrow(contagem_por_ra))) {
        cat(sprintf("  %-32s %5d  (%5.2f%%)\n",
                    contagem_por_ra$NM_SUBDIST[i],
                    contagem_por_ra$N[i],
                    contagem_por_ra$PCT[i]))
      }
    }
    cat("================================================================\n\n")

    return(
      list(
        base_processada  = sinan_geo,
        georref_falha    = georref_falha,
        metricas = list(
          quantidade_de_casos       = quantidade_de_casos,
          n_georreferenciados       = n_georref_end,
          n_nao_georreferenciados   = n_nao_georref,
          cobertura                 = resumo_cobertura,
          contagem_por_fase         = contagem_por_fase,
          contagem_por_ra           = contagem_por_ra,
          fases_concluidas          = fases_concluidas,
          tempo_de_processamento    = total
        )
      )
    )
}

# --------------------------------------------------------------------------
# WRAPPER: georreferenciar_DF_batches
# Divide o input em lotes e combina os resultados.
# Necessário para datasets > ~8.000 registros onde fases de non-equi join
# sem filtro de RA causavam OOM (corrigido na FASE14; mantemos o batching
# como camada extra de segurança).
#
# temp_dir: por padrão é único a cada chamada (tempfile()) — seguro para uso
# em produção/Shiny com múltiplas sessões concorrentes, sem risco de um
# usuário sobrescrever os lotes de outro. Se quiser retomar uma execução
# longa após um crash local, passe um caminho fixo explicitamente:
#   georreferenciar_DF_batches(doenca, temp_dir = "C:/temp/meu_job_lotes")
# --------------------------------------------------------------------------
# progress_callback: função opcional chamada a cada lote/fase, recebe
# list(lote = 2, n_lotes = 19, fase = "MATCH_FASE8", n = 15). O campo `fase`
# fica NULL na notificação de início de lote. Usada pelo Shiny para atualizar
# a barra de progresso; sem efeito quando NULL (uso standalone).
georreferenciar_DF_batches <- function(doenca, batch_size = 3000,
                                        temp_dir = tempfile(pattern = "geo_lotes_"),
                                        progress_callback = NULL) {

  n_total <- nrow(doenca)
  n_lotes <- ceiling(n_total / batch_size)

  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  cat(sprintf("\n[BATCHING] %d registros → %d lotes de ~%d\n", n_total, n_lotes, batch_size))
  cat(sprintf("[BATCHING] Lotes salvos em: %s\n\n", temp_dir))

  # Processa cada lote e salva em disco imediatamente — evita acúmulo na RAM.
  # Se o R crashar, basta retomar a partir do lote que faltou.
  for (b in seq_len(n_lotes)) {
    rds_path <- file.path(temp_dir, sprintf("lote_%03d.rds", b))

    if (file.exists(rds_path)) {
      cat(sprintf(">>> LOTE %d/%d  já processado — pulando\n", b, n_lotes))
      next
    }

    idx_inicio <- (b - 1) * batch_size + 1
    idx_fim    <- min(b * batch_size, n_total)
    lote       <- doenca[idx_inicio:idx_fim, , drop = FALSE]

    cat(sprintf(">>> LOTE %d/%d  (linhas %d–%d, %d registros)\n",
                b, n_lotes, idx_inicio, idx_fim, nrow(lote)))

    if (!is.null(progress_callback)) {
      progress_callback(list(lote = b, n_lotes = n_lotes, fase = NULL, n = NULL))
    }

    fase_callback <- if (is.null(progress_callback)) NULL else function(info) {
      progress_callback(list(lote = b, n_lotes = n_lotes, fase = info$fase, n = info$n))
    }

    res <- tryCatch(
      georreferenciar_DF(lote, progress_callback = fase_callback),
      error = function(e) {
        warning(sprintf("Lote %d falhou: %s", b, conditionMessage(e)))
        NULL
      }
    )

    rm(lote)

    if (!is.null(res)) {
      saveRDS(res, rds_path)
      cat(sprintf("<<< LOTE %d salvo em disco\n\n", b))
    } else {
      cat(sprintf("<<< LOTE %d FALHOU (sem arquivo salvo)\n\n", b))
    }

    rm(res)
    gc()
  }

  # Lê os lotes do disco e consolida — apenas o necessário na RAM de uma vez
  rds_files <- list.files(temp_dir, pattern = "^lote_\\d+\\.rds$", full.names = TRUE)
  rds_files <- sort(rds_files)

  if (length(rds_files) == 0)
    stop("Nenhum lote processado com sucesso. Verifique os dados e a conexão com o banco.")

  if (length(rds_files) < n_lotes)
    warning(sprintf("%d de %d lotes estão faltando no diretório temporário.",
                    n_lotes - length(rds_files), n_lotes))

  lotes_ok <- lapply(rds_files, readRDS)
  metricas_lotes  <- lapply(lotes_ok, `[[`, "metricas")

  base_processada <- do.call(rbind, lapply(lotes_ok, `[[`, "base_processada"))
  georref_falha   <- do.call(rbind, lapply(lotes_ok, `[[`, "georref_falha"))

  n_geo <- sum(base_processada$GEORREFERENCIADO == 1L, na.rm = TRUE)
  n_nao <- sum(base_processada$GEORREFERENCIADO == 0L, na.rm = TRUE)

  cobertura_total <- resumo_georreferenciado(base_processada$GEORREFERENCIADO)

  fases_lista <- Filter(function(x) !is.null(x) && nrow(x) > 0,
                        lapply(lotes_ok, function(r) r$metricas$contagem_por_fase))
  if (length(fases_lista) > 0) {
    fases_raw <- do.call(rbind, fases_lista)
    contagem_por_fase <- aggregate(N ~ FASE_MATCH, data = fases_raw, FUN = sum)
    contagem_por_fase$PCT <- round(100 * contagem_por_fase$N / nrow(base_processada), 2)
    contagem_por_fase <- contagem_por_fase[order(-contagem_por_fase$N), ]
  } else {
    contagem_por_fase <- data.frame()
  }

  ra_lista <- Filter(function(x) !is.null(x) && nrow(x) > 0,
                     lapply(lotes_ok, function(r) r$metricas$contagem_por_ra))
  if (length(ra_lista) > 0) {
    ra_raw <- do.call(rbind, ra_lista)
    contagem_por_ra <- aggregate(N ~ NM_SUBDIST, data = ra_raw, FUN = sum)
    contagem_por_ra$PCT <- round(100 * contagem_por_ra$N / nrow(base_processada), 2)
    contagem_por_ra <- contagem_por_ra[order(-contagem_por_ra$N), ]
  } else {
    contagem_por_ra <- data.frame()
  }

  tempo_total <- Reduce(`+`, lapply(metricas_lotes, `[[`, "tempo_de_processamento"))

  cat("\n================================================================\n")
  cat("  GEORREFERENCIAMENTO DF — RELATÓRIO CONSOLIDADO (BATCHING)\n")
  cat("================================================================\n")
  cat(sprintf("  Total de casos processados : %d\n", n_total))
  cat(sprintf("  Lotes processados          : %d/%d\n", length(lotes_ok), n_lotes))
  cat(sprintf("  Tempo total de execução    : %.2f minutos\n",
              as.numeric(tempo_total, units = "mins")))
  cat("----------------------------------------------------------------\n")
  cat("  COBERTURA\n")
  cat("----------------------------------------------------------------\n")
  for (i in seq_len(nrow(cobertura_total)))
    cat(sprintf("  %-52s %5d  (%5.2f%%)\n",
                cobertura_total$DESCRICAO[i],
                cobertura_total$N[i],
                cobertura_total$PCT[i]))
  cat("================================================================\n\n")

  return(list(
    base_processada = base_processada,
    georref_falha   = if (nrow(georref_falha) > 0) georref_falha else data.frame(),
    metricas = list(
      quantidade_de_casos       = n_total,
      n_lotes                   = n_lotes,
      lotes_processados         = length(lotes_ok),
      n_georreferenciados       = n_geo,
      n_nao_georreferenciados   = n_nao,
      cobertura                 = cobertura_total,
      contagem_por_fase         = contagem_por_fase,
      contagem_por_ra           = contagem_por_ra,
      metricas_por_lote         = metricas_lotes,
      tempo_de_processamento    = tempo_total
    )
  ))
}
