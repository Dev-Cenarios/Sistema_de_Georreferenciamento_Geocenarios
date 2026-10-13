########## FUNÇÕES DA ABA GRÁFICOS E DA PRECISÃO POSICIONAL
#
# Só definições de função e constantes (sourcing seguro), usadas pelo app.R:
#   - classificar_precisao() / grupo_precisao(): onde cada caso caiu no mapa,
#     a partir da fase de match que o resolveu
#   - preparar_graficos(): tudo o que a aba Gráficos precisa, calculado sobre a
#     base final ANTES do descarte de colunas derivadas -- quando o usuário
#     declara as próprias colunas, FASE, MUNIRESAT e Regiao_adm saem da base
#   - grafico_*(): os ggplots da aba
#
# Paleta: uma leitura só em todo o painel (gráficos e mapa) --
#   azul = posição por endereço, laranja = centroide de polígono (fallback),
#   cinza = sem coordenada.
# Azul e laranja são as duas primeiras cores da paleta categórica de referência
# e passam no validador de paleta (dataviz/validate_palette.js) sobre superfície
# branca, inclusive com todos os pares -- o caso do mapa de pontos: separação
# para daltonismo ΔE 24,7 (alvo >= 8), visão normal ΔE 33,6, contraste >= 3:1.
# O cinza (3,6:1 sobre o branco) tem pouco contraste contra as duas cores, por
# isso vai sempre separado por vão branco e identificado na legenda.

COR_ENDERECO  <- "#2a78d6"
COR_CENTROIDE <- "#eb6834"
COR_SEM_COORD <- "#898781"
CORES_GRUPO   <- c("Endereço" = COR_ENDERECO, "Centroide" = COR_CENTROIDE,
                   "Sem coordenada" = COR_SEM_COORD)

TINTA_PRIMARIA   <- "#0b0b0b"
TINTA_SECUNDARIA <- "#52514e"
TINTA_MUTED      <- "#898781"
COR_GRADE        <- "#e1e0d9"

# --------------------------------------------------------------------------
# PRECISÃO POSICIONAL
# --------------------------------------------------------------------------

# Níveis, do mais preciso para o menos preciso. A similaridade não serve para
# isso: as fases determinísticas gravam similarity = 1 inclusive quando o caso
# não tinha número e o ponto caiu no início do logradouro (fases 2.2, 3.3 e
# 4.3). O rótulo da fase existe para todos os casos e diz onde o ponto caiu.
NIVEIS_PRECISAO_UF <- c(
  "Número exato", "Número aproximado", "Endereço por similaridade",
  "Início do logradouro", "Centroide do município", "Centroide da UF",
  "Sem coordenada", "Não classificado"
)
NIVEIS_PRECISAO_DF <- c(
  "Endereço", "Endereço aproximado", "Local fixo (instituição)",
  "Centroide da RA", "Centroide do DF", "Sem coordenada", "Não classificado"
)

# Demais UFs:
#   1.x, 3.1, 4.1 ........................ número exato
#   2.1, 3.2, 4.2 ........................ número mais próximo -- exato quando
#                                          DISTANCIA_NUM = 0, aproximado se > 0
#   5 (A.5, G.5) ......................... endereço completo por Jaro-Winkler
#   2.2, 3.3, 4.3 ........................ sem número: âncora no número 0/1,
#                                          ou seja, no início do logradouro
# DF: rótulos terminados em APROX (quadra/conjunto aproximados) são "Endereço
# aproximado"; os demais rótulos de match, "Endereço"; INSTITUCIONAL_FIXO é o
# ponto fixo do complexo penitenciário.
classificar_precisao <- function(fase, distancia_num, latitude, eh_df) {
  fase <- as.character(fase)
  if (eh_df) {
    r <- dplyr::case_when(
      is.na(latitude)                ~ "Sem coordenada",
      fase == "CENTROIDE_DF"         ~ "Centroide do DF",
      fase == "CENTROIDE_RA"         ~ "Centroide da RA",
      fase == "INSTITUCIONAL_FIXO"   ~ "Local fixo (instituição)",
      grepl("APROX", fase)           ~ "Endereço aproximado",
      !is.na(fase)                   ~ "Endereço",
      TRUE                           ~ "Não classificado"
    )
    return(factor(r, levels = NIVEIS_PRECISAO_DF))
  }

  sub <- sub("^FASE [A-Z]\\.", "", fase)   # "FASE A.2.1" -> "2.1"; "FASE G.5" -> "5"
  distancia_num <- suppressWarnings(as.numeric(distancia_num))
  r <- dplyr::case_when(
    is.na(latitude)                               ~ "Sem coordenada",
    fase == "CENTROIDE_UF"                        ~ "Centroide da UF",
    fase == "CENTROIDE_MUN"                       ~ "Centroide do município",
    sub %in% c("2.2", "3.3", "4.3")               ~ "Início do logradouro",
    sub == "5"                                    ~ "Endereço por similaridade",
    sub %in% c("2.1", "3.2", "4.2") &
      !is.na(distancia_num) & distancia_num > 0   ~ "Número aproximado",
    sub %in% c("1", "1.1", "1.2", "3.1", "4.1",
               "2.1", "3.2", "4.2")               ~ "Número exato",
    TRUE                                          ~ "Não classificado"
  )
  factor(r, levels = NIVEIS_PRECISAO_UF)
}

# Agrupa os níveis nas três leituras da paleta.
grupo_precisao <- function(precisao) {
  p <- as.character(precisao)
  dplyr::case_when(
    is.na(p) | p == "Sem coordenada" ~ "Sem coordenada",
    startsWith(p, "Centroide")       ~ "Centroide",
    TRUE                             ~ "Endereço"
  )
}

# --------------------------------------------------------------------------
# ORDEM DE EXECUÇÃO DAS FASES (gráfico de cobertura acumulada)
# --------------------------------------------------------------------------
ORDEM_FASES_UF <- c(
  "FASE A.1.1", "FASE A.1.2", "FASE B.1",
  "FASE A.2.1", "FASE A.2.2", "FASE B.2.1", "FASE B.2.2",
  "FASE A.3.1", "FASE A.3.2", "FASE A.3.3", "FASE B.3.1", "FASE B.3.2", "FASE B.3.3",
  "FASE A.4.1", "FASE A.4.2", "FASE A.4.3", "FASE B.4.1", "FASE B.4.2", "FASE B.4.3",
  "FASE A.5", "FASE G.5",
  "CENTROIDE_MUN", "CENTROIDE_UF"
)

# Rótulos do DF na ordem em que aparecem em GEOCODING_DF_CORRIDO.sql (a ordem
# de execução), mais os dois atribuídos no R depois do SQL. QUADRA_APROX sai
# das fases 5, 7 e 14 e fica na posição da primeira.
ORDEM_ROTULOS_DF <- c(
  "POSICIONAL", "POSICIONAL_NORM", "SUBFIXO_LETRA", "SO_LOTE", "QUADRA_APROX",
  "IGNORADO", "CONJ_EXATO_APROX", "CONJ_PROXIMO_APROX", "QUADRA_EXATA_CONJ_EXATO",
  "QUADRA_EXATA_CONJ_APROX", "QUADRA_APROX_CONJ_APROX", "ID_EXT", "QUADRA_NOMINAL",
  "CHACARA", "RUA_AVENIDA", "NOMINAL_BIGRAMA", "QUADRA_CONJ_EXATO", "QUADRA_EXATA",
  "RUA_IGNORADO", "ID_RARO", "ID_NORM", "JARO_TOKENS", "CENTROIDE_RA",
  "INSTITUCIONAL_FIXO", "CENTROIDE_DF"
)

ROTULOS_CENTROIDE <- c(
  "CENTROIDE_MUN" = "Centroide do município", "CENTROIDE_UF" = "Centroide da UF",
  "CENTROIDE_RA"  = "Centroide da RA",        "CENTROIDE_DF" = "Centroide do DF"
)

# --------------------------------------------------------------------------
# AUXILIARES
# --------------------------------------------------------------------------

fmt_n   <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, digitos = 1) {
  paste0(formatC(x, format = "f", digits = digitos, decimal.mark = ","), "%")
}

# Códigos IBGE (município, setor) chegam do DuckDB como texto, número ou
# integer64 (BIGINT). integer64 sem o namespace do bit64 carregado vira lixo em
# as.character() e escapa do is.na() -- por isso o namespace é carregado antes
# de converter.
codigo_como_texto <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "integer64")) {
    requireNamespace("bit64", quietly = TRUE)
    x <- as.character(x)
  } else if (is.numeric(x)) {
    x <- ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = 0))
  } else {
    x <- as.character(x)
  }
  x <- trimws(x)
  x[!is.na(x) & (x == "" | x == "NA")] <- NA_character_
  x
}

# Contagem em faixas fixas, para valores com cauda longa (distância numérica,
# casos por setor): um histograma com bins uniformes esconderia tudo na
# primeira barra. A coluna de faixa sai como fator na ordem de `rotulos`, com
# as faixas vazias incluídas -- sem o fator, o gráfico ordenava as faixas em
# ordem alfabética ("> 500" antes de "0").
contar_faixas <- function(x, cortes, rotulos, nome) {
  t <- table(factor(rotulos[findInterval(x, cortes)], levels = rotulos))
  d <- data.frame(factor(names(t), levels = rotulos), as.integer(t))
  names(d) <- c(nome, "N")
  d
}

# --------------------------------------------------------------------------
# PREPARAÇÃO
# --------------------------------------------------------------------------

# base: resultados$base_processada, antes do descarte de colunas derivadas.
# eh_df: pipeline do DF (FASE_MATCH, Regiao_adm, CD_SETOR) ou das demais UFs
#   (FASE, MUNIRESAT, COD_SETOR, DISTANCIA_NUM, CD_MUN_CNEFE, similarity).
# limiar: limite de similaridade usado NESTE processamento (não o valor atual
#   do controle deslizante, que o usuário pode ter mexido depois).
# nomes_unidade: vetor nomeado código(6 dígitos) -> nome do município, para o
#   gráfico por município; sem ele, mostra o código.
preparar_graficos <- function(base, eh_df, limiar = NA, nomes_unidade = NULL) {
  n <- nrow(base)
  col_fase  <- if (eh_df) "FASE_MATCH" else "FASE"
  col_setor <- if (eh_df) "CD_SETOR" else "COD_SETOR"
  fase <- as.character(base[[col_fase]])

  precisao <- if ("PRECISAO_POSICIONAL" %in% names(base)) {
    factor(base$PRECISAO_POSICIONAL, levels = if (eh_df) NIVEIS_PRECISAO_DF else NIVEIS_PRECISAO_UF)
  } else {
    classificar_precisao(fase, base$DISTANCIA_NUM, base$LATITUDE, eh_df)
  }
  grupo <- grupo_precisao(precisao)

  # --- 1. indicadores ---
  setor <- codigo_como_texto(base[[col_setor]])
  setor[grupo != "Endereço"] <- NA          # centroide não tem setor
  kpi <- list(
    n              = n,
    n_endereco     = sum(grupo == "Endereço"),
    n_centroide    = sum(grupo == "Centroide"),
    n_sem          = sum(grupo == "Sem coordenada"),
    n_setores      = length(unique(setor[!is.na(setor)])),
    n_com_setor    = sum(!is.na(setor))
  )

  # --- 2. precisão posicional ---
  tab_prec <- as.data.frame(table(PRECISAO = precisao), stringsAsFactors = FALSE)
  tab_prec <- tab_prec[tab_prec$Freq > 0, ]
  names(tab_prec)[2] <- "N"
  tab_prec$PCT   <- 100 * tab_prec$N / n
  tab_prec$GRUPO <- grupo_precisao(tab_prec$PRECISAO)
  tab_prec$PRECISAO <- factor(tab_prec$PRECISAO, levels = levels(precisao))

  # --- 3. cobertura acumulada na ordem de execução ---
  ordem <- if (eh_df) ORDEM_ROTULOS_DF else ORDEM_FASES_UF
  cont  <- table(fase[!is.na(fase) & !is.na(base$LATITUDE)])
  etapas <- c(intersect(ordem, names(cont)), setdiff(names(cont), ordem))
  cascata <- data.frame(ETAPA = etapas, N = as.integer(cont[etapas]), stringsAsFactors = FALSE)
  cascata$INCREMENTO_PCT <- 100 * cascata$N / n
  cascata$ACUM_PCT       <- cumsum(cascata$INCREMENTO_PCT)
  cascata$GRUPO   <- ifelse(cascata$ETAPA %in% names(ROTULOS_CENTROIDE), "Centroide", "Endereço")
  cascata$ROTULO  <- ifelse(cascata$ETAPA %in% names(ROTULOS_CENTROIDE),
                            ROTULOS_CENTROIDE[cascata$ETAPA], sub("^FASE ", "", cascata$ETAPA))

  # --- 4. por município/RA informado ---
  unidade <- if (eh_df) {
    as.character(base$Regiao_adm)
  } else {
    # Só códigos numéricos são cortados para 6 dígitos -- "Ignorado" fica
    # como está (sem o teste, virava "Ignora").
    cod <- codigo_como_texto(base$MUNIRESAT)
    cod <- ifelse(grepl("^[0-9]{6,7}$", cod), substr(cod, 1, 6), cod)
    nome <- if (!is.null(nomes_unidade)) unname(nomes_unidade[cod]) else rep(NA_character_, n)
    ifelse(!is.na(nome), nome, ifelse(is.na(cod), "Não informado", cod))
  }
  unidade[is.na(unidade) | unidade == ""] <- "Não informado"
  totais <- sort(table(unidade), decreasing = TRUE)
  top <- names(totais)[seq_len(min(15, length(totais)))]
  por_unidade <- as.data.frame(table(UNIDADE = unidade, GRUPO = grupo), stringsAsFactors = FALSE)
  por_unidade <- por_unidade[por_unidade$UNIDADE %in% top, ]
  names(por_unidade)[3] <- "N"
  por_unidade$TOTAL <- as.integer(totais[por_unidade$UNIDADE])
  por_unidade$PCT   <- 100 * por_unidade$N / por_unidade$TOTAL
  por_unidade$UNIDADE_ROTULO <- paste0(por_unidade$UNIDADE, " (", fmt_n(por_unidade$TOTAL), ")")
  por_unidade$UNIDADE_ROTULO <- factor(
    por_unidade$UNIDADE_ROTULO,
    levels = rev(paste0(top, " (", fmt_n(as.integer(totais[top])), ")"))
  )
  por_unidade$GRUPO <- factor(por_unidade$GRUPO, levels = names(CORES_GRUPO))
  n_unidades <- length(totais)

  # --- 5 a 8: só nas demais UFs ---
  distancia <- divergencia <- similaridade <- NULL
  if (!eh_df) {
    sub <- sub("^FASE [A-Z]\\.", "", fase)

    # 5. distância do número nos matches por número mais próximo
    aprox <- sub %in% c("2.1", "3.2", "4.2")
    d <- suppressWarnings(as.numeric(base$DISTANCIA_NUM[aprox]))
    d <- d[!is.na(d)]
    if (length(d) > 0) {
      distancia <- list(
        tabela = contar_faixas(d, c(0, 1, 11, 51, 101, 501),
                               c("0", "1–10", "11–50", "51–100", "101–500", "> 500"), "FAIXA"),
        mediana = stats::median(d), n = length(d), n_exato = sum(d == 0)
      )
    }

    # 6. município do CNEFE diferente do informado, entre os casos com match
    #    por endereço e município informado válido (6 dígitos)
    mun_inf <- substr(codigo_como_texto(base$MUNIRESAT), 1, 6)
    mun_cn  <- substr(codigo_como_texto(base$CD_MUN_CNEFE), 1, 6)
    avaliavel <- grupo == "Endereço" & grepl("^[0-9]{6}$", mun_inf) & !is.na(mun_cn)
    diverge <- avaliavel & mun_inf != mun_cn
    divergencia <- list(
      n_avaliados   = sum(avaliavel),
      n_divergentes = sum(diverge),
      por_fase = if (any(diverge)) {
        t <- as.data.frame(table(FASE = sub("^FASE ", "", fase[diverge])), stringsAsFactors = FALSE)
        names(t)[2] <- "N"
        t[order(-t$N), ]
      } else NULL
    )

    # 8. similaridade só onde ela decide o match (fases 4 e 5)
    prob <- grepl("^(4\\.|5$)", sub) & !is.na(base$LATITUDE)
    s <- suppressWarnings(as.numeric(base$similarity[prob]))
    s <- s[!is.na(s)]
    if (length(s) > 0) {
      similaridade <- list(valores = s, limiar = limiar, n = length(s),
                           n_perto = if (is.na(limiar)) NA else sum(s < limiar + 0.05))
    }
  }

  # --- 7. casos por setor censitário ---
  setores <- NULL
  if (kpi$n_setores > 0) {
    por_setor <- sort(as.integer(table(setor[!is.na(setor)])), decreasing = TRUE)
    k <- ceiling(0.1 * length(por_setor))
    setores <- list(
      tabela = contar_faixas(por_setor, c(1, 2, 3, 6, 11, 21),
                             c("1", "2", "3–5", "6–10", "11–20", "> 20"), "CASOS"),
      n_setores = length(por_setor),
      pct_top10 = 100 * sum(por_setor[seq_len(k)]) / sum(por_setor),
      max_casos = por_setor[1]
    )
    names(setores$tabela)[2] <- "N_SETORES"
  }

  list(eh_df = eh_df, kpi = kpi, precisao = tab_prec, cascata = cascata,
       por_unidade = por_unidade, n_unidades = n_unidades, distancia = distancia,
       divergencia = divergencia, setores = setores, similaridade = similaridade)
}

# --------------------------------------------------------------------------
# GRÁFICOS (ggplot2)
# --------------------------------------------------------------------------

tema_graficos <- function(grade = c("x", "y")) {
  grade <- match.arg(grade)
  t <- ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      text               = ggplot2::element_text(color = TINTA_PRIMARIA),
      plot.title         = ggplot2::element_blank(),
      plot.subtitle      = ggplot2::element_text(color = TINTA_SECUNDARIA, size = 12,
                                                 margin = ggplot2::margin(b = 10)),
      plot.title.position = "plot",
      axis.text          = ggplot2::element_text(color = TINTA_SECUNDARIA),
      axis.title         = ggplot2::element_text(color = TINTA_MUTED, size = 11),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major   = ggplot2::element_line(color = COR_GRADE, linewidth = 0.3),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(color = TINTA_SECUNDARIA),
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin        = ggplot2::margin(6, 16, 6, 6)
    )
  # Barras horizontais: só a grade vertical; colunas: só a horizontal.
  if (grade == "x") t + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  else              t + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

# Vão branco de ~2px entre barras e segmentos (em vez de borda escura).
VAO <- 0.7

grafico_precisao <- function(g) {
  d <- g$precisao
  d$PRECISAO <- factor(d$PRECISAO, levels = rev(levels(droplevels(d$PRECISAO))))
  d$ROTULO <- paste0(fmt_n(d$N), " (", fmt_pct(d$PCT), ")")
  ggplot2::ggplot(d, ggplot2::aes(x = PCT, y = PRECISAO, fill = GRUPO)) +
    ggplot2::geom_col(width = 0.72, color = "white", linewidth = VAO) +
    ggplot2::geom_text(ggplot2::aes(label = ROTULO), hjust = -0.08, size = 3.8,
                       color = TINTA_SECUNDARIA) +
    ggplot2::scale_fill_manual(values = CORES_GRUPO, breaks = intersect(names(CORES_GRUPO), d$GRUPO)) +
    ggplot2::scale_x_continuous(labels = function(x) fmt_pct(x, 0), limits = c(0, max(d$PCT) * 1.28),
                                expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::labs(x = "% dos casos", y = NULL,
                  subtitle = paste0(fmt_pct(100 * g$kpi$n_endereco / g$kpi$n), " dos casos com posição de endereço")) +
    tema_graficos("x")
}

grafico_cascata <- function(g) {
  d <- g$cascata
  d$ROTULO <- factor(d$ROTULO, levels = rev(d$ROTULO))
  # Toda barra leva o quanto a etapa acrescentou. Rotular só as maiores (>= 1%)
  # deixava as demais parecendo sem dado.
  d$TEXTO <- ifelse(d$INCREMENTO_PCT < 0.05, "+<0,1%", paste0("+", fmt_pct(d$INCREMENTO_PCT)))
  final <- utils::tail(d$ACUM_PCT, 1)
  ggplot2::ggplot(d, ggplot2::aes(x = ACUM_PCT, y = ROTULO, fill = GRUPO)) +
    ggplot2::geom_col(width = 0.72, color = "white", linewidth = VAO) +
    ggplot2::geom_text(ggplot2::aes(label = TEXTO), hjust = -0.1, size = 3.5, color = TINTA_SECUNDARIA) +
    ggplot2::scale_fill_manual(values = CORES_GRUPO, breaks = intersect(names(CORES_GRUPO), d$GRUPO)) +
    ggplot2::scale_x_continuous(labels = function(x) fmt_pct(x, 0), limits = c(0, 112),
                                breaks = seq(0, 100, 25), expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::labs(x = "% dos casos com coordenada até a etapa", y = NULL,
                  subtitle = paste0("Ao fim da cascata, ", fmt_pct(final),
                                    " dos casos têm coordenada. Rótulo: quanto cada etapa acrescenta.")) +
    tema_graficos("x")
}

grafico_por_unidade <- function(g) {
  d <- g$por_unidade
  ggplot2::ggplot(d, ggplot2::aes(x = PCT, y = UNIDADE_ROTULO, fill = GRUPO)) +
    ggplot2::geom_col(width = 0.72, color = "white", linewidth = VAO,
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = CORES_GRUPO, drop = TRUE,
                               breaks = intersect(names(CORES_GRUPO), as.character(d$GRUPO[d$N > 0]))) +
    ggplot2::scale_x_continuous(labels = function(x) fmt_pct(x, 0), breaks = seq(0, 100, 25),
                                expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::labs(x = "% dos casos da unidade", y = NULL,
                  subtitle = paste0(if (g$n_unidades > 15) "As 15 unidades com mais casos, de " else "",
                                    if (g$n_unidades > 15) fmt_n(g$n_unidades) else "",
                                    if (g$n_unidades > 15) ". " else "",
                                    "Entre parênteses, o total de casos.")) +
    tema_graficos("x")
}

grafico_distancia <- function(g) {
  d <- g$distancia
  ggplot2::ggplot(d$tabela, ggplot2::aes(x = FAIXA, y = N)) +
    ggplot2::geom_col(width = 0.72, fill = COR_ENDERECO, color = "white", linewidth = VAO) +
    ggplot2::scale_y_continuous(labels = fmt_n, expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = "Diferença entre o número informado e o encontrado (em números de porta)",
                  y = "Casos",
                  subtitle = paste0(fmt_n(d$n), " casos resolvidos pelo número mais próximo; mediana de ",
                                    fmt_n(d$mediana), " números de distância")) +
    tema_graficos("y")
}

grafico_divergencia <- function(g) {
  d <- g$divergencia$por_fase
  d$FASE <- factor(d$FASE, levels = rev(d$FASE))
  ggplot2::ggplot(d, ggplot2::aes(x = N, y = FASE)) +
    ggplot2::geom_col(width = 0.72, fill = COR_ENDERECO, color = "white", linewidth = VAO) +
    ggplot2::scale_x_continuous(labels = fmt_n, expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Casos com município divergente", y = NULL,
                  subtitle = "Em que fase de match a divergência aconteceu") +
    tema_graficos("x")
}

grafico_setores <- function(g) {
  d <- g$setores
  ggplot2::ggplot(d$tabela, ggplot2::aes(x = CASOS, y = N_SETORES)) +
    ggplot2::geom_col(width = 0.72, fill = COR_ENDERECO, color = "white", linewidth = VAO) +
    ggplot2::scale_y_continuous(labels = fmt_n, expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = "Casos no setor", y = "Setores",
                  subtitle = paste0("Os 10% de setores com mais casos concentram ", fmt_pct(d$pct_top10),
                                    " dos casos com setor")) +
    tema_graficos("y")
}

grafico_similaridade <- function(g) {
  d <- g$similaridade
  df <- data.frame(SIM = d$valores)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = SIM)) +
    ggplot2::geom_histogram(binwidth = 0.02, boundary = 0, fill = COR_ENDERECO,
                            color = "white", linewidth = VAO) +
    ggplot2::scale_y_continuous(labels = fmt_n, expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::scale_x_continuous(labels = function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")) +
    ggplot2::labs(x = "Similaridade (Jaro-Winkler)", y = "Casos",
                  subtitle = if (is.na(d$limiar)) paste0(fmt_n(d$n), " casos nas fases 4 e 5") else
                    paste0(fmt_n(d$n), " casos nas fases 4 e 5; ", fmt_n(d$n_perto), " (",
                           fmt_pct(100 * d$n_perto / d$n), ") a menos de 0,05 acima do limite")) +
    tema_graficos("y")
  if (!is.na(d$limiar)) {
    p <- p +
      ggplot2::geom_vline(xintercept = d$limiar, color = TINTA_PRIMARIA, linewidth = 0.5) +
      ggplot2::annotate("text", x = d$limiar, y = Inf, vjust = 1.4, hjust = -0.08, size = 3.6,
                        color = TINTA_PRIMARIA,
                        label = paste0("limite = ", formatC(d$limiar, format = "f", digits = 2, decimal.mark = ",")))
  }
  p
}
