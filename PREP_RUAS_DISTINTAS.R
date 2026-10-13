# ==================================================================================
# PREP_RUAS_DISTINTAS.R
#
# Cria (ou recria) as tabelas auxiliares "<TABELA_CNEFE>_RUAS" -- ruas
# distintas por município/RA, usadas pelas FASES 4A/4B do georreferenciamento
# (app.R) para não precisar escanear a CNEFE inteira toda vez que um
# registro precisa descobrir quais ruas existem no seu município.
#
# Sem essa tabela pré-computada, o app.R monta uma versão TEMPORÁRIA
# equivalente na hora, uma vez por execução -- já ajuda bastante, mas em
# municípios muito grandes (ex: São Paulo capital, ~5,5 milhões de
# endereços) esse escaneamento ainda pesa. Rodando este script uma vez, o
# app.R passa a usar a versão persistente direto, sem escanear nada.
#
# QUANDO RODAR:
#   - Uma vez, agora.
#   - De novo, toda vez que o CNEFE.duckdb for atualizado (novos arquivos
#     CNEFE importados, municípios adicionados etc.) -- a tabela auxiliar
#     não se atualiza sozinha.
#
# AJUSTE ANTES DE RODAR:
#   - DUCKDB_PATH: caminho completo do CNEFE.duckdb de produção.
# ==================================================================================

library(DBI)
library(duckdb)

# --------------------------------------------------------------------------
# AJUSTE AQUI
# --------------------------------------------------------------------------
DUCKDB_PATH <- "CNEFE.duckdb"   # <-- caminho do .duckdb de producao (padrao: na pasta do projeto)

prep_ruas_distintas <- function() {

  cat(sprintf("[PREP_RUAS_DISTINTAS] Conectando a %s (leitura/escrita, uso único)...\n", DUCKDB_PATH))
  con <- dbConnect(duckdb(), dbdir = DUCKDB_PATH, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(con, "SET memory_limit = '4GB'")
  dbExecute(con, "SET threads = 2")

  tabelas <- dbGetQuery(con, "SHOW TABLES")$name
  # Tabelas CNEFE por UF: "cnefePE", "cnefeTO", "cnefeSP" etc. (2 letras no
  # final). Não confundir com as tabelas "_RUAS" já existentes (essas têm
  # mais de 2 caracteres depois de "cnefe") nem com outras tabelas auxiliares
  # do pipeline (tempA, tempB, CNEFE_TOKENS_POS etc.).
  tabelas_cnefe <- tabelas[grepl("^cnefe[A-Za-z]{2}$", tabelas, ignore.case = TRUE)]

  if (length(tabelas_cnefe) == 0) {
    stop("Nenhuma tabela CNEFE<UF> encontrada (esperado algo como cnefePE, cnefeTO, cnefeSP...).")
  }

  cat(sprintf("[PREP_RUAS_DISTINTAS] Tabelas CNEFE encontradas: %s\n\n", paste(tabelas_cnefe, collapse = ", ")))

  for (tab in tabelas_cnefe) {
    tabela_ruas <- paste0(tab, "_RUAS")

    colunas <- dbGetQuery(con, sprintf("DESCRIBE %s", tab))$column_name
    # Não-DF filtra por CD_MUN (código IBGE do município); DF filtra por
    # NM_SUBDIST (nome da Região Administrativa) -- mesma coluna usada como
    # "meio_filtro_mun_ra" dentro de georreferenciar_UF() no app.R.
    coluna_mun <- if ("CD_MUN" %in% colunas) {
      "CD_MUN"
    } else if ("NM_SUBDIST" %in% colunas) {
      "NM_SUBDIST"
    } else {
      NA
    }

    if (is.na(coluna_mun)) {
      cat(sprintf("[%s] pulado -- não encontrei CD_MUN nem NM_SUBDIST nas colunas.\n", tab))
      next
    }

    t0 <- Sys.time()
    dbExecute(con, sprintf("
      CREATE OR REPLACE TABLE %s AS
      SELECT DISTINCT %s AS MUN_FILTRO, ENDERECO_CNEFE3
      FROM %s
    ", tabela_ruas, coluna_mun, tab))
    dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)

    n <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", tabela_ruas))$n
    cat(sprintf("[%s] -> %s: %d ruas distintas (coluna: %s, %.1fs)\n", tab, tabela_ruas, n, coluna_mun, dt))
  }

  cat("\n[PREP_RUAS_DISTINTAS] Concluído. O app.R vai detectar essas tabelas\n")
  cat("[PREP_RUAS_DISTINTAS] automaticamente na próxima execução (sem precisar reiniciar nada além disso).\n")
}

prep_ruas_distintas()
