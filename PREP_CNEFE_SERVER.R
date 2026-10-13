# ==================================================================================
# PREP_CNEFE_SERVER.R
#
# Versão do PREP_CNEFE.R para rodar DIRETO no duckdb já existente no RStudio
# Server (o mesmo que já tem cnefeDF, cnefePE, cnefeTO, cnefeAM etc.) — não
# copia nem substitui nada, só ACRESCENTA as tabelas estáticas persistentes
# do pipeline novo do DF.
#
# AJUSTE ANTES DE RODAR:
#   - DUCKDB_PATH: caminho completo do .duckdb já usado em produção
#   - TABELA_CNEFEDF: nome da tabela CNEFE do DF nesse banco (DuckDB não
#     diferencia maiúsculas/minúsculas em identificadores não citados entre
#     aspas, então "cnefeDF" e "CNEFEDF" apontam pra mesma tabela)
#
# O QUE ESTE SCRIPT CRIA (21 tabelas novas, nomes genéricos):
#   identificadores, ID_SECUNDARIOS, stopwords, identificadores_ext,
#   identificadores_raros, CNEFE_AUX, CNEFE_TOKENS_POS, CNEFE_BIGRAMAS,
#   CNEFE_F8_CANDIDATOS, CNEFE_BIGRAMAS_EXT, CNEFE_BIGRAMAS_QUADRA,
#   CNEFE_BIGRAMAS_CHACARA, CNEFE_F12_TOKENS, CNEFE_F12_TOKEN_TOTAL,
#   CNEFE_F13_BIGRAMAS, CNEFE_F13_TOK_JAC, CNEFE_F13_TOK_JAC_TOTAL,
#   CNEFE_F14_QUADRA, CNEFE_BIGRAMAS_RAROS, CNEFE_F18_TOKENS, CENTROIDE_RA
#
# ⚠️ Esses nomes são genéricos (não prefixados por UF). Se cnefePE/cnefeTO/
# cnefeAM também ganharem uma otimização parecida no futuro, será preciso
# prefixar (ex: CNEFE_AUX_DF) para não colidir. Por enquanto, só o DF usa
# esse conjunto de tabelas.
#
# QUANDO RODAR:
#   - Uma vez, antes do primeiro uso do georreferenciar_DF_batches() em produção.
#   - De novo, só se cnefeDF for atualizado (dado censitário, raramente muda).
# ==================================================================================

library(DBI)
library(duckdb)

# --------------------------------------------------------------------------
# AJUSTE AQUI
# --------------------------------------------------------------------------
DUCKDB_PATH     <- "CNEFE.duckdb"   # <-- caminho do .duckdb de producao (padrao: na pasta do projeto)
TABELA_CNEFEDF  <- "cnefeDF"        # <-- confirmar nome exato da tabela do DF

# --------------------------------------------------------------------------
# split_sql_rapido (mesma versão quote-aware usada no pipeline)
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

prep_cnefe_server <- function() {

  cat(sprintf("[PREP_CNEFE_SERVER] Conectando a %s (leitura/escrita, uso único)...\n", DUCKDB_PATH))
  con <- dbConnect(duckdb(), dbdir = DUCKDB_PATH, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # ------------------------------------------------------------------------
  # Checagem defensiva: confirma que a tabela e as colunas esperadas existem
  # antes de rodar qualquer coisa. Evita criar tabelas "estáticas" erradas
  # silenciosamente se o schema de produção for diferente do usado nos testes.
  # ------------------------------------------------------------------------
  tabelas <- dbGetQuery(con, "SHOW TABLES")$name
  if (!TABELA_CNEFEDF %in% tabelas) {
    stop(sprintf(
      "Tabela '%s' não encontrada em %s. Tabelas disponíveis: %s",
      TABELA_CNEFEDF, DUCKDB_PATH, paste(tabelas, collapse = ", ")
    ))
  }

  colunas_necessarias <- c(
    "NOM_SEGLOGR", "ENDERECO_CNEFE", "VAL_COMP_ELEM1",
    "LATITUDE", "LONGITUDE", "NM_SUBDIST", "CD_SUBDIST", "COD_SETOR"
  )
  colunas_existentes <- dbGetQuery(con, sprintf("DESCRIBE %s", TABELA_CNEFEDF))$column_name
  faltando <- setdiff(colunas_necessarias, colunas_existentes)

  if (length(faltando) > 0) {
    stop(sprintf(
      "Colunas ausentes em %s: %s\nColunas existentes: %s\nO pipeline (GEOCODING_DF_CORRIDO.sql) espera exatamente essas colunas — ajuste o SQL ou renomeie as colunas antes de continuar.",
      TABELA_CNEFEDF, paste(faltando, collapse = ", "), paste(colunas_existentes, collapse = ", ")
    ))
  }

  cat(sprintf("[PREP_CNEFE_SERVER] Tabela '%s' validada — todas as colunas necessárias presentes.\n", TABELA_CNEFEDF))

  # Se a tabela de origem tiver nome diferente de CNEFEDF, cria uma VIEW com
  # o nome que o SQL do pipeline espera — sem duplicar dados.
  if (toupper(TABELA_CNEFEDF) != "CNEFEDF") {
    dbExecute(con, sprintf("CREATE OR REPLACE VIEW CNEFEDF AS SELECT * FROM %s", TABELA_CNEFEDF))
    cat(sprintf("[PREP_CNEFE_SERVER] VIEW CNEFEDF criada apontando para '%s'.\n", TABELA_CNEFEDF))
  }

  dbExecute(con, "SET memory_limit = '4GB'")
  dbExecute(con, "SET threads = 2")

  sql_completo <- paste(
    readLines("PREP_CNEFE.sql", encoding = "UTF-8", warn = FALSE),
    collapse = "\n"
  )

  statements <- split_sql_rapido(sql_completo)
  statements <- statements[nchar(trimws(statements)) > 4]

  cat(sprintf("[PREP_CNEFE_SERVER] %d statements a executar\n\n", length(statements)))

  for (i in seq_along(statements)) {
    stmt <- statements[[i]]
    stmt_upper <- toupper(trimws(stmt))

    if (grepl("^SELECT", stmt_upper)) {
      res <- dbGetQuery(con, stmt)
      cat(sprintf("  [%d] %s: %s\n", i, names(res)[1], as.character(res[[1]][1])))
    } else {
      t0 <- Sys.time()
      dbExecute(con, stmt)
      dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
      tb <- regmatches(stmt_upper, regexpr("TABLE\\s+\\w+", stmt_upper))
      tb <- trimws(gsub("TABLE", "", tb))
      cat(sprintf("  [%d] Criado: %-28s (%.1fs)\n", i, tb, dt))
    }
  }

  cat("\n[PREP_CNEFE_SERVER] Concluído. As tabelas estáticas do pipeline DF estão persistidas\n")
  cat("[PREP_CNEFE_SERVER] no mesmo duckdb de produção, ao lado de cnefePE/cnefeTO/cnefeAM.\n")
  cat("[PREP_CNEFE_SERVER] georreferenciar_DF_batches() já pode ser usada normalmente.\n")
}

prep_cnefe_server()

library(DBI); library(duckdb)

# tamanho do arquivo (pra confirmar se é o banco certo, com dados reais)
file.info(DUCKDB_PATH)$size / 1e9  # em GB

con <- dbConnect(duckdb(), dbdir = DUCKDB_PATH, read_only = TRUE)

# tabelas no schema padrão
print(dbGetQuery(con, "SHOW TABLES"))

# tabelas em TODOS os schemas/catálogos (caso estejam em outro lugar)
print(dbGetQuery(con, "SHOW ALL TABLES"))

dbDisconnect(con, shutdown = TRUE)
