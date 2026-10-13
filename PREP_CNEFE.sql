-- ================================================================
-- PREP_CNEFE.sql
-- Pré-processamento ESTÁTICO do CNEFE — roda 1x (não a cada lote/chamada).
--
-- Todas as tabelas aqui dependem apenas de CNEFEDF e de listas fixas
-- (identificadores, ID_SECUNDARIOS, stopwords, etc.) — nunca dos dados
-- do SINAN. Materializá-las como tabelas persistentes elimina o
-- recálculo repetido (tokenização + UNNEST do CNEFE inteiro do DF)
-- em toda chamada de georreferenciar_DF().
--
-- Execute via PREP_CNEFE.R sempre que CNEFEDF for atualizado (dado
-- censitário, raramente muda). Em uso normal do pipeline, NÃO precisa
-- rodar de novo.
-- ================================================================

-- ----------------------------------------------------------------
-- [0] LOOKUP TABLES (listas fixas, idênticas às do pipeline original)
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE identificadores AS
SELECT * FROM (VALUES
    ('SQN'), ('SQS'), ('CLN'), ('CLS'),
    ('SCRN'), ('CRS'),
    ('SHCGN'), ('SHCGS'),
    ('SEPN'), ('SEPS'),
    ('SGAN'), ('SGAS'),
    ('SCN'), ('SCS'),
    ('SHIS'), ('SHIN'),
    ('QI'), ('QL'),
    ('SMDB'), ('ML'),
    ('QRSW'), ('CLSW'), ('EQSW'),
    ('SQNW'), ('CLNW'), ('EQNW'),
    ('AOS'),
    ('QNA'), ('QNB'), ('QNC'), ('QND'),
    ('QNE'), ('QNF'), ('QNG'), ('QNH'),
    ('QNJ'), ('QNL'),
    ('QNM'), ('QNN'), ('QNO'),
    ('QNP'), ('QNQ'), ('QNR'),
    ('CNN'), ('CNM'),
    ('QE'), ('QN'), ('QR'), ('QS'),
    ('SMPW'),
    ('SHCES'), ('SHCE')
) AS t(id);

CREATE OR REPLACE TABLE ID_SECUNDARIOS AS
SELECT * FROM (VALUES
    ('CONJUNTO'), ('CONJ'), ('CJ'),
    ('BLOCO'),    ('BL'),
    ('VIA'),
    ('LOTE'),     ('LT'),
    ('APTO'),     ('AP'),
    ('MODULO'),   ('MOD'),
    ('ETAPA'),    ('ET'),
    ('TRECHO'),   ('TR'),
    ('SETOR'),    ('ST'),
    ('CHACARA'),  ('CH'),
    ('QUADRA'),   ('QD')
) AS t(id);

CREATE OR REPLACE TABLE stopwords AS
SELECT * FROM (VALUES
    ('DE'), ('DA'), ('DO'), ('DAS'), ('DOS'),
    ('E'), ('A'), ('O'), ('EM'), ('NO'), ('NA'),
    ('COM'), ('PARA'), ('POR'), ('AO'), ('SE'),
    ('RUA'), ('AVENIDA'), ('AV'), ('R')
) AS t(sw);

CREATE OR REPLACE TABLE identificadores_ext AS
SELECT id FROM identificadores
UNION
SELECT * FROM (VALUES
    ('SHSN'), ('SHPS'), ('QSB'), ('CNB'), ('SHTB'), ('AQD'),
    ('CL'), ('CSB'), ('CR'), ('SQ'), ('SHTN'), ('SHTS'),
    ('SHCN'), ('SHCS'), ('SHTC'), ('CNB'), ('CNA'),
    ('EQS'), ('EQN'), ('EQNP'), ('EQSP'),
    ('SDS'), ('SDN'), ('SRES'), ('SREN'),
    ('SRTS'), ('SRTN'), ('SCTS'), ('SCTN'),
    ('SMS'), ('SMN'), ('SAIS'), ('SAIN'),
    ('SGCV'), ('SGOV'), ('SGOW'),
    ('SHOB'), ('SHOT'), ('SHON'),
    ('CNF'), ('CNG'), ('CNH'),
    ('SHTB'), ('SHTN')
) AS t(id);

CREATE OR REPLACE TABLE identificadores_raros AS
SELECT * FROM (VALUES
    ('QSD'), ('QSC'), ('QMS'), ('SQNW'), ('CCSW'),
    ('SCLRN'), ('SCLN'), ('ADE'), ('SRL'), ('QRB'),
    ('ANL'), ('SHR'), ('QC'), ('SMA'), ('EQNP'),
    ('CSB'), ('CSE'), ('QCS'), ('QSE'), ('SHCA'),
    ('SHPS'), ('SHOB'), ('CR'), ('EQ'), ('CAVP'),
    ('AR'), ('SQ'), ('QNR')
) AS t(id);

-- ----------------------------------------------------------------
-- [1] CNEFE_AUX — normalização + tokenização base
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_AUX AS

WITH base AS (
    SELECT
        c.*,
        UPPER(COALESCE(c.NOM_SEGLOGR, '')) AS LOGRADOURO_RAW
    FROM CNEFEDF c
),

normalizado AS (
    SELECT *,
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            LOGRADOURO_RAW,
        '\bCONJ\b', 'CONJUNTO', 'g'),
        '\bBL\b',   'BLOCO',    'g') AS LOGRADOURO
    FROM base
),

tokenized AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(LOGRADOURO, '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS LOGRADOURO_TOKENS
    FROM normalizado
)

SELECT * FROM tokenized;

SELECT COUNT(*) AS n_cnefe FROM CNEFE_AUX;

-- ----------------------------------------------------------------
-- [2] CNEFE_TOKENS_POS — explode com posição de token
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_TOKENS_POS AS
SELECT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    tok.token,
    tok.pos,
    c.LOGRADOURO_TOKENS[tok.pos + 1] AS token_next
FROM CNEFE_AUX c,
     UNNEST(c.LOGRADOURO_TOKENS) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- ----------------------------------------------------------------
-- [3] CNEFE_BIGRAMAS — bigramas com IDs primários/secundários
-- (a recriação duplicada que existia no pipeline original foi
-- removida; era idêntica a esta, pois ID_SECUNDARIOS nunca mudava
-- entre as duas execuções)
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_BIGRAMAS AS
SELECT DISTINCT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    c.token       AS bigrama_chave,
    c.token_next  AS bigrama_valor
FROM CNEFE_TOKENS_POS c
WHERE (
    c.token IN (SELECT id FROM identificadores)
    OR c.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND c.token_next IS NOT NULL
AND c.token_next != '';

-- ----------------------------------------------------------------
-- [4] CNEFE_F8_CANDIDATOS
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_F8_CANDIDATOS AS
SELECT DISTINCT
    cb_id.bigrama_chave  AS ID_PRIMARIO,
    cb_id.bigrama_valor  AS NUM_QUADRA_CNEFE_STR,
    CASE
        WHEN cb_id.bigrama_valor ~ '^[0-9]+$'
        THEN CAST(cb_id.bigrama_valor AS INTEGER)
        ELSE NULL
    END                  AS NUM_QUADRA_CNEFE,
    cb_id.ENDERECO_CNEFE,
    cb_id.VAL_COMP_ELEM1,
    cb_id.LATITUDE,
    cb_id.LONGITUDE,
    cb_id.NM_SUBDIST,
    cb_conj.bigrama_valor AS CONJ_CNEFE
FROM CNEFE_BIGRAMAS cb_id
LEFT JOIN CNEFE_BIGRAMAS cb_conj
    ON  cb_conj.ENDERECO_CNEFE = cb_id.ENDERECO_CNEFE
    AND cb_conj.bigrama_chave  = 'CONJUNTO'
WHERE cb_id.bigrama_chave IN (SELECT id FROM identificadores)
  AND cb_id.bigrama_valor ~ '^[0-9]+$';

-- ----------------------------------------------------------------
-- [5] CNEFE_BIGRAMAS_EXT — lista estendida de identificadores
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_BIGRAMAS_EXT AS
SELECT DISTINCT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    tok.token      AS bigrama_chave,
    c.LOGRADOURO_TOKENS[tok.pos + 1] AS bigrama_valor
FROM CNEFE_AUX c,
     UNNEST(c.LOGRADOURO_TOKENS) WITH ORDINALITY AS tok(token, pos)
WHERE (
    tok.token IN (SELECT id FROM identificadores_ext)
    OR tok.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND c.LOGRADOURO_TOKENS[tok.pos + 1] IS NOT NULL
AND c.LOGRADOURO_TOKENS[tok.pos + 1] != ''
AND tok.token != '';

-- ----------------------------------------------------------------
-- [6] CNEFE_BIGRAMAS_QUADRA
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_BIGRAMAS_QUADRA AS
SELECT DISTINCT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    tok.token      AS bigrama_chave,
    c.LOGRADOURO_TOKENS[tok.pos + 1] AS bigrama_valor
FROM CNEFE_AUX c,
     UNNEST(c.LOGRADOURO_TOKENS) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token IN ('QUADRA', 'CONJUNTO', 'BLOCO', 'LOTE', 'CHACARA', 'RUA', 'VIA')
  AND c.LOGRADOURO_TOKENS[tok.pos + 1] IS NOT NULL
  AND c.LOGRADOURO_TOKENS[tok.pos + 1] != ''
  AND tok.token != '';

-- ----------------------------------------------------------------
-- [7] CNEFE_BIGRAMAS_CHACARA
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_BIGRAMAS_CHACARA AS
SELECT DISTINCT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    tok.token      AS bigrama_chave,
    c.LOGRADOURO_TOKENS[tok.pos + 1] AS bigrama_valor
FROM CNEFE_AUX c,
     UNNEST(c.LOGRADOURO_TOKENS) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token IN ('CHACARA', 'LOTE', 'CONJUNTO', 'BLOCO')
  AND c.LOGRADOURO_TOKENS[tok.pos + 1] IS NOT NULL
  AND c.LOGRADOURO_TOKENS[tok.pos + 1] != ''
  AND tok.token != '';

-- ----------------------------------------------------------------
-- [8] CNEFE_F12_TOKENS / CNEFE_F12_TOKEN_TOTAL — RUA/AVENIDA, sem stopwords
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_F12_TOKENS AS
SELECT DISTINCT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    tok.token
FROM CNEFE_AUX c,
     UNNEST(c.LOGRADOURO_TOKENS) AS tok(token)
WHERE (c.LOGRADOURO LIKE 'RUA%' OR c.LOGRADOURO LIKE 'AVENIDA%')
  AND tok.token != ''
  AND tok.token NOT IN (SELECT sw FROM stopwords);

CREATE OR REPLACE TABLE CNEFE_F12_TOKEN_TOTAL AS
SELECT ENDERECO_CNEFE, COUNT(*) AS TOKENS_CNEFE_TOTAL
FROM CNEFE_F12_TOKENS
GROUP BY ENDERECO_CNEFE;

-- ----------------------------------------------------------------
-- [9] CNEFE_F13_BIGRAMAS / CNEFE_F13_TOK_JAC / CNEFE_F13_TOK_JAC_TOTAL
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_F13_BIGRAMAS AS
SELECT DISTINCT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    tok.token      AS bigrama_chave,
    c.LOGRADOURO_TOKENS[tok.pos + 1] AS bigrama_valor
FROM CNEFE_AUX c,
     UNNEST(c.LOGRADOURO_TOKENS) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token IN ('QUADRA','CONJUNTO','BLOCO','LOTE','CHACARA','RUA','AVENIDA','VIA','TRECHO')
  AND c.LOGRADOURO_TOKENS[tok.pos + 1] IS NOT NULL
  AND c.LOGRADOURO_TOKENS[tok.pos + 1] != ''
  AND tok.token != '';

CREATE OR REPLACE TABLE CNEFE_F13_TOK_JAC AS
SELECT DISTINCT
    c.ENDERECO_CNEFE,
    c.NM_SUBDIST,
    tok.token
FROM CNEFE_AUX c,
     UNNEST(c.LOGRADOURO_TOKENS) AS tok(token)
WHERE tok.token != ''
  AND tok.token NOT IN (SELECT sw FROM stopwords)
  AND LENGTH(tok.token) > 1;

CREATE OR REPLACE TABLE CNEFE_F13_TOK_JAC_TOTAL AS
SELECT ENDERECO_CNEFE, COUNT(*) AS N FROM CNEFE_F13_TOK_JAC GROUP BY ENDERECO_CNEFE;

-- ----------------------------------------------------------------
-- [10] CNEFE_F14_QUADRA — candidatos QUADRA (filtro de RA aplicado
-- no JOIN do MATCH_FASE14, não aqui)
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_F14_QUADRA AS
SELECT DISTINCT
    cb.ENDERECO_CNEFE,
    cb.VAL_COMP_ELEM1,
    cb.LATITUDE,
    cb.LONGITUDE,
    cb.NM_SUBDIST,
    CAST(cb.bigrama_valor AS INTEGER) AS NUM_QUADRA_CNEFE,
    cb_conj.bigrama_valor             AS CONJ_CNEFE
FROM CNEFE_BIGRAMAS_QUADRA cb
LEFT JOIN CNEFE_BIGRAMAS_QUADRA cb_conj
    ON  cb_conj.ENDERECO_CNEFE = cb.ENDERECO_CNEFE
    AND cb_conj.bigrama_chave  = 'CONJUNTO'
WHERE cb.bigrama_chave = 'QUADRA'
  AND cb.bigrama_valor ~ '^[0-9]+$';

-- ----------------------------------------------------------------
-- [11] CNEFE_BIGRAMAS_RAROS
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_BIGRAMAS_RAROS AS
SELECT DISTINCT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    tok.token      AS bigrama_chave,
    c.LOGRADOURO_TOKENS[tok.pos + 1] AS bigrama_valor
FROM CNEFE_AUX c,
     UNNEST(c.LOGRADOURO_TOKENS) WITH ORDINALITY AS tok(token, pos)
WHERE (
    tok.token IN (SELECT id FROM identificadores_raros)
    OR tok.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND c.LOGRADOURO_TOKENS[tok.pos + 1] IS NOT NULL
AND c.LOGRADOURO_TOKENS[tok.pos + 1] != ''
AND tok.token != '';

-- ----------------------------------------------------------------
-- [12] CNEFE_F18_TOKENS — 4 primeiros tokens do logradouro
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CNEFE_F18_TOKENS AS
SELECT
    c.ENDERECO_CNEFE,
    c.VAL_COMP_ELEM1,
    c.LATITUDE,
    c.LONGITUDE,
    c.NM_SUBDIST,
    COALESCE(c.LOGRADOURO_TOKENS[1], '') AS tok1,
    COALESCE(c.LOGRADOURO_TOKENS[2], '') AS tok2,
    COALESCE(c.LOGRADOURO_TOKENS[3], '') AS tok3,
    COALESCE(c.LOGRADOURO_TOKENS[4], '') AS tok4,
    COALESCE(
        CASE WHEN c.LOGRADOURO_TOKENS[1] ~ '^[0-9]+$' THEN c.LOGRADOURO_TOKENS[1] END,
        CASE WHEN c.LOGRADOURO_TOKENS[2] ~ '^[0-9]+$' THEN c.LOGRADOURO_TOKENS[2] END,
        CASE WHEN c.LOGRADOURO_TOKENS[3] ~ '^[0-9]+$' THEN c.LOGRADOURO_TOKENS[3] END,
        CASE WHEN c.LOGRADOURO_TOKENS[4] ~ '^[0-9]+$' THEN c.LOGRADOURO_TOKENS[4] END
    ) AS TOKEN_NUMERICO
FROM CNEFE_AUX c;

-- ----------------------------------------------------------------
-- [13] CENTROIDE_RA — centroides por RA (fallback final, também estático)
-- ----------------------------------------------------------------

CREATE OR REPLACE TABLE CENTROIDE_RA AS
SELECT
    NM_SUBDIST,
    AVG(LATITUDE)  AS LAT_CENTROIDE,
    AVG(LONGITUDE) AS LON_CENTROIDE,
    COUNT(*)       AS N_ENDERECOS
FROM CNEFEDF
WHERE LATITUDE  IS NOT NULL
  AND LONGITUDE IS NOT NULL
  AND NM_SUBDIST IS NOT NULL
  AND NM_SUBDIST != ''
GROUP BY NM_SUBDIST;

SELECT 'PREP_CNEFE concluído' AS status;
