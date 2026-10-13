-- ================================================================
-- PIPELINE DE GEORREFERENCIAMENTO - TOKENS E BIGRAMAS
-- SINAN × CNEFE - Distrito Federal
-- ================================================================
-- SEQUÊNCIA:
--   [0]  LOOKUP TABLES  (identificadores, ID_SECUNDARIOS, stopwords)
--   [1]  SINAN          (normalização + tokenização)
--   [2]  CNEFE          (normalização + tokenização + bigramas base)
--   [3]  FASE 1         (bigramas exatos, TEM_ID=1, filtro RA)
--   [4]  FASE 2         (bigramas relaxados 75%, TEM_ID=1)
--   [5]  FASE 3         (salto de letra intermediária, TEM_ID=1)
--   [6]  FASE 4         (só LOTE sem CONJUNTO, TEM_ID=1)
--   [7]  FASE 5         (bigrama primário + bônus conjunto, TEM_ID=1)
--   [8]  FASE 6         (IGNORADO sem filtro RA, TEM_ID=1)
--   [9]  FASE 7         (conjunto próximo sem filtro RA, TEM_ID=1)
--   [10] FASE 8         (número de quadra mais próximo, TEM_ID=1)
--   [11] FASE 9         (IDs não reconhecidos, TEM_ID=0)
--   [12] FASE 10        (QUADRA_NUMERO + Q_INFORMAL, TEM_ID=0)
--   [13] FASE 11        (CHACARA, TEM_ID=0)
--   [14] FASE 12        (RUA/AVENIDA Jaccard, TEM_ID=0)
--   [15] FASE 13        (nominais bigramas + Jaccard, TEM_ID=0)
--   [16] FASE 14        (QUADRA sem filtro RA, TEM_ID=0)
--   [17] FASE 15        (RUA/AVENIDA IGNORADO sem filtro RA, TEM_ID=0)
--   [18] RESULTADO      (GEORREF_COMPLETO + diagnóstico)
-- ================================================================


-- ================================================================
-- [0] LOOKUP TABLES
-- Materializadas uma vez em CNEFE.duckdb por PREP_CNEFE.sql.
-- Aqui só apontamos VIEWs para elas — sem custo de recriação.
-- ================================================================

CREATE OR REPLACE VIEW identificadores AS SELECT * FROM cnefe.identificadores;
CREATE OR REPLACE VIEW ID_SECUNDARIOS AS SELECT * FROM cnefe.ID_SECUNDARIOS;

-- Endereço que não é residência: presídio, situação de rua, sem endereço.
-- As fases 13 e 18 pulam esses casos, que seguem para o centroide da RA ou,
-- no R, para o ponto fixo do complexo penitenciário. Palavra inteira (\b):
-- como substring, CIR casava com CIRCULAR. Inclui os termos de REGEX_DETENTO
-- e REGEX_SITUACAO_RUA (georreferenciamento_funcoes.R) -- manter as listas
-- juntas -- mais os da lista original da fase 18 (MORADOR, MISSAO, NAO DEIXOU,
-- NAO INFORMADO).
CREATE OR REPLACE TEMP MACRO nao_residencial(txt) AS
    regexp_matches(
        strip_accents(UPPER(txt)),
        '\b(PAPUDA|PRESIDIO|PENITENCIARI[AO]|PRISIONAL|PRISAO|CADEIA PUBLICA|CENTRO DE DETENCAO|DETENTO|APENADO|PRESO|CDP|CPP|CIR|PDF|SITUACAO DE RUA|MORADORA?|PESSOA EM SITUACAO|POPULACAO DE RUA|POP RUA|ALBERGUE|ALBERGADO|ANDARILHO|MISSAO|NAO DEIXOU|NAO INFORMADO)\b'
    );

-- ================================================================
-- [1] SINAN
-- Fonte: SINANDF
-- Normalização aplicada antes da tokenização para maximizar match
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN AS

WITH base AS (
    SELECT
        s.*,
        COALESCE(NULLIF(TRIM(s.NM_LOGRADO), ''), s.NM_COMPLEM) AS LOGRADOURO_RAW
    FROM SINAN_TEMP s
),

numero_extraido AS (
    SELECT
        b.*,
        REGEXP_EXTRACT(
            UPPER(b.LOGRADOURO_RAW),
            '(?:CASA|APARTAMENTO|AP|CS|C|CHACARA)\s*([0-9]+)',
            1
        ) AS NUMERO_EXTRAIDO
    FROM base b
),

numero_final AS (
    SELECT
        n.*,
        COALESCE(
            NULLIF(
                CASE
                    WHEN n.NU_NUMERO IS NULL
                      OR TRIM(CAST(n.NU_NUMERO AS VARCHAR)) = ''
                      OR UPPER(TRIM(CAST(n.NU_NUMERO AS VARCHAR))) IN ('S/N','SN','S N')
                    THEN NULL
                    ELSE TRIM(CAST(n.NU_NUMERO AS VARCHAR))
                END, ''
            ),
            NULLIF(n.NUMERO_EXTRAIDO, ''),
            '0'
        ) AS NU_NUMERO_FINAL
    FROM numero_extraido n
),

-- normalização em etapas para evitar erro de parse do DuckDB
norm1 AS (
    SELECT *, UPPER(LOGRADOURO_RAW) AS L
    FROM numero_final
),

norm2 AS (
    -- typos de CONJUNTO + CONJ colado a número
    SELECT *,
        REGEXP_REPLACE(
        REGEXP_REPLACE(L,
        'CJUNTO|CONUNTO|COJUNTO', 'CONJUNTO', 'g'),
        'CONJ([0-9])', 'CONJUNTO \1', 'g') AS L2
    FROM norm1
),

norm3 AS (
    -- CONJ solto → CONJUNTO
    SELECT *, REGEXP_REPLACE(L2, '\bCONJ\b', 'CONJUNTO', 'g') AS L3
    FROM norm2
),

norm4 AS (
    -- número colado a palavra e vice-versa: QR312CONJUNTO → QR 312 CONJUNTO
    SELECT *,
        REGEXP_REPLACE(
        REGEXP_REPLACE(L3,
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '([A-Z]{2,})([0-9]+)', '\1 \2', 'g') AS L4
    FROM norm3
),

norm5 AS (
    -- letra subfixo colada ao número: 12C → 12 C
    SELECT *, REGEXP_REPLACE(L4, '([0-9]+)([A-Z])\b', '\1 \2', 'g') AS L5
    FROM norm4
),

norm6 AS (
    -- caracteres especiais
    SELECT *, REGEXP_REPLACE(L5, '[,;()]', ' ', 'g') AS L6
    FROM norm5
),

logradouro_final AS (
    SELECT *, TRIM(REGEXP_REPLACE(L6, '\s+', ' ', 'g')) AS LOGRADOURO
    FROM norm6
),

tokens AS (
    SELECT
        f.*,
        STRING_SPLIT(
            REGEXP_REPLACE(f.LOGRADOURO, '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS LOGRADOURO_TOKENS
    FROM logradouro_final f
)

SELECT DISTINCT ON (b.NU_NOTIFIC)
    b.NU_NOTIFIC,
    b.NM_LOGRADO,
    b.NM_COMPLEM,
    b.Regiao_adm AS REGIAO_ADM,
    b.LOGRADOURO,
    b.NU_NUMERO_FINAL AS NU_NUMERO,
    b.LOGRADOURO_TOKENS,
    CASE
        WHEN EXISTS (
            SELECT 1
            FROM UNNEST(b.LOGRADOURO_TOKENS) AS t(token)
            JOIN identificadores i ON t.token = i.id
        )
        THEN 1
        ELSE 0
    END AS TEM_ID
FROM tokens b;

-- corrige RA para casar com NM_SUBDIST do CNEFE. Os nomes do CNEFE vêm com
-- maiúsculas e acentos (ex.: Riacho Fundo, Planaltina), iguais aos que o ETL
-- do app gera -- e a comparação diferencia maiúsculas.
CREATE OR REPLACE TEMP TABLE SINAN_CORR AS
SELECT *,
    CASE REGIAO_ADM
        WHEN 'Riacho Fundo I' THEN 'Riacho Fundo'
        -- RAs criadas depois da malha do Censo 2022: o CNEFE ainda registra
        -- os endereços delas dentro da RA de origem
        WHEN 'Arapoanga'      THEN 'Planaltina'
        WHEN 'Água Quente'    THEN 'Recanto das Emas'
        ELSE REGIAO_ADM
    END AS REGIAO_ADM_NORM
FROM SINAN;

-- diagnóstico [1]
SELECT TEM_ID, COUNT(*) AS n FROM SINAN GROUP BY TEM_ID ORDER BY TEM_ID;


-- ================================================================
-- [2] CNEFE
-- Fonte: CNEFEDF
-- Normaliza CONJ → CONJUNTO e BL → BLOCO para alinhar com SINAN
-- ================================================================

CREATE OR REPLACE VIEW CNEFE_AUX AS SELECT * FROM cnefe.CNEFE_AUX;

-- diagnóstico [2]
SELECT COUNT(*) AS n_cnefe FROM CNEFE_AUX;


-- ================================================================
-- [3] FASE 1 — BIGRAMAS EXATOS (TEM_ID = 1)
-- Todos os bigramas do SINAN devem estar presentes no CNEFE
-- Desempate por menor distância de número de imóvel
-- ================================================================

-- explode SINAN com posição de token
CREATE OR REPLACE TEMP TABLE SINAN_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.LOGRADOURO_TOKENS,
    tok.token,
    tok.pos,
    s.LOGRADOURO_TOKENS[tok.pos + 1] AS token_next
FROM SINAN_CORR s,
     UNNEST(s.LOGRADOURO_TOKENS) WITH ORDINALITY AS tok(token, pos)
WHERE s.TEM_ID = 1
  AND tok.token != '';

-- explode CNEFE com posição de token
CREATE OR REPLACE VIEW CNEFE_TOKENS_POS AS SELECT * FROM cnefe.CNEFE_TOKENS_POS;

-- bigramas SINAN (apenas tokens que são identificadores primários ou secundários)
CREATE OR REPLACE TEMP TABLE SINAN_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token       AS bigrama_chave,
    s.token_next  AS bigrama_valor
FROM SINAN_TOKENS_POS s
WHERE (
    s.token IN (SELECT id FROM identificadores)
    OR s.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND s.token_next IS NOT NULL
AND s.token_next != '';

-- bigramas CNEFE
CREATE OR REPLACE VIEW CNEFE_BIGRAMAS AS SELECT * FROM cnefe.CNEFE_BIGRAMAS;

-- match fase 1
CREATE OR REPLACE TEMP TABLE MATCH_POSICIONAL AS

WITH bigramas_por_notific AS (
    SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
    FROM SINAN_BIGRAMAS
    GROUP BY NU_NOTIFIC
),

intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN bigramas_por_notific b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    -- match exato: todos os bigramas do SINAN presentes no CNEFE
    WHERE i.BIGRAMAS_INTERSECAO = b.BIGRAMAS_SINAN_TOTAL
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    BIGRAMAS_INTERSECAO,
    BIGRAMAS_SINAN_TOTAL,
    DIST_NUMERO,
    'POSICIONAL' AS FASE_MATCH
FROM ranked
WHERE RN = 1;

-- diagnóstico [3]
SELECT COUNT(*) AS match_fase1 FROM MATCH_POSICIONAL;

-- ================================================================
-- [4] FASE 2 — BIGRAMAS RELAXADOS (TEM_ID=1 sem match na Fase 1)
-- Threshold: >= 75% dos bigramas devem casar
-- Aceita variações residuais de grafia não cobertas pela normalização
-- ================================================================
-- ================================================================
-- SINAN FASE 2: não matchados com TEM_ID=1, com normalização
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE2 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 1
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
),

normalizado AS (
    SELECT
        *,
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        -- typos de CONJUNTO
        'CJUNTO|CONUNTO|COJUNTO', 'CONJUNTO', 'g'),
        -- CONJ colado ao número: CONJ2 → CONJUNTO 2
        'CONJ([0-9])', 'CONJUNTO \1', 'g'),
        -- CONJ abreviado solto → CONJUNTO
        '\bCONJ\b', 'CONJUNTO', 'g'),
        -- QUADRA por extenso → QUADRA (mantém, está em ID_SECUNDARIOS)
        '\bQUADRA\b', 'QUADRA', 'g'),
        -- letra colada ao número do identificador: QN12D → QN 12 D
        '([A-Z]{2,4})([0-9]+)([A-Z])\b', '\1 \2 \3', 'g'),
        -- número colado à letra: QN 12D → QN 12 D
        '([0-9]+)([A-Z])\b', '\1 \2', 'g'),
        -- colado sem espaço antes de CONJUNTO/BLOCO: QR312CONJUNTO → QR 312 CONJUNTO
        '([0-9])([A-Z]{2,})', '\1 \2', 'g'),
        -- múltiplos espaços
        '\s+', ' ', 'g')
        AS LOGRADOURO_NORM
    FROM nao_matchados
),

re_tokenizado AS (
    SELECT
        *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM normalizado
)

SELECT * FROM re_tokenizado;

-- ================================================================
-- EXPLODE SINAN FASE 2 com posição
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_F2_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    s.TOKENS_NORM[tok.pos + 1] AS token_next
FROM SINAN_FASE2 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- ================================================================
-- BIGRAMAS SINAN FASE 2
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_F2_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token       AS bigrama_chave,
    s.token_next  AS bigrama_valor
FROM SINAN_F2_TOKENS_POS s
WHERE (
    s.token IN (SELECT id FROM identificadores)
    OR s.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND s.token_next IS NOT NULL
AND s.token_next != '';

-- TOTAL DE BIGRAMAS POR NOTIFICAÇÃO
CREATE OR REPLACE TEMP TABLE SINAN_F2_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F2_BIGRAMAS
GROUP BY NU_NOTIFIC;

-- (bloco de recriação de CNEFE_TOKENS_POS/CNEFE_BIGRAMAS removido —
--  era idêntico ao já criado acima; ID_SECUNDARIOS nunca mudava entre
--  as duas execuções, então a "recriação" era trabalho repetido à toa.
--  As VIEWs criadas no início do script já cobrem este uso.)

-- ================================================================
-- MATCH FASE 2
-- ================================================================

CREATE OR REPLACE TEMP TABLE MATCH_FASE2 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F2_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F2_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    WHERE i.BIGRAMAS_INTERSECAO >= CEIL(b.BIGRAMAS_SINAN_TOTAL * 0.75)
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY BIGRAMAS_INTERSECAO DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    BIGRAMAS_INTERSECAO,
    BIGRAMAS_SINAN_TOTAL,
    DIST_NUMERO,
    'POSICIONAL_NORM' AS FASE_MATCH
FROM ranked
WHERE RN = 1;

SELECT COUNT(*) FROM MATCH_FASE2;

-- ================================================================
-- FASE 3 — SUBFIXO LETRA (QN 8 F CONJUNTO 7)
-- O CNEFE não registra a letra intermediária (F, D, B, C, E...)
-- Estratégia: gerar bigramas ignorando tokens de letra única
--             que aparecem entre o ID primário e CONJUNTO/LOTE
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE3 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 1
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
),

-- normalização igual à fase 2 mais remoção de letra única entre tokens relevantes
norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        -- QUADRA por extenso → remove prefixo
        '^\s*QUADRA\s+', '', 'g'),
        -- CONJ colado a número
        'CONJ([0-9])', 'CONJUNTO \1', 'g'),
        -- CONJ solto → CONJUNTO
        '\bCONJ\b', 'CONJUNTO', 'g'),
        -- CONJUNTO colado a letra: CONJUNTOG → CONJUNTO G
        'CONJUNTO([A-Z])\b', 'CONJUNTO \1', 'g'),
        -- número colado a palavra
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM nao_matchados
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

-- explode com posição
CREATE OR REPLACE TEMP TABLE SINAN_F3_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    -- token_next: pula letras únicas intermediárias
    -- se o próximo token é letra única, pega o seguinte
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND (tok.token IN (SELECT id FROM identificadores)
              OR tok.token IN (SELECT id FROM ID_SECUNDARIOS))
        THEN s.TOKENS_NORM[tok.pos + 2]  -- pula a letra
        ELSE s.TOKENS_NORM[tok.pos + 1]  -- usa o próximo normalmente
    END AS token_next
FROM SINAN_FASE3 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- bigramas fase 3 (com salto de letra)
CREATE OR REPLACE TEMP TABLE SINAN_F3_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token       AS bigrama_chave,
    s.token_next  AS bigrama_valor
FROM SINAN_F3_TOKENS_POS s
WHERE (
    s.token IN (SELECT id FROM identificadores)
    OR s.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND s.token_next IS NOT NULL
AND s.token_next != ''
-- exclui bigrama onde valor é letra única (seriam matches espúrios)
AND LENGTH(s.token_next) > 1;

-- total por notificação
CREATE OR REPLACE TEMP TABLE SINAN_F3_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F3_BIGRAMAS
GROUP BY NU_NOTIFIC;

-- match fase 3
CREATE OR REPLACE TEMP TABLE MATCH_FASE3 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F3_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F3_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    WHERE i.BIGRAMAS_INTERSECAO >= CEIL(b.BIGRAMAS_SINAN_TOTAL * 0.75)
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY BIGRAMAS_INTERSECAO DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    BIGRAMAS_INTERSECAO,
    BIGRAMAS_SINAN_TOTAL,
    DIST_NUMERO,
    'SUBFIXO_LETRA' AS FASE_MATCH
FROM ranked
WHERE RN = 1;

-- diagnóstico fase 3
SELECT COUNT(*) AS match_fase3 FROM MATCH_FASE3;

-- ================================================================
-- FASE 4 — SÓ LOTE SEM CONJUNTO
-- Endereços como QNM 12 LOTE 3, QNA 40 LOTE 3
-- Bigrama primário (ID, número) + (LOTE, número)
-- Sem exigir CONJUNTO
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE4 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 1
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND LOGRADOURO LIKE '%LOTE%'
      AND LOGRADOURO NOT LIKE '%CONJUNTO%'
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        '^\s*QUADRA\s+', '', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM nao_matchados
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

CREATE OR REPLACE TEMP TABLE SINAN_F4_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    s.TOKENS_NORM[tok.pos + 1] AS token_next
FROM SINAN_FASE4 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

CREATE OR REPLACE TEMP TABLE SINAN_F4_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token       AS bigrama_chave,
    s.token_next  AS bigrama_valor
FROM SINAN_F4_TOKENS_POS s
WHERE (
    s.token IN (SELECT id FROM identificadores)
    OR s.token = 'LOTE'
)
AND s.token_next IS NOT NULL
AND s.token_next != ''
AND LENGTH(s.token_next) > 1;

CREATE OR REPLACE TEMP TABLE SINAN_F4_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F4_BIGRAMAS
GROUP BY NU_NOTIFIC;

CREATE OR REPLACE TEMP TABLE MATCH_FASE4 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F4_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F4_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    -- exige 100% dos bigramas para ser mais preciso (menos bigramas = mais risco)
    WHERE i.BIGRAMAS_INTERSECAO = b.BIGRAMAS_SINAN_TOTAL
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    BIGRAMAS_INTERSECAO,
    BIGRAMAS_SINAN_TOTAL,
    DIST_NUMERO,
    'SO_LOTE' AS FASE_MATCH
FROM ranked
WHERE RN = 1;

-- diagnóstico fase 4
SELECT COUNT(*) AS match_fase4 FROM MATCH_FASE4;

-- ================================================================
-- FASE 5 — MATCH POR BIGRAMA PRIMÁRIO APENAS
-- Para estruturas corretas que não matcharam nas fases anteriores
-- Aceita qualquer endereço da mesma quadra (centroide aproximado)
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE5 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 1
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        -- remove prefixos lixo antes do ID: CSSM 1, CSC 7, QUADRA, RUA 9
        '^(CSSM|CSC|ADE|QUADRA|RUA|SETOR|PARQUE|JARDIM|AVFL)[A-Z0-9 ]*?(Q[A-Z]+|S[A-Z]+|CL[A-Z]+|AOS)', '\2', 'g'),
        -- CONJ colado a letra: CONJB → CONJUNTO B
        '\bCONJ([A-TV-Z])\b', 'CONJUNTO \1', 'g'),
        -- CONJUNTO colado a letra: CONJUNTOG → CONJUNTO G
        'CONJUNTO([A-Z])\b', 'CONJUNTO \1', 'g'),
        -- número colado a palavra
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '([A-Z]{2,})([0-9]+)', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM nao_matchados
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

-- explode com posição
CREATE OR REPLACE TEMP TABLE SINAN_F5_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    s.TOKENS_NORM[tok.pos + 1] AS token_next
FROM SINAN_FASE5 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- bigramas APENAS do identificador primário (ignora secundários)
CREATE OR REPLACE TEMP TABLE SINAN_F5_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F5_TOKENS_POS s
WHERE s.token IN (SELECT id FROM identificadores)
  AND s.token_next IS NOT NULL
  AND s.token_next != ''
  -- valor deve ser número (não letra, não palavra)
  AND s.token_next ~ '^[0-9]+$';

-- match fase 5: só bigrama primário, centroide da quadra
CREATE OR REPLACE TEMP TABLE MATCH_FASE5 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F5_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

com_bonus AS (
    SELECT
        i.*,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO,

        -- bonus: verifica se algum bigrama secundário do CNEFE
        -- (CONJUNTO X, BLOCO X) aparece nos tokens do SINAN
        CASE WHEN EXISTS (
            SELECT 1
            FROM CNEFE_BIGRAMAS cb
            WHERE cb.ENDERECO_CNEFE = i.ENDERECO_CNEFE
              AND cb.bigrama_chave IN (SELECT id FROM ID_SECUNDARIOS)
              AND EXISTS (
                  SELECT 1
                  FROM SINAN_F5_TOKENS_POS sp
                  WHERE sp.NU_NOTIFIC  = i.NU_NOTIFIC
                    AND sp.token       = cb.bigrama_chave
                    AND sp.token_next  = cb.bigrama_valor
              )
        ) THEN 1 ELSE 0 END AS BONUS_SECUNDARIO

    FROM intersecao i
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY
                DIST_NUMERO       ASC NULLS LAST,
                BONUS_SECUNDARIO  DESC  -- prefere candidato cujo conjunto bate
        ) AS RN
    FROM com_bonus
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    BIGRAMAS_INTERSECAO,
    1                  AS BIGRAMAS_SINAN_TOTAL,
    DIST_NUMERO,
    BONUS_SECUNDARIO,
    'QUADRA_APROX'     AS FASE_MATCH
FROM ranked
WHERE RN = 1;

-- diagnóstico
SELECT COUNT(*) AS match_fase5 FROM MATCH_FASE5;

-- ================================================================
-- FASE 6 — IGNORADO (RA desconhecida)
-- Remove filtro de RA, compensa com match exato de todos os bigramas
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE6 AS

SELECT *
FROM SINAN_CORR
-- UPPER(): o ETL grava Ignorado, e sem ele esta fase nunca selecionava ninguém
WHERE UPPER(REGIAO_ADM) = 'IGNORADO'
  AND TEM_ID = 1
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5);

-- explode com posição
CREATE OR REPLACE TEMP TABLE SINAN_F6_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.LOGRADOURO_TOKENS,
    tok.token,
    tok.pos,
    s.LOGRADOURO_TOKENS[tok.pos + 1] AS token_next
FROM SINAN_FASE6 s,
     UNNEST(s.LOGRADOURO_TOKENS) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- bigramas SINAN fase 6
CREATE OR REPLACE TEMP TABLE SINAN_F6_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F6_TOKENS_POS s
WHERE (
    s.token IN (SELECT id FROM identificadores)
    OR s.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND s.token_next IS NOT NULL
AND s.token_next != '';

-- total de bigramas por notificação
CREATE OR REPLACE TEMP TABLE SINAN_F6_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F6_BIGRAMAS
GROUP BY NU_NOTIFIC;

-- match fase 6: sem filtro de RA, exige 100% dos bigramas
CREATE OR REPLACE TEMP TABLE MATCH_FASE6 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F6_BIGRAMAS s
    -- sem filtro de RA
    JOIN CNEFE_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F6_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    -- exige 100%: sem RA o risco de falso positivo é maior
    WHERE i.BIGRAMAS_INTERSECAO = b.BIGRAMAS_SINAN_TOTAL
),

com_bonus AS (
    SELECT *,
        CASE WHEN EXISTS (
            SELECT 1
            FROM CNEFE_BIGRAMAS cb
            WHERE cb.ENDERECO_CNEFE = candidatos.ENDERECO_CNEFE
              AND cb.bigrama_chave IN (SELECT id FROM ID_SECUNDARIOS)
              AND EXISTS (
                  SELECT 1
                  FROM SINAN_F6_TOKENS_POS sp
                  WHERE sp.NU_NOTIFIC  = candidatos.NU_NOTIFIC
                    AND sp.token       = cb.bigrama_chave
                    AND sp.token_next  = cb.bigrama_valor
              )
        ) THEN 1 ELSE 0 END AS BONUS_SECUNDARIO
    FROM candidatos
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY
                BIGRAMAS_INTERSECAO DESC,
                DIST_NUMERO         ASC NULLS LAST,
                BONUS_SECUNDARIO    DESC
        ) AS RN
    FROM com_bonus
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    BIGRAMAS_INTERSECAO,
    BIGRAMAS_SINAN_TOTAL,
    DIST_NUMERO,
    'IGNORADO' AS FASE_MATCH
FROM ranked
WHERE RN = 1;

-- diagnóstico
SELECT COUNT(*) AS match_fase6 FROM MATCH_FASE6;

-- ================================================================
-- FASE 7 — MATCH HIERÁRQUICO RELAXADO
-- Para os ~291 com estrutura correta mas conjunto inexistente no CNEFE
-- + casos com typos residuais e prefixos lixo
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE7 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 1
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        -- remove prefixos lixo antes do ID real
        '^(CSSM|CSC|ADE|CSB|AVFL|JARDIM|PARQUE|ALBERG)[A-Z0-9 ]*?(Q[A-Z]+|S[A-Z]+|CL[A-Z]+|AOS|SHIN|SHIS)', '\2', 'g'),
        -- QUADRA solto antes do ID: QUADRA QR → QR
        '^\s*QUADRA\s+', '', 'g'),
        -- typo CONJU NTO → CONJUNTO
        'CONJU\s+NTO', 'CONJUNTO', 'g'),
        -- COMJ → CONJUNTO
        '\bCOMJ\b', 'CONJUNTO', 'g'),
        -- CONJ colado a letra: CONJB → CONJUNTO B
        '\bCONJ([A-TV-Z])\b', 'CONJUNTO \1', 'g'),
        -- CONJUNTO colado a letra: CONJUNTOG → CONJUNTO G
        'CONJUNTO([A-Z])\b', 'CONJUNTO \1', 'g'),
        -- número colado a palavra e vice-versa
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM nao_matchados
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

-- explode com posição
CREATE OR REPLACE TEMP TABLE SINAN_F7_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    -- pula letra única intermediária (reaproveitando lógica da fase 3)
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND (tok.token IN (SELECT id FROM identificadores)
              OR tok.token IN (SELECT id FROM ID_SECUNDARIOS))
        THEN s.TOKENS_NORM[tok.pos + 2]
        ELSE s.TOKENS_NORM[tok.pos + 1]
    END AS token_next
FROM SINAN_FASE7 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- bigrama primário (ID, número) — âncora do match
CREATE OR REPLACE TEMP TABLE SINAN_F7_BIGRAMA_PRIM AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F7_TOKENS_POS s
WHERE s.token IN (SELECT id FROM identificadores)
  AND s.token_next ~ '^[0-9]+$'
  AND s.token_next IS NOT NULL
  AND s.token_next != '';

-- bigrama secundário (CONJUNTO/BLOCO, valor) — usado só para bônus
CREATE OR REPLACE TEMP TABLE SINAN_F7_BIGRAMA_SEC AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F7_TOKENS_POS s
WHERE s.token IN (SELECT id FROM ID_SECUNDARIOS)
  AND s.token_next IS NOT NULL
  AND s.token_next != ''
  AND LENGTH(s.token_next) > 0;

-- valor do CONJUNTO no SINAN (para comparação de proximidade)
CREATE OR REPLACE TEMP TABLE SINAN_F7_CONJ AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.token_next AS CONJ_SINAN
FROM SINAN_F7_TOKENS_POS s
WHERE s.token = 'CONJUNTO'
  AND s.token_next IS NOT NULL
  AND s.token_next != '';

-- match fase 7: âncora no bigrama primário + bônus de secundário
CREATE OR REPLACE TEMP TABLE MATCH_FASE7 AS

WITH candidatos AS (
    -- SEM filtro de RA
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST
    FROM SINAN_F7_BIGRAMA_PRIM s
    JOIN CNEFE_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
),

candidatos_com_conj AS (
    SELECT
        cand.*,
        cb.bigrama_valor AS CONJ_CNEFE
    FROM candidatos cand
    LEFT JOIN CNEFE_BIGRAMAS cb
        ON  cb.ENDERECO_CNEFE = cand.ENDERECO_CNEFE
        AND cb.bigrama_chave  = 'CONJUNTO'
),

com_scores AS (
    SELECT
        c.*,
        conj.CONJ_SINAN,
        ABS(
            COALESCE(TRY_CAST(c.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(c.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO,
        CASE
            WHEN c.CONJ_CNEFE = conj.CONJ_SINAN
                THEN 0
            WHEN c.CONJ_CNEFE IS NULL OR conj.CONJ_SINAN IS NULL
                THEN 999
            WHEN LENGTH(c.CONJ_CNEFE) = 1 AND LENGTH(conj.CONJ_SINAN) = 1
             AND c.CONJ_CNEFE ~ '^[A-Z]$' AND conj.CONJ_SINAN ~ '^[A-Z]$'
                THEN ABS(ASCII(c.CONJ_CNEFE) - ASCII(conj.CONJ_SINAN))
            WHEN TRY_CAST(c.CONJ_CNEFE AS INTEGER) IS NOT NULL
             AND TRY_CAST(conj.CONJ_SINAN AS INTEGER) IS NOT NULL
                THEN ABS(
                    CAST(TRY_CAST(c.CONJ_CNEFE   AS INTEGER) AS INTEGER)
                  - CAST(TRY_CAST(conj.CONJ_SINAN AS INTEGER) AS INTEGER)
                )
            ELSE 999
        END AS DIST_CONJUNTO
    FROM candidatos_com_conj c
    LEFT JOIN SINAN_F7_CONJ conj ON conj.NU_NOTIFIC = c.NU_NOTIFIC
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY
                DIST_CONJUNTO ASC NULLS LAST,
                DIST_NUMERO   ASC NULLS LAST
        ) AS RN
    FROM com_scores
    -- exige ao menos que o conjunto seja comparável (não 999)
    -- para evitar matches completamente sem critério
    WHERE DIST_CONJUNTO < 999
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    DIST_NUMERO,
    DIST_CONJUNTO,
    CONJ_SINAN,
    CONJ_CNEFE,
    CASE
        WHEN DIST_CONJUNTO = 0   THEN 'CONJ_EXATO_APROX'
        WHEN DIST_CONJUNTO < 999 THEN 'CONJ_PROXIMO_APROX'
        ELSE                          'QUADRA_APROX'
    END AS FASE_MATCH
FROM ranked
WHERE RN = 1;

SELECT FASE_MATCH, COUNT(*) AS n FROM MATCH_FASE7 GROUP BY FASE_MATCH ORDER BY n DESC;

-- ================================================================
-- FASE 8 — NÚMERO DE QUADRA MAIS PRÓXIMO, SEM FILTRO DE RA
-- Para os ~339 com estrutura correta mas quadra inexistente no CNEFE
-- Estratégia: busca o número de quadra mais próximo do mesmo ID
--             em todo o CNEFE, com bônus se conjunto também bate
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE8 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 1
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        '^(CSSM|CSC|ADE|CSB|AVFL|JARDIM|PARQUE|ALBERG|SETOR HAB)[A-Z0-9 ]*?(Q[A-Z]+|S[A-Z]+|CL[A-Z]+|AOS|SHIN|SHIS)', '\2', 'g'),
        '^\s*QUADRA\s+', '', 'g'),
        'CONJU\s+NTO|COMJ', 'CONJUNTO', 'g'),
        'CONJ([0-9])', 'CONJUNTO \1', 'g'),
        'CONJUNTO([A-Z])\b', 'CONJUNTO \1', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM nao_matchados
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

-- explode com posição + salto de letra intermediária
CREATE OR REPLACE TEMP TABLE SINAN_F8_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND (tok.token IN (SELECT id FROM identificadores)
              OR tok.token IN (SELECT id FROM ID_SECUNDARIOS))
        THEN s.TOKENS_NORM[tok.pos + 2]
        ELSE s.TOKENS_NORM[tok.pos + 1]
    END AS token_next
FROM SINAN_FASE8 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- ID primário do SINAN (ex: QR, QN, QNM...)
CREATE OR REPLACE TEMP TABLE SINAN_F8_ID AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token AS ID_PRIMARIO,
    -- número da quadra no SINAN
    CASE
        WHEN s.token_next ~ '^[0-9]+$' THEN CAST(s.token_next AS INTEGER)
        ELSE NULL
    END AS NUM_QUADRA_SINAN
FROM SINAN_F8_TOKENS_POS s
WHERE s.token IN (SELECT id FROM identificadores)
  AND s.token_next IS NOT NULL
  AND s.token_next != '';

-- valor do CONJUNTO no SINAN
CREATE OR REPLACE TEMP TABLE SINAN_F8_CONJ AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.token_next AS CONJ_SINAN
FROM SINAN_F8_TOKENS_POS s
WHERE s.token = 'CONJUNTO'
  AND s.token_next IS NOT NULL
  AND s.token_next != '';

-- todos os endereços do CNEFE que têm o mesmo ID primário
CREATE OR REPLACE VIEW CNEFE_F8_CANDIDATOS AS SELECT * FROM cnefe.CNEFE_F8_CANDIDATOS;

-- MATCH: para cada notificação, encontra candidatos com ID igual
-- e calcula distância do número de quadra + proximidade do conjunto
CREATE OR REPLACE TEMP TABLE MATCH_FASE8 AS

WITH candidatos AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        s.NUM_QUADRA_SINAN,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        c.NUM_QUADRA_CNEFE,
        c.CONJ_CNEFE,
        -- distância entre número de quadra do SINAN e do CNEFE
        ABS(s.NUM_QUADRA_SINAN - c.NUM_QUADRA_CNEFE) AS DIST_QUADRA
    FROM SINAN_F8_ID s
    JOIN CNEFE_F8_CANDIDATOS c
        ON  c.ID_PRIMARIO      = s.ID_PRIMARIO
        AND s.NUM_QUADRA_SINAN IS NOT NULL
        AND c.NUM_QUADRA_CNEFE IS NOT NULL
),

com_scores AS (
    SELECT
        c.*,
        conj.CONJ_SINAN,
        ABS(
            COALESCE(TRY_CAST(c.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(c.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO,
        -- proximidade do conjunto
        CASE
            WHEN c.CONJ_CNEFE = conj.CONJ_SINAN
                THEN 0
            WHEN c.CONJ_CNEFE IS NULL OR conj.CONJ_SINAN IS NULL
                THEN 999
            WHEN LENGTH(c.CONJ_CNEFE) = 1 AND LENGTH(conj.CONJ_SINAN) = 1
             AND c.CONJ_CNEFE ~ '^[A-Z]$' AND conj.CONJ_SINAN ~ '^[A-Z]$'
                THEN ABS(ASCII(c.CONJ_CNEFE) - ASCII(conj.CONJ_SINAN))
            WHEN TRY_CAST(c.CONJ_CNEFE AS INTEGER) IS NOT NULL
             AND TRY_CAST(conj.CONJ_SINAN AS INTEGER) IS NOT NULL
                THEN ABS(
                    CAST(TRY_CAST(c.CONJ_CNEFE   AS INTEGER) AS INTEGER)
                  - CAST(TRY_CAST(conj.CONJ_SINAN AS INTEGER) AS INTEGER)
                )
            ELSE 50  -- diferente mas comparável, penalidade moderada
        END AS DIST_CONJUNTO
    FROM candidatos c
    LEFT JOIN SINAN_F8_CONJ conj ON conj.NU_NOTIFIC = c.NU_NOTIFIC
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY
                DIST_QUADRA   ASC NULLS LAST,  -- quadra mais próxima primeiro
                DIST_CONJUNTO ASC NULLS LAST,  -- depois conjunto mais próximo
                DIST_NUMERO   ASC NULLS LAST   -- depois número de imóvel
        ) AS RN
    FROM com_scores
    -- só aceita se a quadra for próxima (até 10 números de distância)
    WHERE DIST_QUADRA <= 10
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    NUM_QUADRA_SINAN,
    NUM_QUADRA_CNEFE,
    DIST_QUADRA,
    DIST_CONJUNTO,
    CONJ_SINAN,
    CONJ_CNEFE,
    DIST_NUMERO,
    CASE
        WHEN DIST_QUADRA = 0 AND DIST_CONJUNTO = 0  THEN 'QUADRA_EXATA_CONJ_EXATO'
        WHEN DIST_QUADRA = 0                         THEN 'QUADRA_EXATA_CONJ_APROX'
        ELSE                                              'QUADRA_APROX_CONJ_APROX'
    END AS FASE_MATCH
FROM ranked
WHERE RN = 1;

-- diagnóstico
SELECT FASE_MATCH, COUNT(*) AS n FROM MATCH_FASE8 GROUP BY FASE_MATCH ORDER BY n DESC;
SELECT COUNT(*) AS total_fase8 FROM MATCH_FASE8;

-- SEM IDENTIFICADORES DE ENDEREÇO:
-- ================================================================
-- FASE 9 — IDs NÃO RECONHECIDOS
-- SHSN, SHPS, QSB, CNB, SHTB, AQD, CL, CSB, CR, SQ, SHTB...
-- Mesma lógica de bigramas das fases anteriores
-- ================================================================

-- Ampliar lista de identificadores com os não reconhecidos
CREATE OR REPLACE VIEW identificadores_ext AS SELECT * FROM cnefe.identificadores_ext;

CREATE OR REPLACE TEMP TABLE SINAN_FASE9 AS

WITH base AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 0
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE8)
),

-- filtra só os que têm ID não reconhecido como primeiro token
com_id_ext AS (
    SELECT b.*
    FROM base b
    WHERE EXISTS (
        SELECT 1
        FROM UNNEST(b.LOGRADOURO_TOKENS) AS t(tok)
        JOIN identificadores_ext ie ON t.tok = ie.id
        -- não está na lista original
        AND NOT EXISTS (SELECT 1 FROM identificadores i WHERE i.id = t.tok)
    )
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        'CONJ([0-9])', 'CONJUNTO \1', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM com_id_ext
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

CREATE OR REPLACE TEMP TABLE SINAN_F9_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND (tok.token IN (SELECT id FROM identificadores_ext)
              OR tok.token IN (SELECT id FROM ID_SECUNDARIOS))
        THEN s.TOKENS_NORM[tok.pos + 2]
        ELSE s.TOKENS_NORM[tok.pos + 1]
    END AS token_next
FROM SINAN_FASE9 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

CREATE OR REPLACE TEMP TABLE SINAN_F9_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F9_TOKENS_POS s
WHERE (
    s.token IN (SELECT id FROM identificadores_ext)
    OR s.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND s.token_next IS NOT NULL
AND s.token_next != '';

CREATE OR REPLACE TEMP TABLE SINAN_F9_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F9_BIGRAMAS
GROUP BY NU_NOTIFIC;

-- bigramas CNEFE com lista estendida
CREATE OR REPLACE VIEW CNEFE_BIGRAMAS_EXT AS SELECT * FROM cnefe.CNEFE_BIGRAMAS_EXT;

CREATE OR REPLACE TEMP TABLE MATCH_FASE9 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F9_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS_EXT c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F9_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    WHERE i.BIGRAMAS_INTERSECAO = b.BIGRAMAS_SINAN_TOTAL
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY BIGRAMAS_INTERSECAO DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC, SINAN_LOGRADOURO, SINAN_NUMERO,
    ENDERECO_CNEFE, LATITUDE, LONGITUDE, NM_SUBDIST,
    BIGRAMAS_INTERSECAO, BIGRAMAS_SINAN_TOTAL, DIST_NUMERO,
    'ID_EXT' AS FASE_MATCH
FROM ranked WHERE RN = 1;

SELECT COUNT(*) AS match_fase9 FROM MATCH_FASE9;


-- ================================================================
-- FASE 10 — QUADRA_NUMERO + Q_INFORMAL
-- QUADRA 201 CONJUNTO 2, Q 7 CONJUNTO C, Q01 CONJUNTO F
-- Normaliza QUADRA/Q → token âncora e aplica bigramas
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE10 AS

WITH base AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 0
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE8)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
      AND (
          LOGRADOURO LIKE 'QUADRA %'
          OR REGEXP_MATCHES(LOGRADOURO, '^Q\s*\d+')
          OR REGEXP_MATCHES(LOGRADOURO, '^Q\d+')
      )
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        -- Q informal → QUADRA: Q 7, Q01, Q 44
        '^Q\s*0*(\d+)', 'QUADRA \1', 'g'),
        -- QUADRA + número colado: QUADRA7 → QUADRA 7
        'QUADRA(\d+)', 'QUADRA \1', 'g'),
        -- typos de CONJUNTO
        'CJUNTO|CONUNTO|COJUNTO', 'CONJUNTO', 'g'),
        'CONJ([0-9])', 'CONJUNTO \1', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM base
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

CREATE OR REPLACE TEMP TABLE SINAN_F10_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND tok.token IN ('QUADRA', 'CONJUNTO', 'BLOCO', 'LOTE', 'CHACARA')
        THEN s.TOKENS_NORM[tok.pos + 2]
        ELSE s.TOKENS_NORM[tok.pos + 1]
    END AS token_next
FROM SINAN_FASE10 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- âncora: bigrama (QUADRA, número) + secundários
CREATE OR REPLACE TEMP TABLE SINAN_F10_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F10_TOKENS_POS s
WHERE s.token IN ('QUADRA', 'CONJUNTO', 'BLOCO', 'LOTE', 'CHACARA', 'RUA', 'VIA')
  AND s.token_next IS NOT NULL
  AND s.token_next != ''
  AND LENGTH(s.token_next) > 0;

CREATE OR REPLACE TEMP TABLE SINAN_F10_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F10_BIGRAMAS
GROUP BY NU_NOTIFIC;

-- bigramas CNEFE com QUADRA como chave
CREATE OR REPLACE VIEW CNEFE_BIGRAMAS_QUADRA AS SELECT * FROM cnefe.CNEFE_BIGRAMAS_QUADRA;

CREATE OR REPLACE TEMP TABLE MATCH_FASE10 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F10_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS_QUADRA c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F10_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    WHERE i.BIGRAMAS_INTERSECAO >= CEIL(b.BIGRAMAS_SINAN_TOTAL * 0.75)
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY BIGRAMAS_INTERSECAO DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC, SINAN_LOGRADOURO, SINAN_NUMERO,
    ENDERECO_CNEFE, LATITUDE, LONGITUDE, NM_SUBDIST,
    BIGRAMAS_INTERSECAO, BIGRAMAS_SINAN_TOTAL, DIST_NUMERO,
    'QUADRA_NOMINAL' AS FASE_MATCH
FROM ranked WHERE RN = 1;

SELECT COUNT(*) AS match_fase10 FROM MATCH_FASE10;


-- ================================================================
-- FASE 11 — CHACARA
-- CHACARA 28 CONJUNTO 1 LOTE 1, SHSN CHAC 61 LOTE 21
-- Bigrama (CHACARA/CHAC, número) + (LOTE, número)
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE11 AS

WITH base AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 0
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE8)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
      AND (
          LOGRADOURO LIKE '%CHACARA%'
          OR LOGRADOURO LIKE '%CHAC %'
          OR LOGRADOURO LIKE '%CH %'
      )
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        -- normaliza abreviações de chácara
        '\bCHAC\b|\bCH\b', 'CHACARA', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM base
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

CREATE OR REPLACE TEMP TABLE SINAN_F11_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    s.TOKENS_NORM[tok.pos + 1] AS token_next
FROM SINAN_FASE11 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

CREATE OR REPLACE TEMP TABLE SINAN_F11_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F11_TOKENS_POS s
WHERE s.token IN ('CHACARA', 'LOTE', 'CONJUNTO', 'BLOCO')
  AND s.token_next IS NOT NULL
  AND s.token_next != ''
  AND s.token_next ~ '^[0-9A-Z]+$';

CREATE OR REPLACE TEMP TABLE SINAN_F11_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F11_BIGRAMAS
GROUP BY NU_NOTIFIC;

-- bigramas CNEFE com CHACARA
CREATE OR REPLACE VIEW CNEFE_BIGRAMAS_CHACARA AS SELECT * FROM cnefe.CNEFE_BIGRAMAS_CHACARA;

CREATE OR REPLACE TEMP TABLE MATCH_FASE11 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F11_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS_CHACARA c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F11_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    WHERE i.BIGRAMAS_INTERSECAO >= CEIL(b.BIGRAMAS_SINAN_TOTAL * 0.75)
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY BIGRAMAS_INTERSECAO DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC, SINAN_LOGRADOURO, SINAN_NUMERO,
    ENDERECO_CNEFE, LATITUDE, LONGITUDE, NM_SUBDIST,
    BIGRAMAS_INTERSECAO, BIGRAMAS_SINAN_TOTAL, DIST_NUMERO,
    'CHACARA' AS FASE_MATCH
FROM ranked WHERE RN = 1;

SELECT COUNT(*) AS match_fase11 FROM MATCH_FASE11;

-- ================================================================
-- FASE 12 — RUA / AVENIDA (Jaccard de tokens)
-- Compara tokens do SINAN contra tokens do CNEFE
-- Stopwords removidas, filtro de RA obrigatório
-- ================================================================

-- tabela de stopwords para ignorar tokens genéricos
CREATE OR REPLACE VIEW stopwords AS SELECT * FROM cnefe.stopwords;

CREATE OR REPLACE TEMP TABLE SINAN_FASE12 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 0
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
      AND (
          LOGRADOURO LIKE 'RUA%'
          OR LOGRADOURO LIKE 'AVENIDA%'
          OR LOGRADOURO LIKE 'AV %'
          OR LOGRADOURO LIKE 'AV.%'
      )
      AND UPPER(REGIAO_ADM_NORM) != 'IGNORADO'
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        '\bAV\b\.?', 'AVENIDA', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM nao_matchados
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

-- explode SINAN fase 12 — remove stopwords
CREATE OR REPLACE TEMP TABLE SINAN_F12_TOKENS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    tok.token
FROM SINAN_FASE12 s,
     UNNEST(s.TOKENS_NORM) AS tok(token)
WHERE tok.token != ''
  AND tok.token NOT IN (SELECT sw FROM stopwords);

-- total de tokens por notificação (para calcular Jaccard)
CREATE OR REPLACE TEMP TABLE SINAN_F12_TOKEN_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS TOKENS_SINAN_TOTAL
FROM SINAN_F12_TOKENS
GROUP BY NU_NOTIFIC;

-- explode CNEFE para RUA/AVENIDA — remove stopwords
CREATE OR REPLACE VIEW CNEFE_F12_TOKENS AS SELECT * FROM cnefe.CNEFE_F12_TOKENS;

-- total de tokens por endereço CNEFE
CREATE OR REPLACE VIEW CNEFE_F12_TOKEN_TOTAL AS SELECT * FROM cnefe.CNEFE_F12_TOKEN_TOTAL;

CREATE OR REPLACE TEMP TABLE MATCH_FASE12 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS TOKENS_INTERSECAO
    FROM SINAN_F12_TOKENS s
    JOIN CNEFE_F12_TOKENS c
        ON  c.token      = s.token
        AND c.NM_SUBDIST = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

com_jaccard AS (
    SELECT
        i.*,
        ts.TOKENS_SINAN_TOTAL,
        tc.TOKENS_CNEFE_TOTAL,
        -- Jaccard: intersecao / uniao
        ROUND(
            CAST(i.TOKENS_INTERSECAO AS DOUBLE) /
            NULLIF(
                CAST(ts.TOKENS_SINAN_TOTAL AS DOUBLE)
                + CAST(tc.TOKENS_CNEFE_TOTAL AS DOUBLE)
                - CAST(i.TOKENS_INTERSECAO AS DOUBLE),
            0),
        4) AS JACCARD,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F12_TOKEN_TOTAL  ts ON ts.NU_NOTIFIC    = i.NU_NOTIFIC
    JOIN CNEFE_F12_TOKEN_TOTAL  tc ON tc.ENDERECO_CNEFE = i.ENDERECO_CNEFE
    WHERE i.TOKENS_INTERSECAO >= 2
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY JACCARD DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM com_jaccard
    WHERE JACCARD >= 0.4
)

SELECT
    NU_NOTIFIC, SINAN_LOGRADOURO, SINAN_NUMERO,
    ENDERECO_CNEFE, LATITUDE, LONGITUDE, NM_SUBDIST,
    TOKENS_INTERSECAO, TOKENS_SINAN_TOTAL, JACCARD, DIST_NUMERO,
    'RUA_AVENIDA' AS FASE_MATCH
FROM ranked WHERE RN = 1;

SELECT COUNT(*) AS match_fase12 FROM MATCH_FASE12;

-- ================================================================
-- FASE 13 — NOMINAIS COM BIGRAMAS (QUADRA, LOTE, CHACARA, RUA)
-- Usa bigramas dos tokens estruturais como âncora principal
-- Jaccard como score de desempate entre candidatos
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE13 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 0
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE12)
      AND LOGRADOURO IS NOT NULL
      AND TRIM(LOGRADOURO) != ''
      AND UPPER(REGIAO_ADM_NORM) != 'IGNORADO'
      -- endereço não residencial -- ver nao_residencial(), no início. Antes
      -- só excluía quatro textos exatos (SISTEMA PRISIONAL, CDP, CIR,
      -- PENITENCIARIA FEMININA DO DF).
      AND NOT nao_residencial(LOGRADOURO)
),

norm AS (
    SELECT *,
        TRIM(
            REGEXP_REPLACE(
            REGEXP_REPLACE(
            REGEXP_REPLACE(
            REGEXP_REPLACE(
            REGEXP_REPLACE(
            REGEXP_REPLACE(
                UPPER(LOGRADOURO),
            '^Q\s*0*(\d+)', 'QUADRA \1', 'g'),
            'QUADRA(\d+)', 'QUADRA \1', 'g'),
            '\bCHAC\b|\bCH\b', 'CHACARA', 'g'),
            'CJUNTO|CONUNTO|COJUNTO', 'CONJUNTO', 'g'),
            'CONJ([0-9])', 'CONJUNTO \1', 'g'),
            '([0-9]+)([A-Z]{2,})', '\1 \2', 'g')
        ) AS LOGRADOURO_NORM
    FROM nao_matchados
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

-- explode com posição para bigramas
CREATE OR REPLACE TEMP TABLE SINAN_F13_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    -- pula letra única intermediária
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND tok.token IN ('QUADRA','CONJUNTO','BLOCO','LOTE','CHACARA','RUA','AVENIDA','VIA')
        THEN s.TOKENS_NORM[tok.pos + 2]
        ELSE s.TOKENS_NORM[tok.pos + 1]
    END AS token_next
FROM SINAN_FASE13 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- bigramas estruturais do SINAN
CREATE OR REPLACE TEMP TABLE SINAN_F13_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F13_TOKENS_POS s
WHERE s.token IN ('QUADRA','CONJUNTO','BLOCO','LOTE','CHACARA','RUA','AVENIDA','VIA','TRECHO')
  AND s.token_next IS NOT NULL
  AND s.token_next != ''
  AND LENGTH(s.token_next) > 0;

-- total de bigramas por notificação
CREATE OR REPLACE TEMP TABLE SINAN_F13_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F13_BIGRAMAS
GROUP BY NU_NOTIFIC;

-- bigramas CNEFE com chaves nominais
CREATE OR REPLACE VIEW CNEFE_F13_BIGRAMAS AS SELECT * FROM cnefe.CNEFE_F13_BIGRAMAS;

-- tokens SINAN para Jaccard — com REGIAO_ADM_NORM para filtro no JOIN
CREATE OR REPLACE TEMP TABLE SINAN_F13_TOK_JAC AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.REGIAO_ADM_NORM,
    tok.token
FROM SINAN_FASE13 s,
     UNNEST(s.TOKENS_NORM) AS tok(token)
WHERE tok.token != ''
  AND tok.token NOT IN (SELECT sw FROM stopwords)
  AND LENGTH(tok.token) > 1;

CREATE OR REPLACE TEMP TABLE SINAN_F13_TOK_JAC_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS N FROM SINAN_F13_TOK_JAC GROUP BY NU_NOTIFIC;

-- tokens CNEFE para Jaccard
CREATE OR REPLACE VIEW CNEFE_F13_TOK_JAC AS SELECT * FROM cnefe.CNEFE_F13_TOK_JAC;

CREATE OR REPLACE VIEW CNEFE_F13_TOK_JAC_TOTAL AS SELECT * FROM cnefe.CNEFE_F13_TOK_JAC_TOTAL;

-- MATCH FASE 13
CREATE OR REPLACE TEMP TABLE MATCH_FASE13 AS

WITH intersecao_bigramas AS (
    -- JOIN pelos bigramas estruturais
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F13_BIGRAMAS s
    JOIN CNEFE_F13_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

-- filtra candidatos com pelo menos 1 bigrama em comum
candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao_bigramas i
    JOIN SINAN_F13_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    WHERE i.BIGRAMAS_INTERSECAO >= CEIL(b.BIGRAMAS_SINAN_TOTAL * 0.75)
),

-- Jaccard para desempate entre candidatos com mesmo bigrama score
jaccard_inter AS (
    SELECT
        s.NU_NOTIFIC,
        c.ENDERECO_CNEFE,
        COUNT(*) AS JAC_INTER
    FROM SINAN_F13_TOK_JAC s
    JOIN CNEFE_F13_TOK_JAC c
        ON  c.token      = s.token
        AND c.NM_SUBDIST = s.REGIAO_ADM_NORM
    GROUP BY s.NU_NOTIFIC, c.ENDERECO_CNEFE
),

com_jaccard AS (
    SELECT
        c.*,
        ROUND(
            CAST(COALESCE(j.JAC_INTER, 0) AS DOUBLE) /
            NULLIF(
                CAST(ts.N AS DOUBLE)
                + CAST(tc.N AS DOUBLE)
                - CAST(COALESCE(j.JAC_INTER, 0) AS DOUBLE),
            0),
        4) AS JACCARD
    FROM candidatos c
    LEFT JOIN jaccard_inter          j  ON  j.NU_NOTIFIC    = c.NU_NOTIFIC
                                        AND j.ENDERECO_CNEFE = c.ENDERECO_CNEFE
    LEFT JOIN SINAN_F13_TOK_JAC_TOTAL ts ON ts.NU_NOTIFIC    = c.NU_NOTIFIC
    LEFT JOIN CNEFE_F13_TOK_JAC_TOTAL tc ON tc.ENDERECO_CNEFE = c.ENDERECO_CNEFE
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY
                BIGRAMAS_INTERSECAO DESC,
                JACCARD             DESC,
                DIST_NUMERO         ASC NULLS LAST
        ) AS RN
    FROM com_jaccard
)

SELECT
    NU_NOTIFIC, SINAN_LOGRADOURO, SINAN_NUMERO,
    ENDERECO_CNEFE, LATITUDE, LONGITUDE, NM_SUBDIST,
    BIGRAMAS_INTERSECAO, BIGRAMAS_SINAN_TOTAL, JACCARD, DIST_NUMERO,
    'NOMINAL_BIGRAMA' AS FASE_MATCH
FROM ranked WHERE RN = 1;

SELECT COUNT(*) AS match_fase13 FROM MATCH_FASE13;

-- ================================================================
-- FASE 14 — QUADRA_NUMERO SEM FILTRO DE RA
-- Para os ~1.019 com QUADRA + número que não matcharam com RA
-- + IGNORADO com padrão QUADRA (396 casos)
-- Mesma lógica da Fase 8: número mais próximo + conjunto mais próximo
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE14 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE TEM_ID = 0
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE12)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE13)
      -- só os que têm QUADRA ou Q informal com número
      AND (
          REGEXP_MATCHES(UPPER(LOGRADOURO), '^QUADRA\s+\d+')
          OR REGEXP_MATCHES(UPPER(LOGRADOURO), '^Q\s*\d+')
          OR REGEXP_MATCHES(UPPER(LOGRADOURO), '^Q\d+')
      )
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        '^Q\s*0*(\d+)', 'QUADRA \1', 'g'),
        'QUADRA(\d+)', 'QUADRA \1', 'g'),
        'CJUNTO|CONUNTO|COJUNTO', 'CONJUNTO', 'g'),
        'CONJ([0-9])', 'CONJUNTO \1', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')) AS LOGRADOURO_NORM
    FROM nao_matchados
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

-- explode com posição
CREATE OR REPLACE TEMP TABLE SINAN_F14_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND tok.token IN ('QUADRA','CONJUNTO','BLOCO','LOTE')
        THEN s.TOKENS_NORM[tok.pos + 2]
        ELSE s.TOKENS_NORM[tok.pos + 1]
    END AS token_next
FROM SINAN_FASE14 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

-- número da QUADRA no SINAN
CREATE OR REPLACE TEMP TABLE SINAN_F14_QUADRA AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    CAST(s.token_next AS INTEGER) AS NUM_QUADRA_SINAN
FROM SINAN_F14_TOKENS_POS s
WHERE s.token = 'QUADRA'
  AND s.token_next ~ '^[0-9]+$'
  AND s.token_next IS NOT NULL;

-- valor do CONJUNTO no SINAN
CREATE OR REPLACE TEMP TABLE SINAN_F14_CONJ AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.token_next AS CONJ_SINAN
FROM SINAN_F14_TOKENS_POS s
WHERE s.token = 'CONJUNTO'
  AND s.token_next IS NOT NULL
  AND s.token_next != '';

-- candidatos CNEFE: todos com QUADRA + número (filtro de RA aplicado no join)
CREATE OR REPLACE VIEW CNEFE_F14_QUADRA AS SELECT * FROM cnefe.CNEFE_F14_QUADRA;

-- MATCH: número de quadra mais próximo + conjunto mais próximo
CREATE OR REPLACE TEMP TABLE MATCH_FASE14 AS

WITH candidatos AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO,
        s.NU_NUMERO        AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        c.NUM_QUADRA_CNEFE,
        c.CONJ_CNEFE,
        ABS(s.NUM_QUADRA_SINAN - c.NUM_QUADRA_CNEFE) AS DIST_QUADRA
    FROM SINAN_F14_QUADRA s
    JOIN CNEFE_F14_QUADRA c
        ON c.NM_SUBDIST = s.REGIAO_ADM_NORM
        AND ABS(s.NUM_QUADRA_SINAN - c.NUM_QUADRA_CNEFE) <= 5
),

com_scores AS (
    SELECT
        c.*,
        conj.CONJ_SINAN,
        ABS(
            COALESCE(TRY_CAST(c.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(c.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO,
        CASE
            WHEN c.CONJ_CNEFE = conj.CONJ_SINAN THEN 0
            WHEN c.CONJ_CNEFE IS NULL OR conj.CONJ_SINAN IS NULL THEN 999
            WHEN LENGTH(c.CONJ_CNEFE) = 1 AND LENGTH(conj.CONJ_SINAN) = 1
             AND c.CONJ_CNEFE ~ '^[A-Z]$' AND conj.CONJ_SINAN ~ '^[A-Z]$'
                THEN ABS(ASCII(c.CONJ_CNEFE) - ASCII(conj.CONJ_SINAN))
            WHEN TRY_CAST(c.CONJ_CNEFE AS INTEGER) IS NOT NULL
             AND TRY_CAST(conj.CONJ_SINAN AS INTEGER) IS NOT NULL
                THEN ABS(
                    CAST(TRY_CAST(c.CONJ_CNEFE   AS INTEGER) AS INTEGER)
                  - CAST(TRY_CAST(conj.CONJ_SINAN AS INTEGER) AS INTEGER)
                )
            ELSE 50
        END AS DIST_CONJUNTO
    FROM candidatos c
    LEFT JOIN SINAN_F14_CONJ conj ON conj.NU_NOTIFIC = c.NU_NOTIFIC
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY
                DIST_QUADRA   ASC NULLS LAST,
                DIST_CONJUNTO ASC NULLS LAST,
                DIST_NUMERO   ASC NULLS LAST
        ) AS RN
    FROM com_scores
)

SELECT
    NU_NOTIFIC,
    LOGRADOURO    AS SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    DIST_QUADRA,
    DIST_CONJUNTO,
    CONJ_SINAN,
    CONJ_CNEFE,
    DIST_NUMERO,
    CASE
        WHEN DIST_QUADRA = 0 AND DIST_CONJUNTO = 0 THEN 'QUADRA_CONJ_EXATO'
        WHEN DIST_QUADRA = 0                        THEN 'QUADRA_EXATA'
        ELSE                                             'QUADRA_APROX'
    END AS FASE_MATCH
FROM ranked
WHERE RN = 1;

SELECT FASE_MATCH, COUNT(*) AS n FROM MATCH_FASE14 GROUP BY FASE_MATCH ORDER BY n DESC;
SELECT COUNT(*) AS total_fase14 FROM MATCH_FASE14;

-- ================================================================
-- FASE 15 — RUA/AVENIDA SEM FILTRO DE RA (IGNORADO)
-- Para os ~432 RUA/AVENIDA com RA IGNORADO
-- Jaccard sem filtro de RA, threshold mais alto
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE15 AS

SELECT *
FROM SINAN_CORR
WHERE TEM_ID = 0
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE12)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE13)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE14)
  AND UPPER(REGIAO_ADM) = 'IGNORADO'
  AND (
      LOGRADOURO LIKE 'RUA%'
      OR LOGRADOURO LIKE 'AVENIDA%'
      OR LOGRADOURO LIKE 'AV %'
  )
  AND LOGRADOURO IS NOT NULL
  AND TRIM(LOGRADOURO) != '';

-- explode tokens — remove stopwords
CREATE OR REPLACE TEMP TABLE SINAN_F15_TOKENS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    tok.token
FROM SINAN_FASE15 s,
     UNNEST(s.LOGRADOURO_TOKENS) AS tok(token)
WHERE tok.token != ''
  AND tok.token NOT IN (SELECT sw FROM stopwords)
  AND LENGTH(tok.token) > 1;

CREATE OR REPLACE TEMP TABLE SINAN_F15_TOKEN_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS TOKENS_SINAN_TOTAL
FROM SINAN_F15_TOKENS
GROUP BY NU_NOTIFIC;

CREATE OR REPLACE TEMP TABLE MATCH_FASE15 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS TOKENS_INTERSECAO
    FROM SINAN_F15_TOKENS s
    -- sem filtro de RA
    JOIN CNEFE_F12_TOKENS c ON c.token = s.token
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

com_jaccard AS (
    SELECT
        i.*,
        ts.TOKENS_SINAN_TOTAL,
        tc.TOKENS_CNEFE_TOTAL,
        ROUND(
            CAST(i.TOKENS_INTERSECAO AS DOUBLE) /
            NULLIF(
                CAST(ts.TOKENS_SINAN_TOTAL AS DOUBLE)
                + CAST(tc.TOKENS_CNEFE_TOTAL AS DOUBLE)
                - CAST(i.TOKENS_INTERSECAO AS DOUBLE),
            0),
        4) AS JACCARD,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F15_TOKEN_TOTAL  ts ON ts.NU_NOTIFIC     = i.NU_NOTIFIC
    JOIN CNEFE_F12_TOKEN_TOTAL  tc ON tc.ENDERECO_CNEFE = i.ENDERECO_CNEFE
    WHERE i.TOKENS_INTERSECAO >= 2
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY JACCARD DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM com_jaccard
    -- threshold mais alto sem RA para evitar falsos positivos
    WHERE JACCARD >= 0.6
)

SELECT
    NU_NOTIFIC, SINAN_LOGRADOURO, SINAN_NUMERO,
    ENDERECO_CNEFE, LATITUDE, LONGITUDE, NM_SUBDIST,
    TOKENS_INTERSECAO, TOKENS_SINAN_TOTAL, JACCARD, DIST_NUMERO,
    'RUA_IGNORADO' AS FASE_MATCH
FROM ranked WHERE RN = 1;

SELECT COUNT(*) AS total_fase15 FROM MATCH_FASE15;

-- ================================================================
-- FASE 16 — IDs RAROS DO DF
-- QSD, QSC, QMS, SQNW, CCSW, SCLRN, SCLN, ADE, SRL, QRB,
-- ANL, SHR, QC, AR, SMA, EQNP, CSB, CSE, QCS, QSE, SHCA, SHPS
-- Mesma lógica de bigramas das fases 1-8
-- ================================================================

CREATE OR REPLACE VIEW identificadores_raros AS SELECT * FROM cnefe.identificadores_raros;

CREATE OR REPLACE TEMP TABLE SINAN_FASE16 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE8)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE12)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE13)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE14)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE15)
      AND LOGRADOURO IS NOT NULL
      AND TRIM(LOGRADOURO) != ''
),

-- filtra só os que começam com ID raro
com_id_raro AS (
    SELECT *
    FROM nao_matchados
    WHERE SPLIT_PART(UPPER(TRIM(LOGRADOURO)), ' ', 1)
          IN (SELECT id FROM identificadores_raros)
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        'CONJ([0-9])', 'CONJUNTO \1', 'g'),
        '\bCONJ\b', 'CONJUNTO', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')
        ) AS LOGRADOURO_NORM
    FROM com_id_raro
),

tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
)

SELECT * FROM tokenizado;

-- explode com posição
CREATE OR REPLACE TEMP TABLE SINAN_F16_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND (tok.token IN (SELECT id FROM identificadores_raros)
              OR tok.token IN (SELECT id FROM ID_SECUNDARIOS))
        THEN s.TOKENS_NORM[tok.pos + 2]
        ELSE s.TOKENS_NORM[tok.pos + 1]
    END AS token_next
FROM SINAN_FASE16 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

CREATE OR REPLACE TEMP TABLE SINAN_F16_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F16_TOKENS_POS s
WHERE (
    s.token IN (SELECT id FROM identificadores_raros)
    OR s.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND s.token_next IS NOT NULL
AND s.token_next != '';

CREATE OR REPLACE TEMP TABLE SINAN_F16_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F16_BIGRAMAS
GROUP BY NU_NOTIFIC;

-- bigramas CNEFE com IDs raros
CREATE OR REPLACE VIEW CNEFE_BIGRAMAS_RAROS AS SELECT * FROM cnefe.CNEFE_BIGRAMAS_RAROS;

CREATE OR REPLACE TEMP TABLE MATCH_FASE16 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F16_BIGRAMAS s
    JOIN CNEFE_BIGRAMAS_RAROS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
        AND c.NM_SUBDIST    = s.REGIAO_ADM_NORM
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F16_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    WHERE i.BIGRAMAS_INTERSECAO >= CEIL(b.BIGRAMAS_SINAN_TOTAL * 0.75)
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY BIGRAMAS_INTERSECAO DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC, SINAN_LOGRADOURO, SINAN_NUMERO,
    ENDERECO_CNEFE, LATITUDE, LONGITUDE, NM_SUBDIST,
    BIGRAMAS_INTERSECAO, BIGRAMAS_SINAN_TOTAL, DIST_NUMERO,
    'ID_RARO' AS FASE_MATCH
FROM ranked WHERE RN = 1;

SELECT COUNT(*) AS match_fase16 FROM MATCH_FASE16;


-- ================================================================
-- FASE 17 — NORMALIZAÇÃO DE CASOS ESPECIAIS + BIGRAMAS
-- IDs com espaços (Q N N → QNN), prefixos de bairro antes do ID
-- (ARAPOANGA QUADRA 18 → QUADRA 18), IDs com letra colada (QE CH)
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE17 AS

WITH nao_matchados AS (
    SELECT *
    FROM SINAN_CORR
    WHERE NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE8)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE12)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE13)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE14)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE15)
      AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE16)
      AND LOGRADOURO IS NOT NULL
      AND TRIM(LOGRADOURO) != ''
),

norm AS (
    SELECT *,
        TRIM(REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
        REGEXP_REPLACE(
            UPPER(LOGRADOURO),
        -- ID com espaços: Q N N → QNN, Q R → QR, Q A → QA
        '\bQ\s+([A-Z])\s+([A-Z])\s+', 'Q\1\2 ', 'g'),
        '\bQ\s+([A-Z])\s+', 'Q\1 ', 'g'),
        -- prefixo de bairro/setor antes do ID: ARAPOANGA QUADRA → QUADRA
        '^[A-Z]+\s+(QUADRA\s+\d)', '\1', 'g'),
        -- BURITIS N QUADRA → QUADRA (prefixo com número)
        '^[A-Z]+\s+\d+\s+(QUADRA\s+\d)', '\1', 'g'),
        -- SMPW Q N → SMPW + número
        'SMPW\s+Q\s+(\d+)', 'SMPW \1', 'g'),
        -- typos e abreviações de CONJUNTO
        'CJUNTO|CONUNTO|COJUNTO', 'CONJUNTO', 'g'),
        'CONJ([0-9])', 'CONJUNTO \1', 'g'),
        '([0-9]+)([A-Z]{2,})', '\1 \2', 'g'),
        '\s+', ' ', 'g')
        ) AS LOGRADOURO_NORM
    FROM nao_matchados
),

-- re-detecta TEM_ID após normalização
tokenizado AS (
    SELECT *,
        STRING_SPLIT(
            REGEXP_REPLACE(TRIM(LOGRADOURO_NORM), '[^A-Z0-9]+', ' ', 'g'),
            ' '
        ) AS TOKENS_NORM
    FROM norm
),

com_id AS (
    SELECT *,
        CASE
            WHEN EXISTS (
                SELECT 1
                FROM UNNEST(t.TOKENS_NORM) AS tk(tok)
                WHERE tk.tok IN (SELECT id FROM identificadores)
                   OR tk.tok IN (SELECT id FROM identificadores_raros)
            ) THEN 1 ELSE 0
        END AS TEM_ID_NORM
    FROM tokenizado t
)

SELECT * FROM com_id
WHERE TEM_ID_NORM = 1;

-- explode com posição + salto de letra intermediária
CREATE OR REPLACE TEMP TABLE SINAN_F17_TOKENS_POS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO_NORM AS LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.TOKENS_NORM,
    tok.token,
    tok.pos,
    CASE
        WHEN LENGTH(COALESCE(s.TOKENS_NORM[tok.pos + 1], '')) = 1
         AND s.TOKENS_NORM[tok.pos + 1] ~ '^[A-Z]$'
         AND (tok.token IN (SELECT id FROM identificadores)
              OR tok.token IN (SELECT id FROM identificadores_raros)
              OR tok.token IN (SELECT id FROM ID_SECUNDARIOS))
        THEN s.TOKENS_NORM[tok.pos + 2]
        ELSE s.TOKENS_NORM[tok.pos + 1]
    END AS token_next
FROM SINAN_FASE17 s,
     UNNEST(s.TOKENS_NORM) WITH ORDINALITY AS tok(token, pos)
WHERE tok.token != '';

CREATE OR REPLACE TEMP TABLE SINAN_F17_BIGRAMAS AS
SELECT DISTINCT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    s.token      AS bigrama_chave,
    s.token_next AS bigrama_valor
FROM SINAN_F17_TOKENS_POS s
WHERE (
    s.token IN (SELECT id FROM identificadores)
    OR s.token IN (SELECT id FROM identificadores_raros)
    OR s.token IN (SELECT id FROM ID_SECUNDARIOS)
)
AND s.token_next IS NOT NULL
AND s.token_next != ''
AND LENGTH(s.token_next) > 0;

CREATE OR REPLACE TEMP TABLE SINAN_F17_BIGRAMA_TOTAL AS
SELECT NU_NOTIFIC, COUNT(*) AS BIGRAMAS_SINAN_TOTAL
FROM SINAN_F17_BIGRAMAS
GROUP BY NU_NOTIFIC;

CREATE OR REPLACE TEMP TABLE MATCH_FASE17 AS

WITH intersecao AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        COUNT(*)          AS BIGRAMAS_INTERSECAO
    FROM SINAN_F17_BIGRAMAS s
    -- sem filtro de RA para maximizar cobertura
    JOIN CNEFE_BIGRAMAS c
        ON  c.bigrama_chave = s.bigrama_chave
        AND c.bigrama_valor = s.bigrama_valor
    GROUP BY
        s.NU_NOTIFIC, s.LOGRADOURO, s.NU_NUMERO, s.REGIAO_ADM_NORM,
        c.ENDERECO_CNEFE, c.VAL_COMP_ELEM1, c.LATITUDE, c.LONGITUDE, c.NM_SUBDIST
),

candidatos AS (
    SELECT
        i.*,
        b.BIGRAMAS_SINAN_TOTAL,
        ABS(
            COALESCE(TRY_CAST(i.VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(i.SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM intersecao i
    JOIN SINAN_F17_BIGRAMA_TOTAL b ON b.NU_NOTIFIC = i.NU_NOTIFIC
    WHERE i.BIGRAMAS_INTERSECAO >= CEIL(b.BIGRAMAS_SINAN_TOTAL * 0.75)
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY BIGRAMAS_INTERSECAO DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM candidatos
)

SELECT
    NU_NOTIFIC, SINAN_LOGRADOURO, SINAN_NUMERO,
    ENDERECO_CNEFE, LATITUDE, LONGITUDE, NM_SUBDIST,
    BIGRAMAS_INTERSECAO, BIGRAMAS_SINAN_TOTAL, DIST_NUMERO,
    'ID_NORM' AS FASE_MATCH
FROM ranked WHERE RN = 1;

SELECT COUNT(*) AS match_fase17 FROM MATCH_FASE17;


-- ================================================================
-- FASE 18 — JARO SIMILARITY NOS TOKENS
-- Para todos os faltantes após fases 1-17
-- Pré-filtro: token mais longo com Jaro >= 0.88 no CNEFE
-- Score final: média Jaro dos 4 primeiros tokens não-vazios
-- ================================================================

CREATE OR REPLACE TEMP TABLE SINAN_FASE18 AS
SELECT *
FROM SINAN_CORR
WHERE NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE8)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE12)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE13)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE14)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE15)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE16)
  AND NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE17)
  AND LOGRADOURO IS NOT NULL
  AND TRIM(LOGRADOURO) != ''
  -- exclui irresolvíveis semânticos -- ver nao_residencial(), no início.
  -- Era NOT SIMILAR TO com % dos dois lados: no DuckDB o SIMILAR TO é regex
  -- sobre o texto inteiro e o % é literal, então nada era excluído.
  AND NOT nao_residencial(LOGRADOURO);

-- 4 primeiros tokens do SINAN + token mais longo (âncora Jaro)
CREATE OR REPLACE TEMP TABLE SINAN_F18_TOKENS AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO,
    s.NU_NUMERO,
    s.REGIAO_ADM_NORM,
    COALESCE(s.LOGRADOURO_TOKENS[1], '') AS tok1,
    COALESCE(s.LOGRADOURO_TOKENS[2], '') AS tok2,
    COALESCE(s.LOGRADOURO_TOKENS[3], '') AS tok3,
    COALESCE(s.LOGRADOURO_TOKENS[4], '') AS tok4,
    -- token mais longo dos 4 primeiros (mais discriminante para Jaro)
    (SELECT tok
     FROM UNNEST(s.LOGRADOURO_TOKENS[1:4]) AS t(tok)
     WHERE tok IS NOT NULL AND LENGTH(tok) > 2
     ORDER BY LENGTH(tok) DESC
     LIMIT 1
    ) AS TOKEN_LONGO,
    -- primeiro token numérico (âncora exata de número)
    COALESCE(
        CASE WHEN s.LOGRADOURO_TOKENS[1] ~ '^[0-9]+$' THEN s.LOGRADOURO_TOKENS[1] END,
        CASE WHEN s.LOGRADOURO_TOKENS[2] ~ '^[0-9]+$' THEN s.LOGRADOURO_TOKENS[2] END,
        CASE WHEN s.LOGRADOURO_TOKENS[3] ~ '^[0-9]+$' THEN s.LOGRADOURO_TOKENS[3] END,
        CASE WHEN s.LOGRADOURO_TOKENS[4] ~ '^[0-9]+$' THEN s.LOGRADOURO_TOKENS[4] END
    ) AS TOKEN_NUMERICO
FROM SINAN_FASE18 s;

-- 4 primeiros tokens do CNEFE
CREATE OR REPLACE VIEW CNEFE_F18_TOKENS AS SELECT * FROM cnefe.CNEFE_F18_TOKENS;

CREATE OR REPLACE TEMP TABLE MATCH_FASE18 AS

WITH pre_filtro AS (
    SELECT
        s.NU_NOTIFIC,
        s.LOGRADOURO      AS SINAN_LOGRADOURO,
        s.NU_NUMERO       AS SINAN_NUMERO,
        s.REGIAO_ADM_NORM,
        s.tok1 AS S_TOK1, s.tok2 AS S_TOK2,
        s.tok3 AS S_TOK3, s.tok4 AS S_TOK4,
        c.ENDERECO_CNEFE,
        c.VAL_COMP_ELEM1,
        c.LATITUDE,
        c.LONGITUDE,
        c.NM_SUBDIST,
        c.tok1 AS C_TOK1, c.tok2 AS C_TOK2,
        c.tok3 AS C_TOK3, c.tok4 AS C_TOK4
    FROM SINAN_F18_TOKENS s
    JOIN CNEFE_F18_TOKENS c
        -- pré-filtro 1: âncora numérica exata quando disponível
        ON (
            (s.TOKEN_NUMERICO IS NOT NULL
             AND c.TOKEN_NUMERICO IS NOT NULL
             AND s.TOKEN_NUMERICO = c.TOKEN_NUMERICO)
        )
        -- pré-filtro 2: token mais longo com Jaro alto no tok1 do CNEFE
        AND (
            s.TOKEN_LONGO IS NULL
            OR jaro_similarity(s.TOKEN_LONGO, c.tok1) >= 0.82
            OR jaro_similarity(s.TOKEN_LONGO, c.tok2) >= 0.82
        )
),

com_jaro AS (
    SELECT
        p.*,
        -- Jaro médio ponderado: tok1 tem peso maior (mais discriminante)
        (
            jaro_similarity(S_TOK1, C_TOK1) * 2 +
            jaro_similarity(S_TOK2, C_TOK2) +
            jaro_similarity(S_TOK3, C_TOK3) +
            jaro_similarity(S_TOK4, C_TOK4)
        ) / 5.0 AS JARO_PONDERADO,
        ABS(
            COALESCE(TRY_CAST(VAL_COMP_ELEM1 AS INTEGER), 0)
            - COALESCE(TRY_CAST(SINAN_NUMERO  AS INTEGER), 0)
        ) AS DIST_NUMERO
    FROM pre_filtro p
),

ranked AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY NU_NOTIFIC
            ORDER BY JARO_PONDERADO DESC, DIST_NUMERO ASC NULLS LAST
        ) AS RN
    FROM com_jaro
    WHERE JARO_PONDERADO >= 0.65
)

SELECT
    NU_NOTIFIC,
    SINAN_LOGRADOURO,
    SINAN_NUMERO,
    ENDERECO_CNEFE,
    LATITUDE,
    LONGITUDE,
    NM_SUBDIST,
    JARO_PONDERADO,
    DIST_NUMERO,
    'JARO_TOKENS' AS FASE_MATCH
FROM ranked
WHERE RN = 1;

SELECT COUNT(*) AS match_fase18 FROM MATCH_FASE18;
SELECT ROUND(AVG(JARO_PONDERADO), 4) AS jaro_medio FROM MATCH_FASE18;

-- ================================================================
-- FASE FINAL — CENTROIDE DA RA COMO FALLBACK
-- Para todos sem match após fases 1-18
-- Usa a RA do SINAN para buscar o centroide
-- ================================================================

-- Calcular centroides por RA a partir do CNEFE
CREATE OR REPLACE VIEW CENTROIDE_RA AS SELECT * FROM cnefe.CENTROIDE_RA;

-- Fallback para sem match com RA conhecida
CREATE OR REPLACE TEMP TABLE MATCH_CENTROIDE AS
SELECT
    s.NU_NOTIFIC,
    s.LOGRADOURO  AS SINAN_LOGRADOURO,
    s.NU_NUMERO   AS SINAN_NUMERO,
    NULL::VARCHAR          AS ENDERECO_CNEFE,
    cr.LAT_CENTROIDE AS LATITUDE,
    cr.LON_CENTROIDE AS LONGITUDE,
    cr.NM_SUBDIST,
    'CENTROIDE_RA' AS FASE_MATCH
FROM SINAN_CORR s
JOIN CENTROIDE_RA cr
    ON cr.NM_SUBDIST = s.REGIAO_ADM_NORM
WHERE s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE8)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE12)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE13)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE14)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE15)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE16)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE17)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE18)
  AND UPPER(s.REGIAO_ADM_NORM) != 'IGNORADO';

SELECT COUNT(*) AS fallback_com_ra    FROM MATCH_CENTROIDE;

-- diagnóstico: quantos ficam sem nada (IGNORADO sem match)
SELECT COUNT(*) AS sem_georref_ignorado
FROM SINAN s
WHERE s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_POSICIONAL)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE2)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE3)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE4)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE5)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE6)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE7)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE8)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE9)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE10)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE11)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE12)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE13)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE14)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE15)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE16)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE17)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_FASE18)
  AND s.NU_NOTIFIC NOT IN (SELECT NU_NOTIFIC FROM MATCH_CENTROIDE);

-- ================================================================
-- GEORREF_COMPLETO FINAL — inclui centroide como fallback
-- ================================================================

CREATE OR REPLACE TEMP TABLE GEORREF_COMPLETO AS

SELECT
    s.NU_NOTIFIC,
    s.NM_LOGRADO,
    s.NM_COMPLEM,
    s.REGIAO_ADM,
    s.LOGRADOURO        AS SINAN_LOGRADOURO,
    s.NU_NUMERO         AS SINAN_NUMERO,
    s.TEM_ID,
    COALESCE(f1.ENDERECO_CNEFE,
             f2.ENDERECO_CNEFE,
             f3.ENDERECO_CNEFE,
             f4.ENDERECO_CNEFE,
             f5.ENDERECO_CNEFE,
             f6.ENDERECO_CNEFE,
             f7.ENDERECO_CNEFE,
             f8.ENDERECO_CNEFE,
             f9.ENDERECO_CNEFE,
             f10.ENDERECO_CNEFE,
             f11.ENDERECO_CNEFE,
             f12.ENDERECO_CNEFE,
             f13.ENDERECO_CNEFE,
             f14.ENDERECO_CNEFE,
             f15.ENDERECO_CNEFE,
             f16.ENDERECO_CNEFE,
             f17.ENDERECO_CNEFE,
             f18.ENDERECO_CNEFE,
             fc.ENDERECO_CNEFE)  AS ENDERECO_CNEFE,
    COALESCE(f1.LATITUDE,
             f2.LATITUDE,
             f3.LATITUDE,
             f4.LATITUDE,
             f5.LATITUDE,
             f6.LATITUDE,
             f7.LATITUDE,
             f8.LATITUDE,
             f9.LATITUDE,
             f10.LATITUDE,
             f11.LATITUDE,
             f12.LATITUDE,
             f13.LATITUDE,
             f14.LATITUDE,
             f15.LATITUDE,
             f16.LATITUDE,
             f17.LATITUDE,
             f18.LATITUDE,
             fc.LATITUDE)        AS LATITUDE,
    COALESCE(f1.LONGITUDE,
             f2.LONGITUDE,
             f3.LONGITUDE,
             f4.LONGITUDE,
             f5.LONGITUDE,
             f6.LONGITUDE,
             f7.LONGITUDE,
             f8.LONGITUDE,
             f9.LONGITUDE,
             f10.LONGITUDE,
             f11.LONGITUDE,
             f12.LONGITUDE,
             f13.LONGITUDE,
             f14.LONGITUDE,
             f15.LONGITUDE,
             f16.LONGITUDE,
             f17.LONGITUDE,
             f18.LONGITUDE,
             fc.LONGITUDE)       AS LONGITUDE,
    COALESCE(f1.NM_SUBDIST,
             f2.NM_SUBDIST,
             f3.NM_SUBDIST,
             f4.NM_SUBDIST,
             f5.NM_SUBDIST,
             f6.NM_SUBDIST,
             f7.NM_SUBDIST,
             f8.NM_SUBDIST,
             f9.NM_SUBDIST,
             f10.NM_SUBDIST,
             f11.NM_SUBDIST,
             f12.NM_SUBDIST,
             f13.NM_SUBDIST,
             f14.NM_SUBDIST,
             f15.NM_SUBDIST,
             f16.NM_SUBDIST,
             f17.NM_SUBDIST,
             f18.NM_SUBDIST,
             fc.NM_SUBDIST)      AS NM_SUBDIST,
    COALESCE(f1.FASE_MATCH,
             f2.FASE_MATCH,
             f3.FASE_MATCH,
             f4.FASE_MATCH,
             f5.FASE_MATCH,
             f6.FASE_MATCH,
             f7.FASE_MATCH,
             f8.FASE_MATCH,
             f9.FASE_MATCH,
             f10.FASE_MATCH,
             f11.FASE_MATCH,
             f12.FASE_MATCH,
             f13.FASE_MATCH,
             f14.FASE_MATCH,
             f15.FASE_MATCH,
             f16.FASE_MATCH,
             f17.FASE_MATCH,
             f18.FASE_MATCH,
             fc.FASE_MATCH)      AS FASE_MATCH
FROM SINAN s
LEFT JOIN MATCH_POSICIONAL f1  ON f1.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE2      f2  ON f2.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE3      f3  ON f3.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE4      f4  ON f4.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE5      f5  ON f5.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE6      f6  ON f6.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE7      f7  ON f7.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE8      f8  ON f8.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE9      f9  ON f9.NU_NOTIFIC  = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE10     f10 ON f10.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE11     f11 ON f11.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE12     f12 ON f12.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE13     f13 ON f13.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE14     f14 ON f14.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE15     f15 ON f15.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE16     f16 ON f16.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE17     f17 ON f17.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_FASE18     f18 ON f18.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN MATCH_CENTROIDE  fc  ON fc.NU_NOTIFIC  = s.NU_NOTIFIC
WHERE COALESCE(f1.NU_NOTIFIC,  f2.NU_NOTIFIC,  f3.NU_NOTIFIC,
               f4.NU_NOTIFIC,  f5.NU_NOTIFIC,  f6.NU_NOTIFIC,
               f7.NU_NOTIFIC,  f8.NU_NOTIFIC,  f9.NU_NOTIFIC,
               f10.NU_NOTIFIC, f11.NU_NOTIFIC, f12.NU_NOTIFIC,
               f13.NU_NOTIFIC, f14.NU_NOTIFIC, f15.NU_NOTIFIC,
               f16.NU_NOTIFIC, f17.NU_NOTIFIC, f18.NU_NOTIFIC,
               fc.NU_NOTIFIC) IS NOT NULL;

-- resumo final
SELECT FASE_MATCH, COUNT(*) AS n
FROM GEORREF_COMPLETO
GROUP BY FASE_MATCH
ORDER BY n DESC;

-- ================================================================
-- EXTRAÇÃO FINAL
-- ================================================================

-- ----------------------------------------------------------------
-- 1. SINAN_GEO
-- SINAN completo + colunas de georreferenciamento
-- ----------------------------------------------------------------

CREATE OR REPLACE TEMP TABLE SINAN_GEO AS

SELECT
    s.*,
    g.ENDERECO_CNEFE,
    g.LATITUDE,
    g.LONGITUDE,
    g.NM_SUBDIST,
    g.FASE_MATCH,
    -- CD_SUBDIST e CD_SETOR via JOIN com CNEFEDF pelo endereço matchado
    c.CD_SUBDIST,
    c.COD_SETOR                              AS CD_SETOR,
    -- Estado intermediario, usado no R para achar quem recebe o centroide do
    -- DF. O valor final (0/1) vem de classificar_georreferenciado(), em
    -- georreferenciamento_funcoes.R.
    CASE
        WHEN g.NU_NOTIFIC IS NULL            THEN 0  -- sem match
        WHEN g.FASE_MATCH = 'CENTROIDE_RA'  THEN 2  -- match aproximado (centroide)
        ELSE 1                                        -- georreferenciado
    END                                      AS GEORREFERENCIADO
FROM SINAN_TEMP s
LEFT JOIN GEORREF_COMPLETO g
    ON g.NU_NOTIFIC = s.NU_NOTIFIC
LEFT JOIN CNEFEDF c
    ON  c.ENDERECO_CNEFE = g.ENDERECO_CNEFE
    AND c.NM_SUBDIST     = g.NM_SUBDIST
QUALIFY ROW_NUMBER() OVER (
    PARTITION BY s.NU_NOTIFIC
    ORDER BY c.ROWID
) = 1;

-- ----------------------------------------------------------------
-- 2. GEORREF_FALHA
-- Apenas os casos sem nenhum match no pipeline
-- ----------------------------------------------------------------

CREATE OR REPLACE TEMP TABLE GEORREF_FALHA AS

SELECT s.*
FROM SINAN s
WHERE s.NU_NOTIFIC NOT IN (
    SELECT DISTINCT NU_NOTIFIC FROM GEORREF_COMPLETO
);

-- ----------------------------------------------------------------
-- 3. RESUMO DE COBERTURA
-- ----------------------------------------------------------------

SELECT
    GEORREFERENCIADO,
    CASE GEORREFERENCIADO
        WHEN 1 THEN 'Georreferenciado (endereço)'
        WHEN 2 THEN 'Georreferenciado (centroide RA)'
        WHEN 0 THEN 'Não georreferenciado'
    END                          AS DESCRICAO,
    COUNT(*)                     AS N,
    ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS PCT
FROM SINAN_GEO
GROUP BY GEORREFERENCIADO
ORDER BY GEORREFERENCIADO DESC;