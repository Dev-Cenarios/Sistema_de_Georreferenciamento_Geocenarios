# Documentação das Fases de Matching — Sistema de Georreferenciamento

Este documento explica **como cada fase do georreferenciamento decide se um endereço notificado corresponde a um endereço da base CNEFE, e onde essa lógica está implementada no código**. É destinado a quem precisa entender a metodologia por trás da cobertura/qualidade de match reportada pelo painel, sem necessariamente ler SQL ou R.

Um caso só é avaliado por uma fase se **não obteve match em nenhuma das fases anteriores** — as fases são cascatas de tentativa, da mais determinística/confiável para a mais aproximada.

Cada item segue o mesmo padrão:

1. **Critério de match** — o que precisa ser verdade para o caso ser considerado um match nessa fase.
2. **Rótulo gravado** — o valor de `FASE_MATCH`/`FASE` que aparece no resultado final e nas Métricas Detalhadas.
3. **Código** — arquivo e linha de referência.

> **Sobre os números de linha.** Eles valem para o estado do código na data da última atualização deste documento e envelhecem a cada edição dos arquivos. Cada referência traz também uma âncora que dá para buscar no código: nas fases do DF, o comentário `-- FASE N —` e o rótulo literal (ex.: `'JARO_TOKENS'`); nas demais UFs, o comentário `# --- A.1.1:` (e equivalentes) e o rótulo `'FASE A.1.1'`. Se a linha não bater, procure pela âncora.

---

## Distrito Federal (18 fases + ponto fixo + fallback)

Implementado em [`GEOCODING_DF_CORRIDO.sql`](../GEOCODING_DF_CORRIDO.sql), executado por [`georreferenciar_DF()`](../georreferenciamento_funcoes.R) (em lotes de 3.000 casos, por `georreferenciar_DF_batches()`).

**Nome da RA usado nos filtros.** Antes das fases, o nome da Região Administrativa informado é normalizado (`REGIAO_ADM_NORM`, tabela `SINAN_CORR`, `GEOCODING_DF_CORRIDO.sql:176`): `Riacho Fundo I` → `Riacho Fundo` (nome do CNEFE); `Arapoanga` → `Planaltina` e `Água Quente` → `Recanto das Emas` (RAs criadas depois da malha do Censo 2022 — o CNEFE ainda registra os endereços delas na RA de origem). Só os filtros de RA e o centroide usam o nome normalizado; a coluna `Regiao_adm` do resultado mantém o nome informado.

**Endereços não residenciais.** As fases 13 e 18 não avaliam endereços de presídio, situação de rua, albergue e similares (macro `nao_residencial()`, `GEOCODING_DF_CORRIDO.sql:44`, com os mesmos termos de `REGEX_DETENTO`/`REGEX_SITUACAO_RUA` do R). Esses casos seguem para o ponto fixo ou o centroide.

### Fase 1

- **Critério de match**: bigramas de identificador + valor idênticos ao CNEFE (ex.: `QNM` → `12`), com filtro de Região Administrativa (RA); em caso de mais de um candidato, desempate pela menor distância de número.
- **Rótulo gravado**: `POSICIONAL`
- **Código**: `GEOCODING_DF_CORRIDO.sql:205` (início da fase), `:314` (rótulo)

### Fase 2

- **Critério de match**: mesmo critério da fase 1, com normalização adicional do bigrama (variações e abreviações não capturadas na fase 1); ainda com filtro de RA.
- **Rótulo gravado**: `POSICIONAL_NORM`
- **Código**: `GEOCODING_DF_CORRIDO.sql:322`, `:493`

### Fase 3

- **Critério de match**: trata a letra intermediária no identificador de quadra que o CNEFE não registra (ex.: candidato de `QN 8` para um SINAN informado como `QN 8 F`).
- **Rótulo gravado**: `SUBFIXO_LETRA`
- **Código**: `GEOCODING_DF_CORRIDO.sql:500`, `:658`

### Fase 4

- **Critério de match**: endereços com identificador secundário `LOTE`, mas sem `CONJUNTO` explícito informado.
- **Rótulo gravado**: `SO_LOTE`
- **Código**: `GEOCODING_DF_CORRIDO.sql:666`, `:802`

### Fase 5

- **Critério de match**: bigrama do identificador primário isolado (sem exigir bigrama de conjunto), com bônus de pontuação se o conjunto também bater.
- **Rótulo gravado**: `QUADRA_APROX`
- **Código**: `GEOCODING_DF_CORRIDO.sql:810`, `:966`

### Fase 6

- **Critério de match**: mesma lógica das fases 1–5, **sem exigir RA correta** — para casos em que a RA foi registrada como "Ignorado".
- **Rótulo gravado**: `IGNORADO`
- **Código**: `GEOCODING_DF_CORRIDO.sql:974`, `:1108`

### Fase 7

- **Critério de match**: sem filtro de RA; hierarquiza candidatos pela distância do conjunto informado.
- **Rótulo gravado**: `CONJ_EXATO_APROX` (conjunto idêntico), `CONJ_PROXIMO_APROX` (conjunto próximo), ou `QUADRA_APROX` (só a quadra bate) — conforme a distância do conjunto.
- **Código**: `GEOCODING_DF_CORRIDO.sql:1116` (início da fase), `:1320–1324` (`CASE WHEN` que define o rótulo)

### Fase 8

- **Critério de match**: sem filtro de RA; candidato de quadra numericamente mais próxima, hierarquizado por exatidão de quadra e de conjunto.
- **Rótulo gravado**: `QUADRA_EXATA_CONJ_EXATO`, `QUADRA_EXATA_CONJ_APROX` ou `QUADRA_APROX_CONJ_APROX`, conforme a combinação de exatidão.
- **Código**: `GEOCODING_DF_CORRIDO.sql:1331`, `:1521–1525`

### Fase 9

- **Critério de match**: identificadores fora da lista principal do DF (lista estendida de siglas), sem estrutura reconhecida nas fases 1–8.
- **Rótulo gravado**: `ID_EXT`
- **Código**: `GEOCODING_DF_CORRIDO.sql:1535`, `:1690`

### Fase 10

- **Critério de match**: interseção de bigramas com tokens estruturais (`QUADRA`, `CONJUNTO`, `BLOCO`, `LOTE`, `CHACARA`, `RUA`, `VIA`).
- **Rótulo gravado**: `QUADRA_NOMINAL`
- **Código**: `GEOCODING_DF_CORRIDO.sql:1697`, `:1849`

### Fase 11

- **Critério de match**: bigramas específicos de chácara/lote/conjunto/bloco.
- **Rótulo gravado**: `CHACARA`
- **Código**: `GEOCODING_DF_CORRIDO.sql:1856`, `:1994`

### Fase 12

- **Critério de match**: similaridade de Jaccard entre os conjuntos de tokens do logradouro (SINAN × CNEFE), restrito a `RUA`/`AVENIDA`, limiar ≥ 0,4.
- **Rótulo gravado**: `RUA_AVENIDA`
- **Código**: `GEOCODING_DF_CORRIDO.sql:2000`, `:2135`

### Fase 13

- **Critério de match**: bigramas de tokens estruturais como âncora principal, com Jaccard de tokens como critério de desempate (não há limiar numérico de Jaccard nesta fase). Só avalia casos com RA informada e pula endereços não residenciais (`nao_residencial()`).
- **Rótulo gravado**: `NOMINAL_BIGRAMA`
- **Código**: `GEOCODING_DF_CORRIDO.sql:2141`, `:2349`; a exclusão de endereços não residenciais está em `:2162`

### Fase 14

- **Critério de match**: candidato de quadra por proximidade numérica (distância ≤ 5), **sem filtro de RA** — esta é a fase que causava OOM (estouro de memória) antes da correção que restringiu o join por distância a evitar produto cartesiano descontrolado.
- **Rótulo gravado**: `QUADRA_CONJ_EXATO`, `QUADRA_EXATA` ou `QUADRA_APROX`, conforme exatidão de quadra e conjunto.
- **Código**: `GEOCODING_DF_CORRIDO.sql:2355`, `:2530–2534`

### Fase 15

- **Critério de match**: mesma lógica da fase 12 (Jaccard de tokens), sem filtro de RA, limiar ≥ 0,6. Como a fase 6, só avalia casos com RA "Ignorado".
- **Rótulo gravado**: `RUA_IGNORADO`
- **Código**: `GEOCODING_DF_CORRIDO.sql:2542`, `:2644`

### Fase 16

- **Critério de match**: lista de identificadores raros do DF (ex.: `QSD`, `QSC`, `QMS`), bigramas exatos.
- **Rótulo gravado**: `ID_RARO`
- **Código**: `GEOCODING_DF_CORRIDO.sql:2650`, `:2812`

### Fase 17

- **Critério de match**: normalização de casos especiais — identificadores com espaços indevidos (ex.: "Q N N" → `QNN`) ou prefixo de bairro antes do identificador.
- **Rótulo gravado**: `ID_NORM`
- **Código**: `GEOCODING_DF_CORRIDO.sql:2819`, `:3001`

### Fase 18

- **Critério de match**: similaridade de Jaro (`jaro_similarity` — não Jaro-Winkler) entre os 4 primeiros tokens do logradouro, ponderada (o 1º token pesa o dobro: `(2·t1 + t2 + t3 + t4) / 5`), limiar ≥ 0,65. Só considera candidatos do CNEFE com o **mesmo token numérico** do endereço notificado e, como pré-filtro, exige que o token mais longo do endereço tenha Jaro ≥ 0,82 com um dos dois primeiros tokens do candidato. **Não filtra por RA** e pula endereços não residenciais (`nao_residencial()`). **Única fase do pipeline do DF que usa similaridade de string caractere-a-caractere** — todas as anteriores usam match exato de bigramas/tokens, Jaccard de tokens ou distância numérica.
- **Rótulo gravado**: `JARO_TOKENS`
- **Código**: `GEOCODING_DF_CORRIDO.sql:3008`, `:3141`; a exclusão de endereços não residenciais está em `:3039`

### Ponto fixo do complexo penitenciário (não é uma fase de match)

- **Critério**: caso **sem match de endereço** — sem coordenada, ou só com o centroide da RA — cujo logradouro ou complemento cita presídio/penitenciária (`REGEX_DETENTO`, em palavra inteira: `PAPUDA`, `PRESIDIO`, `PENITENCIARIA`, `PRISIONAL`, `CDP`, `CPP`, `CIR`, `PDF` etc.). Tem **prioridade sobre o centroide da RA**: a posição do presídio é mais precisa que o centro da RA informada. Um caso que casou numa das 18 fases fica no endereço casado, não é redirecionado.
- **Resultado**: coordenada fixa (−15,8647; −47,8203), que cai na RA Jardim Botânico (conferido na malha), `GEORREFERENCIADO = 1`. Não recebe setor censitário.
- **Rótulo gravado**: `INSTITUCIONAL_FIXO`
- **Código**: `aplicar_ponto_fixo_DF()`, em `georreferenciamento_funcoes.R:132`, chamada por `georreferenciar_DF()` antes de `aplicar_centroides_DF()` (`:524`)

### Fallback (não é uma fase de match)

- **Critério**: casos sem match em nenhuma das 18 fases (e sem ponto fixo) recebem o centroide do polígono da RA informada no SINAN (malha `Shapes UFs/DF_subdistritos_CD2022.shp`) ou, se a RA for desconhecida, do polígono do DF inteiro. Quando o centroide cai fora do próprio polígono (ex.: Lago Sul), usa-se um ponto garantidamente interno (`st_point_on_surface`). Esses casos não recebem setor censitário.
- **Rótulo gravado**: `CENTROIDE_RA` ou `CENTROIDE_DF` (ambos com `GEORREFERENCIADO = 0` — ver [Colunas de classificação](#colunas-de-classificação-do-resultado))
- **Código**: `GEOCODING_DF_CORRIDO.sql:3149` escolhe a RA; `aplicar_centroides_DF()`, em `georreferenciamento_funcoes.R:156`, atribui a coordenada do polígono

---

## Demais UFs — as 21 UFs do menu além do DF (5 fases, 21 subfases)

Vale para AL, AM, BA, CE, ES, GO, MA, MG, MS, MT, PB, PE, PR, RJ, RN, RO, RS, SC, SE, SP e TO. Implementado em [`app.R`](../app.R), na função `georreferenciar_UF()` (`app.R:2446`), contra a tabela `CNEFE<UF>` do `CNEFE.duckdb` (ex.: `CNEFEPE`). Cada fase é aplicada primeiro aos casos **com** município informado (subfase `A`) e, para quem sobrou sem match, repetida nos casos **sem** município (subfase `B`, ou `G` na fase 5), sem exigir correspondência territorial — por isso praticamente toda subfase `A.X` tem uma equivalente `B.X`.

**Limite de similaridade.** As fases 4 e 5 só aceitam um candidato com similaridade ≥ ao limite escolhido no painel (padrão 0,5). As fases 1 a 3 são determinísticas e não usam o limite.

**Como o endereço é montado** (`etl_sinan()`, `app.R:2361`, e `padronizar_colunas_usuario()`, `app.R:1405`, para arquivos com colunas declaradas pelo usuário):

| Coluna | Composição | Fase que usa |
|---|---|---|
| `address` | logradouro + número + bairro | 1 e 5 |
| `address3` | logradouro + bairro (sem número) | 2 e 4 |
| `address2` | logradouro + número (sem bairro) | nenhuma fase atual |

Se o logradouro estiver vazio, o complemento ocupa o lugar dele. As partes ausentes são simplesmente omitidas (`juntar_endereco()`, `app.R:1267`): antes, uma parte ausente virava o texto `"NA"` dentro do endereço (ex.: `NA, S/N, MUSTARDINHA`), que passava a ser comparado como se fosse um endereço real. A fase 3 compara `NM_LOGRADO` direto, sem usar essas colunas.

### Fase 1 — determinístico, endereço completo idêntico

- **Critério comum**: `ADDRESS == ENDERECO_CNEFE` (string idêntica, endereço completo com número).
- `A.1.1` — com o município informado batendo com o do candidato. **Código**: `app.R:2780`
- `A.1.2` — endereço idêntico existente em **apenas 1** município na base CNEFE; aceito mesmo com o município informado divergente (desambiguação segura, pois não há outro candidato possível). **Código**: `app.R:2809`
- `B.1` — equivalente a `A.1.1`/`A.1.2`, sem exigir município (casos "Ignorado"). **Código**: `app.R:2855`

### Fase 2 — contingência de número

- **Critério comum**: `ADDRESS3 == ENDERECO_CNEFE3` (logradouro + bairro idênticos, ignorando o número do imóvel).
- `A.2.1` — número informado no SINAN → escolhe o candidato de número mais próximo; similaridade penalizada proporcionalmente à distância numérica. **Código**: `app.R:2931`
- `A.2.2` — sem número informado → âncora no candidato com número 0 ou 1 do logradouro. **Código**: `app.R:2975`
- `B.2.1` / `B.2.2` — equivalentes a `A.2.1`/`A.2.2`, sem exigir município. **Código**: `app.R:3055`, `app.R:3098`

### Fase 3 — determinístico, nome do logradouro idêntico

- **Critério comum**: `NM_LOGRADO == LOGR_CNEFE`, restrito a logradouros que existem em apenas 1 bairro do município na base CNEFE (evita ambiguidade).
- `A.3.1` — número exato. **Código**: `app.R:3218`
- `A.3.2` — aproximação numérica (candidato de número mais próximo). **Código**: `app.R:3245`
- `A.3.3` — sem número informado → âncora no candidato com número 0 ou 1. **Código**: `app.R:3288`
- `B.3.1` / `B.3.2` / `B.3.3` — equivalentes, sem exigir município. **Código**: `app.R:3387`, `app.R:3413`, `app.R:3455`

### Fase 4 — probabilístico (Jaro-Winkler), endereço sem número

Processada **em lote**: um único pipeline SQL trata todos os casos pendentes de uma vez, na função `fase4_lote()` (`app.R:3597`), chamada uma vez para a subfase `A` (`app.R:3827`) e uma para a `B` (`app.R:3910`). Sequência:

1. **Bloqueio por 5-gramas.** Os candidatos de cada caso são as ruas distintas do CNEFE (do município informado na `A`; da UF inteira na `B`) que compartilham **pelo menos 2 substrings de 5 caracteres** com o `address3` do caso. É um pré-filtro que tolera erro de digitação e descarta buscas sem sobreposição real de texto.
2. **Match de rua.** Só sobre os candidatos, calcula a similaridade Jaro-Winkler entre `address3` e `ENDERECO_CNEFE3`. Vence a rua de maior similaridade (empate: ordem alfabética); só é aceita se ≥ limite.
3. **Âncora do número**, sobre a rua vencedora:
   - `4.1` — o número informado existe na rua → número exato. **Código**: `app.R:3706`
   - `4.2` — número informado, mas inexistente na rua → o mais próximo; a similaridade é reduzida em 0,0003 a cada 5 números de distância, e a diferença fica em `DISTANCIA_NUM`. **Código**: `app.R:3722`
   - `4.3` — sem número (vazio, `S/N`, `NA`) → âncora no número 0 ou 1 da rua. **Código**: `app.R:3746`
- **Rótulos gravados**: `FASE A.4.1`, `A.4.2`, `A.4.3` (com município) e `FASE B.4.1`, `B.4.2`, `B.4.3` (sem município).
- Para a Triagem, guarda até 14 ruas candidatas por caso (`rank < 15`), cada uma com a casa de menor número como representante.
- **Limitação conhecida — casos sem logradouro.** Ao contrário da fase 5, a fase 4 **não** pula casos com `NM_LOGRADO` vazio, e o bloqueio por 5-gramas só barra os que não compartilham texto nenhum com o CNEFE. Um caso só com bairro passa se o nome do bairro fizer parte do nome de uma rua: `address3 = MUSTARDINHA` gera como candidatas `RUA DA MUSTARDINHA` (similaridade ≈ 0,75, acima do limite padrão) e `RUA DO NASCIMENTO, MUSTARDINHA`, e pode casar mesmo sem nenhuma rua informada. Para dimensionar no resultado: `table(base$FASE[is.na(base$NM_LOGRADO) | trimws(base$NM_LOGRADO) == ""])`.

### Fase 5 — probabilístico (Jaro-Winkler), endereço completo

- **Critério comum**: similaridade Jaro-Winkler entre `address` (endereço completo, com número e bairro) e `ENDERECO_CNEFE`, aceita se ≥ limite. Ao contrário da fase 4, é processada **caso a caso** (um loop em R, uma consulta por caso) e **sem pré-filtro por 5-gramas**. Última fase de match: quem não casa aqui vai para o fallback por centroide.
- **Casos sem logradouro são pulados.** Se `NM_LOGRADO` está vazio, o `address` só tem número e bairro, e a similaridade de algo como `S/N, MUSTARDINHA` contra as ruas do mesmo bairro passa de 0,5 por coincidência de caracteres — o caso "casava" com uma rua qualquer. Esses casos não são comparados na fase 5 e seguem para o fallback — desde que a fase 4 também não os tenha resolvido (ver a limitação da fase 4).
- `A.5` — com município informado (candidatos: só o município). **Código**: loop em `app.R:4044`, exclusão de casos sem logradouro em `:4075`
- `G.5` — geral, sem filtro territorial (candidatos: o CNEFE da UF inteira), para todos os casos que ainda restam, inclusive os com município que a `A.5` não resolveu — última tentativa de todo o pipeline. **Código**: loop em `app.R:4272`, exclusão em `:4299`

As fases 4 e 5 (comparação Jaro-Winkler) também alimentam a lista de candidatos rankeados usada na aba de Triagem de Endereços do painel (até 15 por caso na fase 5).

### Fallback (não é uma fase de match)

- **Critério**: casos sem match em nenhuma fase recebem o centroide do polígono do município informado (`MUNIRESAT`, ligado ao `CD_MUN` da malha municipal da UF em `Shapes UFs/`) ou, se o município estiver ausente, "Ignorado" ou fora da malha, do polígono da UF inteira. Quando o centroide cai fora do próprio polígono (ex.: Vitória, Angra dos Reis), usa-se um ponto garantidamente interno (`st_point_on_surface`). Esses casos não recebem setor censitário. Sem malha da UF na pasta, os casos seguem sem coordenada.
- **Rótulo gravado**: `CENTROIDE_MUN` ou `CENTROIDE_UF` (ambos com `GEORREFERENCIADO = 0`)
- **Código**: `aplicar_fallback_centroide_UF()`, em `georreferenciamento_funcoes.R:201`, chamada ao fim de `georreferenciar_UF()` (`app.R:4655`)

---

## Colunas de classificação do resultado

Todo caso do arquivo sai com as três colunas abaixo, no DF e nas demais UFs (método de match com o CNEFE; o GeocodeBr não as grava). A regra é a mesma nos dois pipelines e fica em `classificar_georreferenciado()` (`georreferenciamento_funcoes.R:290`), `classificar_indicador()` (`:305`) e `classificar_precisao()` (`graficos_funcoes.R:60`).

### `GEORREFERENCIADO` — o caso tem posição de endereço?

| Valor | Quando |
|---|---|
| `1` | Casou numa fase de endereço: determinística, ou probabilística com similaridade ≥ limite. Nas demais UFs, o limite escolhido no painel (padrão 0,5) já é exigido dentro das fases 4 e 5 — abaixo dele a fase não casa e o caso segue para o centroide. No DF valem os limites fixos do SQL (fases 12, 15 e 18; a 13 usa Jaccard só para desempate) e as 18 fases contam como 1, assim como o ponto fixo do complexo penitenciário (`INSTITUCIONAL_FIXO`). |
| `0` | Centroide de qualquer tipo (`CENTROIDE_RA`, `CENTROIDE_DF`, `CENTROIDE_MUN`, `CENTROIDE_UF`), caso sem coordenada, ou endereço vazio — logradouro e complemento em branco ou só com "SEM ENDERECO", "NAO INFORMADO", "IGNORADO". |

A decisão vem do rótulo da fase, e não de recalcular `similarity ≥ limite` sobre a coluna final: na fase 4.2 a similaridade é reduzida pela distância do número depois do corte, e um caso aceito com 0,50 aparece como 0,494. Na Triagem de Endereços, um caso revisto cujo candidato escolhido fica abaixo do limite do processamento passa a `0`.

### `INDICADOR` — o endereço informado é de residência?

| Valor | Quando |
|---|---|
| `0` | Detento ou pessoa em situação de rua. |
| `1` | Os demais — todo caso tem ao menos a referência da UF processada. |

- **Detento**: `POP_LIBER` = Sim na ficha do SINAN, ou o endereço (logradouro, complemento, bairro ou endereço completo) cita presídio/penitenciária — `PAPUDA`, `PRESIDIO`, `PENITENCIARIA`, `PRISIONAL`, `CDP`, `CPP`, `CIR`, `PDF` etc., sempre como palavra inteira (`REGEX_DETENTO`, `georreferenciamento_funcoes.R:263`).
- **Situação de rua**: `POP_RUA` = Sim, ou o endereço diz "situação de rua", "morador de rua", albergue etc. (`REGEX_SITUACAO_RUA`, `:267`).

Os campos `POP_LIBER` e `POP_RUA` existem na ficha de tuberculose do SINAN; sem eles (outras fichas, planilhas próprias), só o texto do endereço decide. O `INDICADOR` é independente do `GEORREFERENCIADO`: um detento cujo endereço casou numa fase continua com `GEORREFERENCIADO = 1`.

### `PRECISAO_POSICIONAL` — onde o ponto caiu

Diz a precisão da posição de cada caso a partir do rótulo da fase que o resolveu (a similaridade não serve para isso: as fases determinísticas gravam `similarity = 1` mesmo quando o caso não tinha número e caiu no início do logradouro). Fica nos dados baixados, para filtrar os casos de centroide na análise, e alimenta as cores do mapa e dos gráficos (azul = endereço, laranja = centroide, cinza = sem coordenada).

| Demais UFs | Rótulos de fase |
|---|---|
| Número exato | `1.x`, `3.1`, `4.1`; e `2.1`, `3.2`, `4.2` com `DISTANCIA_NUM = 0` |
| Número aproximado | `2.1`, `3.2`, `4.2` com `DISTANCIA_NUM > 0` |
| Endereço por similaridade | fase 5 (`A.5`, `G.5`) |
| Início do logradouro | `2.2`, `3.3`, `4.3` (sem número, âncora no 0/1) |
| Centroide do município / da UF | `CENTROIDE_MUN` / `CENTROIDE_UF` |

| DF | Rótulos de fase |
|---|---|
| Endereço | rótulos de match das 18 fases |
| Endereço aproximado | rótulos terminados em `APROX` (quadra/conjunto aproximados) |
| Local fixo (instituição) | `INSTITUCIONAL_FIXO` |
| Centroide da RA / do DF | `CENTROIDE_RA` / `CENTROIDE_DF` |

Nos dois pipelines, um caso sem coordenada é "Sem coordenada", e um rótulo que a regra não reconhece, "Não classificado".

### Reprocessar um arquivo que já foi processado (demais UFs)

Se o arquivo enviado já traz colunas que o painel grava no resultado — `ADDRESS_CORE`, `ENDERECO_CNEFE`, `similarity`, `DISTANCIA_NUM`, `FASE`, `LATITUDE`, `LONGITUDE`, `DSC_LOCALIDADE`, `COD_SETOR`, `NM_MUN_CNEFE`, `CD_MUN_CNEFE` e `rank` (comum ao reenviar uma saída anterior do painel) —, elas são **removidas antes do processamento**, com um aviso no painel. Sem isso, `rank` derrubava a consolidação final com `Ambiguous reference to column name "rank"`, e as demais colidiam sem erro: o DuckDB renomeava a segunda ocorrência para `<coluna>_1`, e o painel seguia lendo o valor antigo sob o nome original. `CD_MUN`/`NM_MUN` do usuário não são removidos (o do CNEFE já sai com alias próprio). Código: `colunas_reservadas_finalgeo`, em `app.R:4569`.

Essa proteção existe só no pipeline das demais UFs; no DF, envie o arquivo sem essas colunas.
