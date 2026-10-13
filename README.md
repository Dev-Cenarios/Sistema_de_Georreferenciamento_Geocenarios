# Sistema de Georreferenciamento — SINAN × CNEFE

Este repositório contém o painel R/Shiny e o pipeline de georreferenciamento que associam casos notificados ao SINAN (Sistema de Informação de Agravos de Notificação) a um endereço geolocalizado da base CNEFE (Cadastro Nacional de Endereços para Fins Estatísticos, IBGE) e ao respectivo setor censitário. O sistema atende ao Distrito Federal — via um pipeline determinístico próprio, otimizado para o volume e a formatação de endereços da base do DF — e às demais 21 Unidades Federativas do menu (AL, AM, BA, CE, ES, GO, MA, MG, MS, MT, PB, PE, PR, RJ, RN, RO, RS, SC, SE, SP e TO), via record linkage probabilístico ou geocodificação com o pacote `geocodebr`. É voltado a pesquisadores e gestores de vigilância epidemiológica que precisam transformar bases de notificação em dados espacialmente analisáveis.

### [▶ Acessar o sistema](https://cenarios.unb.br/sistema-de-geocodificacao-de-dados-de-saude/)

O sistema está em funcionamento na página do Programa Cenários, onde pode ser usado direto no navegador. Os arquivos enviados são processados na sessão e não ficam guardados (ver [Fonte de dados](#3-fonte-de-dados)).

> A saída deste sistema — a coordenada de cada caso e as colunas `GEORREFERENCIADO` e `INDICADOR` — alimenta os painéis [SINAN Territórios: Recife-PE](https://github.com/Dev-Cenarios/SINAN_Territorios_Recife_PE_TB) e [SINAN Territórios: Hanseníase – Pernambuco](https://github.com/Dev-Cenarios/SINAN_Territorios_Pernambuco_Hansen). Veja também o [Sistema de Linkage SINAN × SIM](https://github.com/Dev-Cenarios/Sistema_de_Linkage_SINANxSIM).

## Sobre o Projeto

O Programa Cenários, coordenado pelo Prof. Dr. Walter Massa Ramalho da Universidade de Brasília (UnB), é uma iniciativa científica e tecnológica que reúne pesquisadores, gestores de saúde e parceiros nacionais e internacionais para desenvolver soluções inovadoras no enfrentamento de doenças determinadas socialmente. Atualmente, o programa é composto por dois grandes eixos de pesquisa:

### 1. Hanseníase – Modelagem de Cenários de Risco

Cenários Hanseníase tem como objetivo identificar áreas de risco e cadeias invisíveis de transmissão da hanseníase no Brasil, com foco especial nas regiões metropolitanas de Brasília (DF), Recife (PE), Palmas (TO) e demais capitais. A proposta integra inteligência artificial generativa, georreferenciamento por setores censitários e rastreamento de contatos. Os resultados incluem um dashboard nacional, algoritmos de análise espacial e temporal, e relatórios técnicos para subsidiar ações estratégicas do SUS, com foco na redução de desigualdades e modernização da vigilância.

### 2. Tuberculose – Cenários de Risco e Determinantes Sociais

Cenários TB visa apoiar a eliminação da tuberculose como problema de saúde pública no Brasil e em países lusófonos africanos e latino-americanos. A pesquisa combina modelagem epidemiológica, vinculação de bases de dados (linkage) e estratificação de cenários para avaliar a situação dos municípios e orientar ações prioritárias. Entre as entregas previstas estão toolkits com árvores de decisão e IA, dashboards interativos, matrizes de indicadores e metodologias de microplanejamento integradas à atenção primária. Também serão promovidas parcerias com municípios e capacitações para gestores de saúde.

Este sistema — o **Sistema de Georreferenciamento SINAN × CNEFE** — é uma das entregas do eixo Tuberculose, com foco em associar cada caso notificado a um endereço geolocalizado e ao seu setor censitário correspondente, para as bases do Distrito Federal e das demais Unidades Federativas monitoradas pelo programa.

[Saiba mais sobre o projeto!](https://cenarios.unb.br/tuberculose-cenarios-de-risco-e-determinantes-sociais/)

### Abordagem Comum

Ambos os projetos compartilham princípios e tecnologias:

* Uso de dados sociais, econômicos e epidemiológicos para subsidiar decisões.
* Georreferenciamento de casos para detalhamento espacial.
* Análise temporal para identificar tendências e rupturas.
* Ferramentas digitais abertas para garantir transparência e replicabilidade.
* Interação com gestores e oficinas de validação para alinhar resultados às necessidades locais.

Essa atuação integrada fortalece o SUS, promove equidade em saúde e posiciona o Brasil na vanguarda da aplicação de ciência de dados e inteligência artificial em saúde pública.

## Sumário

1. [Funcionalidades](#1-funcionalidades)
2. [Estrutura do projeto](#2-estrutura-do-projeto)
3. [Fonte de dados](#3-fonte-de-dados)
4. [Tecnologias e pré-requisitos](#4-tecnologias-e-pré-requisitos)
5. [Instalação](#5-instalação)
6. [Troubleshooting](#6-troubleshooting)
7. [Desenvolvedores](#7-desenvolvedores)
8. [Licença](#licença)

## 1. Funcionalidades

- **Upload de arquivos** — CSV e Excel de até 500 MB, com padronização automática para arquivos no formato SINAN ou mapeamento manual de colunas para outros formatos de entrada.
- **Georreferenciamento** — pipeline determinístico de 18 fases para o DF, ou record linkage probabilístico (5 fases, 21 subfases) / geocodificação via `geocodebr` para as demais UFs. ([detalhamento](docs/DOCUMENTACAO_FASES_MATCHING.md))
- **Resumo do Processamento** — painel textual com método utilizado, escopo processado, tempo de execução, total de registros processados e número de casos geocodificados por similaridade.
- **Métricas Detalhadas** — distribuição de casos por município/RA e por fase de match; cobertura do georreferenciamento (DF); qualidade e tipo de endereço (demais UFs).
- **Gráficos** — precisão posicional dos casos (número exato, aproximado, início do logradouro, centroide), cobertura acumulada ao longo das fases, resultado por município/RA informado e casos por setor censitário; nas demais UFs, também distância do número nos matches aproximados, município divergente e similaridade das fases probabilísticas.
- **Mapa de Coordenadas** — visualização cartográfica interativa (pontos ou densidade de kernel/KDE), separando posição por endereço de centroide (fallback); o mapa pode ser exportado em PNG como está na tela.
- **Triagem de Endereços** — revisão manual, caso a caso, de registros com baixa similaridade (indisponível para o DF, cujo pipeline é determinístico e não gera candidatos alternativos rankeados).
- **Download dos resultados** — exportação dos dados processados, das métricas e do resultado da triagem. Cada caso sai com as colunas `GEORREFERENCIADO` (1 = posição de endereço; 0 = centroide, sem coordenada ou endereço vazio), `INDICADOR` (0 = detento ou pessoa em situação de rua; 1 = demais) e `PRECISAO_POSICIONAL` (onde o ponto caiu: número exato, aproximado, início do logradouro, centroide etc.; útil para filtrar os casos de centroide) — ver [Colunas de classificação](docs/DOCUMENTACAO_FASES_MATCHING.md#colunas-de-classificação-do-resultado).

Para o critério exato de cada fase de match (o que precisa ser verdade para o caso ser considerado um match, o rótulo gravado no resultado e a referência exata no código), veja a **[Documentação das Fases de Matching](docs/DOCUMENTACAO_FASES_MATCHING.md)**.

## 2. Estrutura do projeto

```text
.
├── app.R                            # Painel Shiny: UI, upload, disparo do georreferenciamento e visualizações
├── georreferenciamento_funcoes.R    # Pipeline do DF (split_sql_rapido(), georreferenciar_DF(), georreferenciar_DF_batches(), ponto fixo e centroide de polígono), fallback por centroide das demais UFs e classificação GEORREFERENCIADO/INDICADOR
├── graficos_funcoes.R               # Precisão posicional de cada caso e os gráficos da aba Gráficos
├── GEOCODING_DF_CORRIDO.sql         # Script SQL do pipeline de 18 fases do DF
├── PREP_CNEFE.sql                   # Pré-processamento das tabelas estáticas do CNEFE do DF (roda uma única vez)
├── PREP_CNEFE_SERVER.R              # Executa o PREP_CNEFE.sql contra o CNEFE.duckdb de produção, com validação de schema
├── PREP_RUAS_DISTINTAS.R            # Cria as tabelas auxiliares CNEFE<UF>_RUAS (ruas distintas por município) da fase 4 das demais UFs — opcional
├── Shapes UFs/                      # Malhas de RAs (DF) e municípios (demais UFs): contorno do mapa e fallback por centroide
├── cenarios_logo_full.jpeg          # Logo exibido no painel
├── docs/
│   └── DOCUMENTACAO_FASES_MATCHING.md   # Critério fase a fase do georreferenciamento (DF e demais UFs), com referência ao código
├── LICENSE, LICENSE.md              # Licença MIT
└── README.md
```

O repositório não versiona o banco `CNEFE.duckdb` (arquivo de vários GB, mantido apenas no ambiente de produção) nem arquivos de dados de entrada (`*.csv`, `*.rds`) — ver `.gitignore`.

## 3. Fonte de dados

- **SINAN** (Sistema de Informação de Agravos de Notificação): casos notificados, por UF. Upload manual pelo usuário do painel (CSV/Excel); não há conexão automática com o sistema oficial.
- **CNEFE** (Cadastro Nacional de Endereços para Fins Estatísticos, IBGE): base de endereços de referência usada na geocodificação, com uma tabela por UF (`CNEFEDF`, `CNEFEPE`, `CNEFEGO` etc.) em um único banco DuckDB (`CNEFE.duckdb`), incluindo coordenadas, setor censitário e município de cada endereço cadastral.
- **Malha geográfica de Regiões Administrativas (DF) e municípios (demais UFs)**: shapefiles em `Shapes UFs/`, usados no contorno do mapa e no fallback dos casos sem match, que recebem o centroide do polígono da RA ou do DF, do município ou da UF. Este repositório versiona as malhas do DF e de 17 UFs; as de AM, MA, PR e RO ficam só nos ambientes de teste e produção.

Por se tratar de dados de notificação de agravo de saúde (potencialmente sensíveis/identificáveis), o processamento é feito localmente — os arquivos enviados pelo usuário não são retidos além da sessão do painel.

## 4. Tecnologias e pré-requisitos

- **Linguagem**: [R](https://www.r-project.org/)
- **Framework**: [Shiny](https://shiny.posit.co/)
- **Pacotes R** (carregados via `pacman::p_load()` no início do `app.R`, com instalação automática se ausentes): `shiny`, `shinythemes`, `dplyr`, `ggplot2`, `sf`, `stringi`, `enderecobr`, `DBI`, `duckdb`, `DT`, `shinyjs`, `readr`, `stringr`, `leaflet`, `leaflet.extras`, `leaflet.extras2` (exportação do mapa em PNG), `geocodebr`. Usados também em pontos específicos do código, sem `library()` explícito: `data.table`, `readxl`, `base64enc`, `bit64` (códigos IBGE que chegam do DuckDB como `integer64`).
- **Banco de dados**: [DuckDB](https://duckdb.org/) — arquivo local `CNEFE.duckdb`, não incluído no repositório (ver seção 5).
- **Pré-requisito de ambiente**: o `CNEFE.duckdb` de produção precisa conter as tabelas estáticas do pipeline do DF já materializadas — ver `PREP_CNEFE_SERVER.R` na seção 5 — e a tabela `CNEFE<UF>` de cada UF processada. As tabelas `CNEFE<UF>_RUAS` (`PREP_RUAS_DISTINTAS.R`) são opcionais: sem elas, a fase 4 das demais UFs monta uma versão temporária a cada execução, mais lenta em municípios grandes. O método GeocodeBr exige ainda o arquivo `SC_<UF>.rds` (setores censitários da UF) na pasta do painel, também não versionado.
- Não há `DESCRIPTION`, `renv.lock`, `Dockerfile` ou arquivo de lockfile de dependências neste repositório — as versões exatas de cada pacote não são fixadas.

## 5. Instalação

```r
# 1. Clonar o repositório
git clone https://github.com/Dev-Cenarios/Sistema_de_Georreferenciamento_Geocenarios.git
setwd("Sistema_de_Georreferenciamento_Geocenarios")

# 2. Garantir que o CNEFE.duckdb de produção está nesta mesma pasta
#    (arquivo não versionado — obter separadamente com o time do projeto)

# 3. Rodar a preparação do CNEFE do DF (uma única vez, ou sempre que a
#    base CNEFEDF for atualizada)
source("PREP_CNEFE_SERVER.R")

# 4. Reiniciar a sessão R (libera a conexão de escrita aberta pelo passo 3)

# 5. Rodar o painel
#    As dependências de pacman::p_load() no topo do app.R são instaladas
#    automaticamente na primeira execução, se ausentes.
shiny::runApp()
```

## 6. Troubleshooting

| Sintoma | Causa provável | Solução |
|---|---|---|
| `IO Error: Cannot open file "CNEFE.duckdb": O arquivo já está sendo usado por outro processo` | Uma conexão de escrita anterior ao `CNEFE.duckdb` (ex.: `PREP_CNEFE_SERVER.R` interrompido, ou rodado fora do `on.exit()` que o protege) não foi liberada na sessão R atual. | Reiniciar a sessão R (Session → Restart R) para liberar o lock no nível do sistema operacional. |
| Processamento do DF falha ou trava com bases grandes (dezenas de milhares de registros) | Lotes grandes demais para a memória disponível em `georreferenciar_DF_batches()`. | Reduzir o parâmetro `batch_size` (padrão 3.000) na chamada da função. |
| Processamento do DF interrompido no meio de uma execução em lotes | Comportamento esperado — não é um erro a corrigir. | Rodar `georreferenciar_DF_batches()` novamente com o mesmo `temp_dir`: lotes já salvos em disco são detectados e não reprocessados. |
| Aviso "O arquivo enviado já tinha as colunas ..." ao processar uma UF fora do DF | O arquivo enviado é uma saída anterior do painel e já traz colunas que o processamento grava no resultado (`rank`, `LATITUDE`, `FASE`, `ENDERECO_CNEFE` etc.). | Nada a fazer: elas são removidas antes de reprocessar. Sem isso, `rank` derrubava a consolidação com `Ambiguous reference to column name "rank"` e as demais colidiam em silêncio. No DF essa remoção não existe — envie o arquivo sem essas colunas. |
| Casos sem logradouro (só bairro) aparecem como posição de endereço, e não como `Centroide`, nas demais UFs | A fase 5 não compara casos sem logradouro, mas a **fase 4 ainda pode casá-los** quando o nome do bairro faz parte do nome de uma rua do CNEFE (ex.: bairro `MUSTARDINHA` → `RUA DA MUSTARDINHA`). Limitação conhecida — ver a fase 4 na documentação. | Filtrar o resultado por `NM_LOGRADO` vazio e conferir `FASE`/`PRECISAO_POSICIONAL` desses casos. |

## 7. Desenvolvedores

- **Eduardo Schneider Arruda** ([@schneiderweisse](https://github.com/schneiderweisse)) — colaborador do Projeto Cenários, Engenheiro de Computação. Trabalha à frente do georreferenciamento que é usado para o painel. Contato: [geoprocenarios@gmail.com](mailto:geoprocenarios@gmail.com)

## Licença

[MIT](LICENSE.md) — © 2026 Projeto Cenários.
