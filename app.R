if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  shiny,
  shinythemes,
  dplyr,
  ggplot2,
  sf,
  stringi,
  enderecobr,
  DBI,
  duckdb,
  DT,
  shinyjs,
  readr,
  stringr, 
  leaflet,
  leaflet.extras,
  leaflet.extras2,
  geocodebr
)

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(sf)
library(enderecobr)
library(DBI)
library(duckdb)
library(DT)
library(shinyjs)
library(readr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)   # exportação do mapa em PNG (easyPrint)
library(geocodebr)
library(stringi)

# Pipeline de georreferenciamento DF (18 fases, CNEFE pré-processado, batching
# automático) — define georreferenciar_DF() e georreferenciar_DF_batches().
# Carregado uma única vez no escopo global do app (não depende de `input`
# reativo, diferente da função antiga que substituiu).
source("georreferenciamento_funcoes.R")

# Precisão posicional de cada caso e os gráficos da aba Gráficos.
source("graficos_funcoes.R")

# Configurar opções para aumentar o tamanho máximo de upload
options(shiny.maxRequestSize = 500*1024^2) # 500MB

# Concatena texto e tags SEM o espaço que o htmltools insere entre elementos
# irmãos ao renderizar. Usado nos textos da aba Início: passar uma pontuação
# como elemento seguinte a uma tag produzia "Arquivo SINAN , as colunas" e
# "no menu à esquerda ." no HTML final. Texto solto é escapado normalmente;
# tags são renderizadas e coladas sem separador.
inline <- function(...) {
  partes <- lapply(list(...), function(x) {
    if (inherits(x, c("shiny.tag", "shiny.tag.list", "html"))) {
      as.character(x)
    } else {
      htmltools::htmlEscape(as.character(x))
    }
  })
  HTML(paste0(unlist(partes), collapse = ""))
}

# Interface do usuário
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Adicionar a logo no canto superior direito
  tags$head(
    tags$style(HTML("
      .header-container {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        width: 100%;
        position: relative;
        margin-bottom: 20px;
        padding-top: 10px;
      }
      .logo-container {
        position: absolute;
        top: 10px;
        right: 10px;
        z-index: 1000;
      }
      .logo-img {
        height: 100px;
        width: auto;
        max-width: 400px;
      }
      .title-container {
        flex-grow: 1;
        padding-right: 220px; /* Espaço para a logo */
      }
      .file-input-info {
        font-size: 12px;
        color: #666;
        margin-top: 5px;
      }
      .file-size-warning {
        color: #e74c3c;
        font-weight: bold;
      }
      .progress-bar {
        height: 20px;
      }
      .shiny-output-error {
        color: #e74c3c;
        font-weight: bold;
      }
      .download-buttons {
        margin-top: 20px;
        padding-top: 20px;
        border-top: 1px solid #eee;
      }
      .metricas-table {
        font-size: 12px;
        margin-top: 10px;
      }
      .side-by-side {
        display: flex;
        gap: 20px;
        margin-bottom: 20px;
      }
      .side-by-side > div {
        flex: 1;
      }
      .scrollable-table {
        max-height: 400px;
        overflow-y: auto;
        border: 1px solid #ddd;
        padding: 10px;
      }
      .table-header {
        background-color: #f8f9fa;
        padding: 10px;
        border-bottom: 1px solid #ddd;
        font-weight: bold;
      }
      .method-title {
        font-size: 16px;
        font-weight: bold;
        color: #2c3e50;
        margin-bottom: 15px;
        padding: 10px;
        background-color: #f8f9fa;
        border-radius: 5px;
        border-left: 4px solid #3498db;
      }
      /* Estilos para a aba de triagem */
      .selected-row {
        background-color: #2ecc71 !important;
        color: white !important;
      }
      .unselected-row {
        background-color: #e74c3c !important;
        color: white !important;
      }
      .address-block {
        border-top: 3px solid #2c3e50 !important;
        font-weight: bold;
      }
      .triagem-controls {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
        border: 1px solid #ddd;
      }
      .threshold-selector {
        margin-bottom: 15px;
      }
      .action-buttons {
        display: flex;
        gap: 10px;
        margin-top: 20px;
      }
      /* Estilos da aba Início (apresentação do sistema) */
      .intro-wrapper {
        max-width: 1020px;
        padding-bottom: 20px;
      }
      .intro-hero {
        background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
        color: #ffffff;
        padding: 26px 30px;
        border-radius: 6px;
        margin-bottom: 28px;
      }
      .intro-hero h2 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #ffffff;
        font-weight: 600;
      }
      .intro-lead {
        font-size: 15px;
        line-height: 1.65;
        color: #ecf0f1;
        margin-bottom: 18px;
      }
      .intro-tags {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
      }
      .intro-tag {
        background-color: rgba(255, 255, 255, 0.15);
        border: 1px solid rgba(255, 255, 255, 0.28);
        padding: 4px 13px;
        border-radius: 20px;
        font-size: 12px;
        white-space: nowrap;
      }
      .intro-section {
        margin-bottom: 32px;
      }
      .intro-section h3 {
        color: #2c3e50;
        font-size: 19px;
        font-weight: 600;
        border-bottom: 2px solid #3498db;
        padding-bottom: 8px;
        margin-top: 0;
        margin-bottom: 16px;
      }
      .intro-section p {
        line-height: 1.68;
        color: #454545;
      }
      .intro-cards {
        display: flex;
        flex-wrap: wrap;
        gap: 16px;
      }
      .intro-card {
        flex: 1 1 290px;
        background-color: #f8f9fa;
        border: 1px solid #ddd;
        border-left: 4px solid #3498db;
        border-radius: 4px;
        padding: 16px 18px;
      }
      .intro-card.card-df {
        border-left-color: #2ecc71;
      }
      .intro-card.card-geocodebr {
        border-left-color: #f39c12;
      }
      .intro-card h4 {
        margin-top: 0;
        margin-bottom: 4px;
        color: #2c3e50;
        font-size: 15px;
        font-weight: 600;
      }
      .intro-card .card-sub {
        font-size: 12px;
        color: #7f8c8d;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 10px;
      }
      .intro-card ul {
        font-size: 13px;
        padding-left: 18px;
        margin-bottom: 0;
      }
      .intro-card li {
        margin-bottom: 5px;
        line-height: 1.5;
      }
      .step-list {
        counter-reset: step;
        list-style: none;
        padding-left: 0;
        margin-bottom: 0;
      }
      .step-list > li {
        counter-increment: step;
        position: relative;
        padding-left: 46px;
        margin-bottom: 15px;
        line-height: 1.6;
        color: #454545;
      }
      .step-list > li::before {
        content: counter(step);
        position: absolute;
        left: 0;
        top: -2px;
        width: 30px;
        height: 30px;
        background-color: #3498db;
        color: #ffffff;
        border-radius: 50%;
        text-align: center;
        line-height: 30px;
        font-weight: bold;
        font-size: 14px;
      }
      .step-list > li strong {
        color: #2c3e50;
      }
      .intro-note {
        background-color: #fef9e7;
        border-left: 4px solid #f39c12;
        padding: 13px 17px;
        border-radius: 4px;
        font-size: 13px;
        line-height: 1.65;
        color: #454545;
      }
      .intro-source {
        margin-bottom: 15px;
        line-height: 1.6;
        color: #454545;
      }
      .intro-source strong {
        color: #2c3e50;
      }
      .intro-links {
        list-style: none;
        padding-left: 0;
      }
      .intro-links li {
        margin-bottom: 11px;
        line-height: 1.55;
      }
      .intro-links a {
        color: #2980b9;
        font-weight: 500;
      }
      .intro-links .link-desc {
        display: block;
        font-size: 12px;
        color: #7f8c8d;
      }
      /* Aba Gráficos: indicadores e seções */
      .kpi-row {
        display: flex;
        flex-wrap: wrap;
        gap: 12px;
        margin: 6px 0 26px 0;
      }
      .kpi-tile {
        flex: 1 1 170px;
        background-color: #ffffff;
        border: 1px solid #e1e0d9;
        border-radius: 6px;
        padding: 12px 16px;
      }
      .kpi-rotulo {
        font-size: 13px;
        color: #52514e;
        margin-bottom: 4px;
      }
      .kpi-marca {
        display: inline-block;
        width: 10px;
        height: 10px;
        border-radius: 2px;
        margin-right: 6px;
        vertical-align: baseline;
      }
      .kpi-valor {
        font-size: 28px;
        font-weight: 600;
        color: #0b0b0b;
        line-height: 1.2;
      }
      .kpi-detalhe {
        font-size: 12px;
        color: #898781;
        margin-top: 2px;
      }
      .grafico-secao {
        margin-bottom: 30px;
      }
      .grafico-secao h4 {
        color: #2c3e50;
        font-weight: 600;
        margin-bottom: 4px;
      }
      .grafico-descricao {
        font-size: 13px;
        color: #52514e;
        line-height: 1.55;
        margin-bottom: 8px;
        max-width: 860px;
      }
      .grafico-secao details {
        margin-top: 6px;
        font-size: 13px;
      }
      .grafico-secao summary {
        cursor: pointer;
        color: #2980b9;
      }
      .mapa-exportar {
        display: flex;
        align-items: center;
        gap: 12px;
        margin-bottom: 10px;
      }
      .mapa-exportar .nota {
        font-size: 12px;
        color: #898781;
      }
    "))
  ),
  
  # Inicializar shinyjs
  useShinyjs(),
  
  div(class = "header-container",
      div(class = "title-container",
          titlePanel("Sistema de Georreferenciamento") # TÍTULO
      ),
      div(class = "logo-container",
          uiOutput("logo_image")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Título do método fixo (removida a caixa de seleção)
      #div(class = "method-title", "Método: Match Probabilístico"), # 1
      
      selectInput(
        "metodo",
        "Escolha o método de processamento:",
        choices = c("Match probabilístico" = "cnefe", "GeocodeBr" = "geocodebr"),
        selected = "cnefe"
      ),
      
      # Upload do arquivo da doença
      fileInput(
        "arquivo_doenca", # 2
        HTML("Carregar arquivo da doença (CSV):<br/>
             <span class='file-input-info'>Tamanho máximo: 500MB</span>"),
        accept = c(".csv", ".txt", ".xlsx", ".xls"),
        buttonLabel = "Procurar...",
        placeholder = "Nenhum arquivo selecionado"
      ),
      
      # Escolha do tipo de arquivo / escopo das colunas de endereço
      radioButtons(
        "escopo_arquivo",
        "Formato do arquivo:",
        choices = c(
          "Arquivo SINAN" = "SINAN",
          "Declarar colunas de endereço" = "OUTRO"
        ),
        selected = "SINAN"
      ),
      
      # Se NÃO for SINAN, abrir inputs para declarar colunas
      conditionalPanel(
        condition = "input.escopo_arquivo == 'OUTRO'",
        
        textInput(
          "usu_cod_unico",
          "Coluna de código único:",
          value = "",
          placeholder = "Ex: NU_NOTIFIC"
        ),
        
        textInput(
          "usu_logr",
          "Coluna de logradouro:",
          value = "",
          placeholder = "Ex: NM_LOGRADO"
        ),
        
        textInput(
          "usu_num",
          "Coluna de número:",
          value = "",
          placeholder = "Ex: NU_NUMERO"
        ),
        
        textInput(
          "usu_mun",
          "Coluna de município:",
          value = "",
          placeholder = "Ex: MUNIRESAT"
        ),

        textInput(
          "usu_bairro",
          "Coluna de bairro (opcional):",
          value = "",
          placeholder = "Ex: NM_BAIRRO"
        ),

        textInput(
          "usu_ra",
          "Coluna de RA:",
          value = "",
          placeholder = "Ex: ID_DISTRIT"
        ),
        
        textInput(
          "usu_compl",
          "Coluna de complemento (opcional):",
          value = "",
          placeholder = "Ex: NM_COMPLEM"
        ),
        
        textInput(
          "usu_address",
          "Coluna de endereço completo (opcional):",
          value = "",
          placeholder = "Ex: address"
        )
      ),
      
      # Seleção de UF
      selectInput(
        "uf",
        "Selecione a UF:",
        choices = c("AL", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT",
                    "PB", "PE", "PR", "RJ", "RN", "RO", "RS", "SC", "SE", "SP",
                    "TO"), # 3
        selected = "DF"
      ),
      
      # Seleção de escopo (UF inteira ou município específico)
      radioButtons(
        "escopo",
        "Escopo do processamento:",
        choices = c("UF Inteira" = "uf", "Município Específico" = "municipio"), # 4
        selected = "uf"
      ),
      
      conditionalPanel(
        condition = "input.escopo == 'municipio'",
        textInput(
          "codigo_municipio",
          "Código do Município (6 dígitos):", # 6
          value = "",
          placeholder = "Ex: 530010"
        )
      ),
      
      # Informações sobre os arquivos carregados
      uiOutput("info_arquivos"),
      
      # Parâmetros adicionais para CNEFE
      sliderInput(
        "similarity_threshold",
        "Limite de similaridade:", # 7
        min = 0.1,
        max = 1.0,
        value = 0.5,
        step = 0.1
      ),
      
      actionButton(
        "processar",
        "Processar Dados",
        icon = icon("play"), # 8
        class = "btn-primary"
      )
    ),
    
    mainPanel(
      width = 9,
      # id nomeado para permitir a troca programática de aba pelo servidor
      # (updateTabsetPanel) — usado para levar o usuário ao Resumo assim que o
      # georreferenciamento termina. Cada tabPanel abaixo tem um `value`
      # estável para ser referenciado por nome, independente do rótulo exibido.
      tabsetPanel(
        id = "abas_principais",
        selected = "inicio",

        tabPanel(
          "Início",
          value = "inicio",
          div(
            class = "intro-wrapper",

            # ---------------- Apresentação ----------------
            div(
              class = "intro-hero",
              h2("Enriquecimento de dados usando o CNEFE"),
              p(
                class = "intro-lead",
                "Este painel associa cada caso notificado ao SINAN (Sistema de Informação de ",
                "Agravos de Notificação) a um endereço geolocalizado da base CNEFE (Cadastro ",
                "Nacional de Endereços para Fins Estatísticos, IBGE) e ao respectivo setor ",
                "censitário — transformando bases de notificação em dados espacialmente ",
                "analisáveis para a vigilância epidemiológica."
              ),
              div(
                class = "intro-tags",
                span(class = "intro-tag", "Programa Cenários — UnB"),
                span(class = "intro-tag", "Eixo Tuberculose"),
                span(class = "intro-tag", "22 Unidades Federativas"),
                span(class = "intro-tag", "Processamento local")
              )
            ),

            # ---------------- Passo a passo ----------------
            div(
              class = "intro-section",
              h3("Passo a passo"),
              p("Todos os controles de configuração ficam no menu à esquerda."),
              tags$ol(
                class = "step-list",
                tags$li(
                  strong("Escolha o método de processamento."), br(),
                  "Match probabilístico usa a base CNEFE local; GeocodeBr usa o pacote ",
                  inline(code("geocodebr"), ". Para o Distrito Federal, o match probabilístico "),
                  "aciona automaticamente o pipeline determinístico de 18 fases."
                ),
                tags$li(
                  strong("Carregue o arquivo da base de notificações."), br(),
                  "Formatos aceitos: CSV, TXT, XLSX e XLS, com até 500 MB."
                ),
                tags$li(
                  strong("Informe o formato do arquivo."), br(),
                  "Em ",
                  inline(em("Arquivo SINAN"), ", as colunas são padronizadas automaticamente. "),
                  "Em ",
                  inline(em("Declarar colunas de endereço"), ", indique quais colunas do seu "),
                  "arquivo correspondem a código único, logradouro, número, município, bairro, ",
                  "RA e complemento — útil para bases que não seguem o layout do SINAN."
                ),
                tags$li(
                  strong("Selecione a UF."), br(),
                  "Define qual tabela do CNEFE será consultada e qual pipeline será executado. ",
                  "Em arquivos no formato SINAN, a UF também é detectada automaticamente a ",
                  "partir da coluna ", inline(code("SG_UF"), ".")
                ),
                tags$li(
                  strong("Defina o escopo do processamento."), br(),
                  "A UF inteira, ou um município específico pelo código IBGE de 6 dígitos."
                ),
                tags$li(
                  strong("Ajuste o limite de similaridade."), br(),
                  "Valor mínimo de similaridade para que um match probabilístico seja aceito ",
                  "nas fases 4 e 5. Valores mais altos são mais conservadores: menos casos ",
                  "georreferenciados, com maior confiança em cada um. Não afeta o pipeline do ",
                  "Distrito Federal, que é determinístico."
                ),
                tags$li(
                  strong("Clique em Processar Dados."), br(),
                  "O andamento aparece em uma barra de progresso com a fase em execução. ",
                  "Ao terminar, o painel abre automaticamente a aba ", inline(strong("Resumo"), ".")
                )
              ),
              br(),
              p(strong("Depois do processamento, as demais abas ficam disponíveis:")),
              tags$ul(
                tags$li(
                  strong("Resumo"), " — método utilizado, escopo processado, tempo de execução ",
                  "e total de registros, com os botões de download."
                ),
                tags$li(
                  strong("Métricas Detalhadas"), " — distribuição de casos por município ou RA; ",
                  "cobertura e matches por fase no DF; qualidade, tipo de endereço e ",
                  "estatísticas de similaridade nas demais UFs."
                ),
                tags$li(
                  strong("Gráficos"), " — precisão posicional dos casos, cobertura ao longo ",
                  "das fases, resultado por município ou RA, casos por setor censitário e, nas ",
                  "demais UFs, distância do número, município divergente e similaridade das ",
                  "fases probabilísticas."
                ),
                tags$li(
                  strong("Mapa de Coordenadas"), " — visualização interativa dos casos, como ",
                  "pontos ou densidade de kernel (KDE), separando posição por endereço de ",
                  "centroide. O mapa pode ser baixado em PNG como está na tela."
                ),
                tags$li(
                  strong("Triagem de Endereços"), " — revisão manual, caso a caso, dos ",
                  "registros de baixa similaridade, escolhendo entre os candidatos rankeados. ",
                  "Indisponível para o DF, cujo pipeline é determinístico e não gera ",
                  "candidatos alternativos."
                )
              )
            ),

            # ---------------- Metodologia ----------------
            div(
              class = "intro-section",
              h3("Metodologia"),
              p(
                "O georreferenciamento funciona como uma ",
                inline(strong("cascata de tentativas"), ": as fases são aplicadas em sequência, "),
                "da mais determinística e confiável para ",
                "a mais aproximada. Um caso só é avaliado por uma fase se ", em("não"),
                " obteve match em nenhuma das fases anteriores — e o rótulo da fase que o ",
                "resolveu fica gravado no resultado, permitindo auditar a confiabilidade de cada ",
                "registro individualmente."
              ),
              p(
                "O método aplicado depende da Unidade Federativa selecionada:"
              ),
              div(
                class = "intro-cards",

                div(
                  class = "intro-card card-df",
                  h4("Distrito Federal"),
                  div(class = "card-sub", "Determinístico · 18 fases"),
                  tags$ul(
                    tags$li(
                      "Pipeline próprio, otimizado para o volume e a formatação de endereços ",
                      "do DF (quadras, conjuntos e lotes)."
                    ),
                    tags$li(
                      "Compara bigramas de identificador e valor — por exemplo ", strong("QNM"),
                      " → ", strong("12"), " — com filtro de Região Administrativa e desempate ",
                      "pela menor distância de número."
                    ),
                    tags$li(
                      "Fases seguintes tratam variações de abreviação, letra de quadra, ",
                      "chácaras, identificadores raros e ausência de RA informada."
                    ),
                    tags$li(
                      "Casos sem match em nenhuma fase recebem o centroide do polígono da ",
                      "Região Administrativa informada ou, se a RA for desconhecida, do DF. ",
                      "Ficam sinalizados como tal e sem setor censitário."
                    )
                  )
                ),

                div(
                  class = "intro-card",
                  h4("Demais Unidades Federativas"),
                  div(class = "card-sub", "Record linkage · 5 fases, 21 subfases"),
                  tags$ul(
                    tags$li(
                      strong("Fases 1 a 3 — determinísticas:"), " endereço completo idêntico, ",
                      "logradouro idêntico ignorando o número, e nome de logradouro idêntico ",
                      "restrito a um único bairro."
                    ),
                    tags$li(
                      strong("Fases 4 e 5 — probabilísticas:"), " similaridade ",
                      "Jaro-Winkler entre o endereço notificado e os candidatos do CNEFE."
                    ),
                    tags$li(
                      "Cada fase roda primeiro nos casos ", strong("com"), " município ",
                      "informado e depois, para quem sobrou, nos casos ", strong("sem"),
                      " município — sem exigir correspondência territorial."
                    ),
                    tags$li(
                      "Casos sem match em nenhuma fase recebem o centroide do polígono do ",
                      "município informado ou, sem município válido, da UF. Ficam ",
                      "sinalizados como tal e sem setor censitário."
                    ),
                    tags$li(
                      "As fases probabilísticas também alimentam a lista de candidatos ",
                      "rankeados usada na aba de Triagem."
                    )
                  )
                ),

                div(
                  class = "intro-card card-geocodebr",
                  h4("GeocodeBr"),
                  div(class = "card-sub", "Alternativa · pacote geocodebr"),
                  tags$ul(
                    tags$li(
                      "Geocodificação pelo pacote ",
                      inline(code("geocodebr"), ", como alternativa ao "),
                      "record linkage probabilístico."
                    ),
                    tags$li(
                      "Retorna precisão do match, endereço encontrado, desvio em metros e ",
                      "setor censitário."
                    ),
                    tags$li(
                      "Selecionável no campo ",
                      inline(strong("Método de processamento"), ", no menu à esquerda.")
                    )
                  )
                )
              ),
              br(),
              p(
                "O critério exato de cada fase, o rótulo gravado no resultado e a referência ",
                "à linha correspondente no código estão descritos na ",
                inline(
                  tags$a(
                    href = "https://github.com/Dev-Cenarios/Sistema_de_Georreferenciamento_Geocenarios/blob/main/docs/DOCUMENTACAO_FASES_MATCHING.md",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    "Documentação das Fases de Matching"
                  ),
                  "."
                )
              ),
              p(
                "Além do rótulo da fase, cada caso do resultado leva duas colunas de leitura ",
                "rápida: ", inline(code("GEORREFERENCIADO"), " (1 quando o caso casou numa fase "),
                "de endereço — determinística, ou probabilística com similaridade acima do ",
                "limite escolhido; 0 para qualquer centroide, coordenada ausente ou endereço ",
                "vazio) e ", inline(code("INDICADOR"), " (0 para detento ou pessoa em situação "),
                "de rua, identificados pelos campos da ficha do SINAN ou pelo texto do ",
                "endereço; 1 para os demais casos, independente do georreferenciamento)."
              )
            ),

            # ---------------- Fontes de dados ----------------
            div(
              class = "intro-section",
              h3("Fontes de dados"),
              div(
                class = "intro-source",
                strong("CNEFE — Cadastro Nacional de Endereços para Fins Estatísticos (IBGE)."),
                br(),
                "Base de endereços de referência usada na geocodificação, com coordenadas, ",
                "setor censitário e município de cada endereço cadastral. Mantida localmente ",
                "em um banco DuckDB, com uma tabela por UF. ",
                inline(
                  tags$a(
                    href = "https://www.ibge.gov.br/estatisticas/sociais/populacao/38734-cadastro-nacional-de-enderecos-para-fins-estatisticos.html",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    "Conheça o CNEFE no site do IBGE"
                  ),
                  "."
                )
              ),
              div(
                class = "intro-source",
                strong("Malhas territoriais do IBGE."), br(),
                "Regiões Administrativas no Distrito Federal e municípios nas demais UFs, ",
                "usadas no contorno do mapa e no ponto atribuído aos casos sem match ",
                "(centroide do polígono)."
              ),
              div(
                class = "intro-note",
                strong("Privacidade. "),
                "Por se tratar de dados de notificação de agravo de saúde, potencialmente ",
                "sensíveis ou identificáveis, todo o processamento é feito localmente. Os ",
                "arquivos enviados não são retidos além da sessão do painel."
              )
            ),

            # ---------------- Referências ----------------
            div(
              class = "intro-section",
              h3("Referências"),
              tags$ul(
                class = "intro-links",
                tags$li(
                  tags$a(
                    href = "https://www.ibge.gov.br/estatisticas/sociais/populacao/38734-cadastro-nacional-de-enderecos-para-fins-estatisticos.html",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    "CNEFE — Cadastro Nacional de Endereços para Fins Estatísticos (IBGE)"
                  ),
                  span(
                    class = "link-desc",
                    "Página oficial da base de endereços usada como referência na geocodificação."
                  )
                ),
                tags$li(
                  tags$a(
                    href = "https://cenarios.unb.br/tuberculose-cenarios-de-risco-e-determinantes-sociais/",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    "Tuberculose — Cenários de Risco e Determinantes Sociais"
                  ),
                  span(class = "link-desc", "Eixo do Programa Cenários do qual este painel é uma entrega.")
                ),
                tags$li(
                  tags$a(
                    href = "https://github.com/Dev-Cenarios/Sistema_de_Georreferenciamento_Geocenarios",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    "Repositório do sistema no GitHub"
                  ),
                  span(class = "link-desc", "Código-fonte, instruções de instalação e documentação técnica.")
                ),
                tags$li(
                  tags$a(
                    href = "https://github.com/Dev-Cenarios/Sistema_de_Georreferenciamento_Geocenarios/blob/main/docs/DOCUMENTACAO_FASES_MATCHING.md",
                    target = "_blank",
                    rel = "noopener noreferrer",
                    "Documentação das Fases de Matching"
                  ),
                  span(
                    class = "link-desc",
                    "Critério de match, rótulo gravado e referência no código, fase a fase."
                  )
                )
              )
            )
          )
        ),

        tabPanel(
          "Resumo",
          value = "resumo",
          h3("Resumo do Processamento"),
          verbatimTextOutput("resumo_processamento"),
          
          # Botões movidos para baixo do quadro de métricas
          conditionalPanel(
            condition = "output.processamento_concluido",
            div(class = "download-buttons",
                downloadButton("download_dados", "Baixar Dados Processados"),
                br(), br(),
                downloadButton("download_metricas", "Baixar Métricas"),
                br(), br(),
                actionButton("limpar", "Limpar Tudo", icon = icon("trash"))
            )
          )
        ),
        
        tabPanel(
          "Métricas Detalhadas",
          value = "metricas",
          conditionalPanel(
            condition = "output.processamento_concluido",
            h3("Métricas Detalhadas - Match Probabilístico"),

            # Layout uma tabela embaixo da outra
            div(

              # Tabela 1 - Distribuição de Casos por RA
              div(
                div(class = "table-header", "Distribuição de casos por Município ou RA (DF)"),
                div(class = "scrollable-table",
                    tableOutput("metricas_RA")
                )
              ),

              # Espaçamento entre tabelas
              br(),

              # Tabela 2 - Distribuição de casos por fase de match (DF e demais
              # UFs). Oculta no GeocodeBr, que não trabalha com fases.
              conditionalPanel(
                condition = "input.metodo == 'cnefe' || input.uf == 'DF'",
                div(
                  div(class = "table-header", "Distribuição de casos por fase de match"),
                  div(class = "scrollable-table",
                      tableOutput("metricas_fases")
                  )
                ),
                br()
              ),

              # DF: pipeline determinístico — cobertura do georreferenciamento
              conditionalPanel(
                condition = "input.uf == 'DF'",
                div(
                  div(class = "table-header", "Cobertura do Georreferenciamento"),
                  div(class = "scrollable-table",
                      tableOutput("metricas_cobertura")
                  )
                )
              ),

              # Outras UFs: método de similaridade — qualidade e tipo de endereço
              conditionalPanel(
                condition = "input.uf != 'DF'",
                div(
                  div(class = "table-header", "Qualidade dos Endereços"),
                  div(class = "scrollable-table",
                      tableOutput("metricas_qualidade")
                  )
                ),
                br(),
                div(
                  div(class = "table-header", "Tipo dos endereços"),
                  div(class = "scrollable-table",
                      tableOutput("metricas_tipo_endereco")
                  )
                )
              )

            )
          ),
          conditionalPanel(
            condition = "!output.processamento_concluido",
            p("Processe os dados primeiro para ver as métricas detalhadas.")
          )
        ),
        
        tabPanel(
          "Gráficos",
          value = "graficos",
          conditionalPanel(
            condition = "output.processamento_concluido",
            h3("Gráficos do Georreferenciamento"),
            # Montado no servidor: o conjunto de gráficos depende do pipeline
            # (DF ou demais UFs) e do que o processamento produziu.
            uiOutput("graficos_conteudo")
          ),
          conditionalPanel(
            condition = "!output.processamento_concluido",
            p("Processe os dados primeiro para ver os gráficos.")
          )
        ),
        
        tabPanel(
          "Mapa de Coordenadas",
          value = "mapa",
          conditionalPanel(
            condition = "output.processamento_concluido",
            h3("Visualização Geográfica dos Dados Processados"),
            
            # Controles de visualização
            div(
              style = "margin-bottom: 15px;",
              radioButtons(
                "mapa_tipo_visualizacao",
                label = "Tipo de Visualização:",
                choices = c(
                  "Pontos" = "pontos",
                  "Densidade de Kernel (KDE)" = "densidade"
                ),
                selected = "pontos",
                inline = TRUE
              ),
              
              # Opções adicionais para densidade
              conditionalPanel(
                condition = "input.mapa_tipo_visualizacao == 'densidade'",
                div(
                  style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  
                  sliderInput(
                    "kde_bandwidth",
                    "Suavização da Densidade:",
                    min = 0.01,
                    max = 0.5,
                    value = 0.1,
                    step = 0.01
                  ),
                  
                  sliderInput(
                    "kde_alpha",
                    "Transparência:",
                    min = 0.1,
                    max = 1.0,
                    value = 0.7,
                    step = 0.1
                  ),
                  
                  checkboxInput(
                    "kde_mostrar_pontos",
                    "Mostrar pontos sobre a densidade",
                    value = FALSE
                  ),

                  # Fora por padrão: todos os casos de fallback de uma unidade
                  # caem no mesmo ponto (o centroide do polígono), e esse
                  # acúmulo vira uma mancha de densidade que não existe.
                  checkboxInput(
                    "kde_incluir_centroides",
                    "Incluir casos de centroide (fallback) na densidade",
                    value = FALSE
                  )
                )
              )
            ),

            div(
              class = "mapa-exportar",
              actionButton("exportar_mapa", "Baixar mapa (PNG)", icon = icon("download")),
              span(class = "nota",
                   "Salva o mapa como está na tela: pontos ou densidade, com o zoom e a legenda atuais.")
            ),

            leafletOutput("mapa_coordenadas", height = "600px")
          ),
          
          conditionalPanel(
            condition = "!output.processamento_concluido",
            p("Processe os dados primeiro para visualizar o mapa de coordenadas.")
          )
        ),
        
        # NOVA ABA: TRIAGEM DE ENDEREÇOS
        tabPanel(
          "Triagem de Endereços",
          value = "triagem",
          conditionalPanel(
            condition = "output.processamento_concluido",
            div(class = "triagem-controls",
                h3("Triagem de Endereços com Baixa Similaridade"),
                
                div(class = "threshold-selector",
                    sliderInput(
                      "triagem_threshold",
                      "Limite de similaridade para triagem:",
                      min = 0.1,
                      max = 1.0,
                      value = 0.8,
                      step = 0.05
                    ),
                    
                    radioButtons(
                      "triagem_scope",
                      "Escopo da triagem:",
                      choices = c(
                        "Todos os endereços" = "todos",
                        "Apenas endereços com similaridade < limiar" = "baixa_similaridade"
                      ),
                      selected = "baixa_similaridade"
                    ),
                    
                    actionButton(
                      "carregar_triagem",
                      "Carregar Dados para Triagem",
                      icon = icon("search"),
                      class = "btn-primary"
                    )
                ),
                
                div(class = "action-buttons",
                    actionButton(
                      "finalizar_triagem",
                      "Finalizar Triagem",
                      icon = icon("check"),
                      class = "btn-success"
                    ),
                    
                    actionButton(
                      "reset_triagem",
                      "Resetar Seleções",
                      icon = icon("undo"),
                      class = "btn-warning"
                    ),
                    
                    downloadButton(
                      "download_triagem",
                      "Baixar Resultado da Triagem",
                      class = "btn-info"
                    )
                )
            ),
            
            # Tabela interativa para triagem
            fluidRow(
              column(
                12,
                uiOutput("box_endereco_atual")
              )
            ),
            
            br(),
            
            fluidRow(
              column(
                12,
                DTOutput("tabela_triagem")
              )
            ),
            
            br(),
            
            fluidRow(
              column(
                6,
                actionButton(
                  "triagem_anterior",
                  "⬅ Endereço anterior",
                  class = "btn-secondary"
                )
              ),
              column(
                6,
                actionButton(
                  "triagem_proximo",
                  "Próximo endereço ➡",
                  class = "btn-primary"
                )
              )
            )
            ,
            
            # Painel de estatísticas da triagem
            uiOutput("estatisticas_triagem")
          ),
          conditionalPanel(
            condition = "!output.processamento_concluido",
            p("Processe os dados primeiro para acessar a triagem de endereços.")
          )
        )  ## ASQUI
      )
    )
  ) ## ASQUI
)

# Servidor
server <- function(input, output, session) {
  
  # Output para a logo - usando base64 encoding para garantir que funcione
  output$logo_image <- renderUI({
    # Caminho da imagem
    caminho_imagem <- "cenarios_logo_full.jpeg"
    
    if (file.exists(caminho_imagem)) {
      # Ler a imagem e converter para base64
      imagem_data <- readBin(caminho_imagem, "raw", file.info(caminho_imagem)$size)
      imagem_base64 <- base64enc::base64encode(imagem_data)
      
      tags$img(src = paste0("data:image/jpeg;base64,", imagem_base64),
               class = "logo-img",
               alt = "Logo Cenários NESP UnB")
    } else {
      # Fallback se a imagem não existir
      tags$div(class = "logo-img", 
               style = "height: 60px; background: #f0f0f0; display: flex; align-items: center; justify-content: center;",
               "Logo não encontrada")
    }
  })
  
  # Reactive values para armazenar resultados
  resultados <- reactiveValues(
    base_processada = NULL,
    metricas = NULL,
    processado = FALSE,
    arquivos_carregados = FALSE,
    dados_triagem = NULL,
    selecoes_triagem = list(),
    dados_finais = NULL
  )
  
  ##########################################
  ##########################################
  ##########################################
  ##########################################
  ######
  ###### FUNÇÕES ######################
  ######
  
  ### PASSO A PASSO PARA IMPLEMENTAÇÃO DA FUNÇÃO NOVA:
  
  # 1. VERIFICAR ETL
  # 2. FUNÇÃO COM ELEMENTOS SHINY
  # 3. UI
  # 4. PADRONIZAÇÃO DE COLUNAS
  # 5. TESTES
  
  # Junta partes de um endereço (complemento/logradouro, número, bairro)
  # ignorando as que estiverem ausentes -- ao contrário de paste()/paste0(),
  # que gravam o TEXTO "NA" quando um componente é NA. Ex.: o padrão usado
  # nos três blocos abaixo, paste(NM_COMPLEM, NU_NUMERO, NM_BAIRRO, sep=", "),
  # com NM_COMPLEM ausente e NU_NUMERO = "S/N", virava o texto
  # "NA, S/N, MUSTARDINHA" -- que entra como endereço real nas fases de
  # comparação (a FASE 4 e principalmente a FASE 5, que não tem o pré-filtro
  # por 5-gramas que a FASE 4 tem). Reportado processando Recife/TB (~29 mil
  # linhas): 0 casos foram para o fallback por centroide, porque até os
  # registros sem nenhuma informação de rua "casavam" com alguma rua real do
  # bairro por coincidência -- jaro_winkler_similarity(rua_real, 'NA') fica
  # entre 0,51 e 0,54 contra qualquer rua, acima do limiar padrão de 0,5.
  # Retorna NA quando NENHUMA parte está preenchida, para o chamador poder
  # distinguir "endereço vazio" de uma string válida (mas pouco informativa)
  # para comparar.
  juntar_endereco <- function(..., sep = ", ") {
    partes <- list(...)
    vazio <- function(x) is.na(x) | trimws(x) == ""
    Reduce(function(acc, parte) {
      ifelse(vazio(parte), acc,
             ifelse(vazio(acc), parte, paste(acc, parte, sep = sep)))
    }, partes)
  }

  # FUNÇÃO DE PADRONIZAÇÃO DE COLUNAS DECLARADAS PELO USUÁRIO
  padronizar_colunas_usuario <- function(df, cod_unico = NULL, logr = NULL, num = NULL, mun = NULL, ra = NULL, compl = NULL, bairro = NULL, address = NULL, uf) {

    # --------------------
    # Conferência das colunas declaradas
    # --------------------
    # Todas de uma vez, dizendo QUAL nome não foi encontrado e quais existem no
    # arquivo. Antes, logradouro e número quebravam com uma mensagem que não
    # dizia qual dos dois era, e município, RA, bairro e complemento eram
    # ignorados em silêncio quando o nome não batia -- um nome errado ali
    # derrubava o filtro territorial sem nenhum aviso, e os casos acabavam no
    # centroide da UF.
    declaradas <- c("código único" = cod_unico, "logradouro" = logr,
                    "número" = num, "município" = mun, "RA" = ra,
                    "complemento" = compl, "bairro" = bairro,
                    "endereço completo" = address)
    declaradas <- declaradas[!is.na(declaradas) & nzchar(declaradas)]
    faltando <- declaradas[!declaradas %in% names(df)]
    if (length(faltando) > 0) {
      stop(paste0(
        "Não encontrei no arquivo ",
        if (length(faltando) == 1) "a coluna declarada em " else "as colunas declaradas em ",
        paste0(names(faltando), ' ("', faltando, '")', collapse = ", "),
        ". Colunas do arquivo: ", paste(names(df), collapse = ", "), "."
      ))
    }

    # --------------------
    # Código único
    # --------------------
    if (!is.null(cod_unico) && cod_unico != "") {
      df$NU_NOTIFIC <- as.character(df[[cod_unico]])
    } else {
      df$NU_NOTIFIC <- as.character(seq_len(nrow(df)))
    }

    # Usado mais abaixo para não deixar o bloco de MUNIRESAT sobrescrever o
    # QUALIDADE_ENDERECO já definido no CASO 1 ("Endereço completo declarado.").
    endereco_completo <- !is.null(address) && address != ""

    # =====================================================
    # CASO 1 — ENDEREÇO COMPLETO
    # =====================================================
    if (!is.null(address) && address != "") {

      df$address    <- as.character(df[[address]])
      df$NM_LOGRADO <- NA_character_
      df$NU_NUMERO  <- NA_character_
      df$NM_COMPLEM <- NA_character_

      df$address = stringi::stri_trans_toupper(df$address)

      # Bairro é opcional mesmo no modo "endereço completo" — usado só para
      # montar address3 (endereço + bairro), consumido pelas fases de
      # correspondência aproximada do pipeline (georreferenciar_UF).
      if (!is.null(bairro) && bairro != "" && bairro %in% names(df)) {
        df$NM_BAIRRO <- enderecobr::padronizar_bairros(as.character(df[[bairro]]))
      } else {
        df$NM_BAIRRO <- NA_character_
      }

      df <- df %>%
        mutate(
          # Sem logradouro/número separados, não dá pra isolar com segurança a
          # versão "sem número" — address2 replica address, e address3 só
          # acrescenta o bairro ao endereço completo já fornecido.
          address2 = address,
          address3 = ifelse(
            is.na(NM_BAIRRO) | NM_BAIRRO == "",
            address,
            paste(address, NM_BAIRRO, sep = ", ")
          ),
          TIPO_ENDERECO = case_when(
            grepl("\\bRUA\\b", address, ignore.case = TRUE) ~ "RUA",
            grepl("\\bAVENIDA\\b|\\bAV\\b", address, ignore.case = TRUE) ~ "AVENIDA",
            grepl("\\bQUADRA\\b|\\bQD\\b|\\bQ\\b", address, ignore.case = TRUE) ~ "QUADRA",
            grepl("\\bCONDOMINIO\\b|\\bCOND\\b|\\bCD\\b", address, ignore.case = TRUE) ~ "CONDOMINIO",
            grepl("\\bVILA\\b|\\bVL\\b", address, ignore.case = TRUE) ~ "VILA",
            grepl("\\bCHACARA\\b|\\bCH\\b", address, ignore.case = TRUE) ~ "CHACARA",
            grepl("\\bCONJUNTO\\b|\\bCJ\\b|\\bCONJ\\b", address, ignore.case = TRUE) ~ "CONJUNTO",
            grepl("\\bSETOR\\b|\\bST\\b", address, ignore.case = TRUE) ~ "SETOR",
            grepl("\\bALAMEDA\\b", address, ignore.case = TRUE) ~ "ALAMEDA",
            grepl("\\bTRAVESSA\\b|\\bTV\\b", address, ignore.case = TRUE) ~ "TRAVESSA",
            grepl("\\bPRACA\\b", address, ignore.case = TRUE) ~ "PRACA",
            grepl("\\bRODOVIA\\b|\\bROD\\b|\\bBR\\b", address, ignore.case = TRUE) ~ "RODOVIA",
            grepl("\\bLOTE\\b|\\bLT\\b", address, ignore.case = TRUE) ~ "LOTE",
            grepl("\\bBLOCO\\b|\\bBL\\b", address, ignore.case = TRUE) ~ "BLOCO",
            grepl("\\bAPARTAMENTO\\b|\\bAPT\\b", address, ignore.case = TRUE) ~ "APARTAMENTO",
            grepl("\\bCASA\\b|\\bCS\\b", address, ignore.case = TRUE) ~ "CASA",
            grepl("\\bNUCLEO\\b|\\bN\\b", address, ignore.case = TRUE) ~ "NUCLEO",
            grepl("\\bAREA\\s+RESIDENCIAL\\b|\\bAR\\b", address, ignore.case = TRUE) ~ "AREA_RESIDENCIAL",
            grepl("\\bRESIDENCIAL\\b", address, ignore.case = TRUE) ~ "RESIDENCIAL",
            grepl("\\bLOTEAMENTO\\b", address, ignore.case = TRUE) ~ "LOTEAMENTO",
            grepl("\\bPARQUE\\b", address, ignore.case = TRUE) ~ "PARQUE",
            grepl("\\bJARDIM\\b", address, ignore.case = TRUE) ~ "JARDIM",
            grepl("\\bVIELA\\b", address, ignore.case = TRUE) ~ "VIELA",
            grepl("\\bBECO\\b", address, ignore.case = TRUE) ~ "BECO",
            grepl("\\bESTRADA\\b", address, ignore.case = TRUE) ~ "ESTRADA",
            !is.na(address) & address != "" ~ "OUTRO",
            TRUE ~ "NAO_INFORMADO"
          )
        )

      df$QUALIDADE_ENDERECO <- "Endereço completo declarado."

    } else {

      # =====================================================
      # CASO 2 — ENDEREÇO SEPARADO
      # =====================================================
      if (is.null(logr) || logr == "" || is.null(num) || num == "") {
        stop("Logradouro e Número são obrigatórios quando endereço completo não é informado.")
      }

      df$NM_LOGRADO <- enderecobr::padronizar_logradouros(as.character(df[[logr]]))
      df$NU_NUMERO  <- enderecobr::padronizar_numeros(as.character(df[[num]]))

      if (!is.null(compl) && compl != "" && compl %in% names(df)) {
        df$NM_COMPLEM <- enderecobr::padronizar_logradouros(as.character(df[[compl]]))
      } else {
        df$NM_COMPLEM <- NA_character_
      }

      # Bairro entra na montagem de address/address2/address3 (ver abaixo) —
      # é o que o pipeline atual usa nas fases de correspondência aproximada,
      # diferente de versões anteriores em que o bairro não era usado.
      if (!is.null(bairro) && bairro != "" && bairro %in% names(df)) {
        df$NM_BAIRRO <- enderecobr::padronizar_bairros(as.character(df[[bairro]]))
      } else {
        df$NM_BAIRRO <- NA_character_
      }

      # Construção de address / address2 / address3 — mesmo padrão do
      # etl_sinan() (ramo não-DF), pra manter os dois caminhos de entrada
      # (SINAN e colunas customizadas) produzindo variantes comparáveis:
      #   address  = logradouro + número + bairro  (endereço completo)
      #   address2 = logradouro + número            (sem bairro)
      #   address3 = logradouro + bairro             (sem número)
      # As 3 variantes são comparadas contra ENDERECO_CNEFE / _CNEFE2 / _CNEFE3
      # em fases distintas de georreferenciar_UF().
      df <- df %>%
        rowwise() %>%
        mutate(
          address = case_when(
            is.na(NM_LOGRADO) | NM_LOGRADO == "" ~ ifelse(
              is.na(NU_NUMERO) | NU_NUMERO == "",
              juntar_endereco(NM_COMPLEM, NM_BAIRRO),
              juntar_endereco(NM_COMPLEM, NU_NUMERO, NM_BAIRRO)
            ),
            TRUE ~ ifelse(
              is.na(NU_NUMERO) | NU_NUMERO == "",
              juntar_endereco(NM_LOGRADO, NM_BAIRRO),
              juntar_endereco(NM_LOGRADO, NU_NUMERO, NM_BAIRRO)
            )
          ),
          address2 = case_when(
            is.na(NM_LOGRADO) | NM_LOGRADO == "" ~ ifelse(
              is.na(NU_NUMERO) | NU_NUMERO == "",
              NM_COMPLEM,
              juntar_endereco(NM_COMPLEM, NU_NUMERO)
            ),
            TRUE ~ ifelse(
              is.na(NU_NUMERO) | NU_NUMERO == "",
              NM_LOGRADO,
              juntar_endereco(NM_LOGRADO, NU_NUMERO)
            )
          ),
          address3 = case_when(
            is.na(NM_LOGRADO) | NM_LOGRADO == "" ~ juntar_endereco(NM_COMPLEM, NM_BAIRRO),
            TRUE ~ juntar_endereco(NM_LOGRADO, NM_BAIRRO)
          )
        ) %>%
        ungroup()

      df <- df %>%
        mutate(
          across(
            c(address, address2, address3),
            ~ {
              x <- corrigir_bytes(.x)
              x <- stringi::stri_trans_toupper(x)
              x <- gsub("[.-]", "", x)
              x
            }
          ),
          # Igual pro DF e pras demais UFs (a lógica de detecção é idêntica em
          # ambos os ramos do etl_sinan) — antes só era calculado se uf=="DF".
          TIPO_ENDERECO = case_when(
            grepl("\\bRUA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RUA",
            grepl("\\bAVENIDA\\b|\\bAV\\b", NM_LOGRADO, ignore.case = TRUE) ~ "AVENIDA",
            grepl("\\bQUADRA\\b|\\bQD\\b|\\bQ\\b", NM_LOGRADO, ignore.case = TRUE) ~ "QUADRA",
            grepl("\\bCONDOMINIO\\b|\\bCOND\\b|\\bCD\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CONDOMINIO",
            grepl("\\bVILA\\b|\\bVL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "VILA",
            grepl("\\bCHACARA\\b|\\bCH\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CHACARA",
            grepl("\\bCONJUNTO\\b|\\bCJ\\b|\\bCONJ\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CONJUNTO",
            grepl("\\bSETOR\\b|\\bST\\b", NM_LOGRADO, ignore.case = TRUE) ~ "SETOR",
            grepl("\\bALAMEDA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "ALAMEDA",
            grepl("\\bTRAVESSA\\b|\\bTV\\b", NM_LOGRADO, ignore.case = TRUE) ~ "TRAVESSA",
            grepl("\\bPRACA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "PRACA",
            grepl("\\bRODOVIA\\b|\\bROD\\b|\\bBR\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RODOVIA",
            grepl("\\bLOTE\\b|\\bLT\\b", NM_LOGRADO, ignore.case = TRUE) ~ "LOTE",
            grepl("\\bBLOCO\\b|\\bBL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "BLOCO",
            grepl("\\bAPARTAMENTO\\b|\\bAPT\\b", NM_LOGRADO, ignore.case = TRUE) ~ "APARTAMENTO",
            grepl("\\bCASA\\b|\\bCS\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CASA",
            grepl("\\bNUCLEO\\b|\\bN\\b", NM_LOGRADO, ignore.case = TRUE) ~ "NUCLEO",
            grepl("\\bAREA\\s+RESIDENCIAL\\b|\\bAR\\b", NM_LOGRADO, ignore.case = TRUE) ~ "AREA_RESIDENCIAL",
            grepl("\\bRESIDENCIAL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RESIDENCIAL",
            grepl("\\bLOTEAMENTO\\b", NM_LOGRADO, ignore.case = TRUE) ~ "LOTEAMENTO",
            grepl("\\bPARQUE\\b", NM_LOGRADO, ignore.case = TRUE) ~ "PARQUE",
            grepl("\\bJARDIM\\b", NM_LOGRADO, ignore.case = TRUE) ~ "JARDIM",
            grepl("\\bVIELA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "VIELA",
            grepl("\\bBECO\\b", NM_LOGRADO, ignore.case = TRUE) ~ "BECO",
            grepl("\\bESTRADA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "ESTRADA",
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "OUTRO",
            TRUE ~ "NAO_INFORMADO"
          )
        )
    }

    # --------------------
    # Criação de MUNIRESAT (usu_mun)
    # --------------------
    if (!is.null(mun) && mun != "" && mun %in% names(df) && uf != "DF") {
      valores_mun <- as.character(df[[mun]])

      # A coluna declarada pode trazer código IBGE (o caso mais comum) ou o
      # NOME do município (vários conjuntos de dados só têm isso). Detecta
      # por valor -- só dígitos vira código como sempre foi; o resto é
      # padronizado (maiúsculas, sem acento) via enderecobr::padronizar_
      # municipios() e resolvido pra código via enderecobr::codigos_municipios,
      # restrito à UF selecionada (o mesmo nome de município se repete em UFs
      # diferentes -- ex: Palmas existe em TO e PR -- por isso o filtro por UF
      # é obrigatório, não é só um lookup pelo nome puro).
      eh_codigo <- grepl("^\\s*\\d+\\s*$", valores_mun)
      if (any(!eh_codigo & !is.na(valores_mun) & valores_mun != "")) {
        codigo_estado_uf <- enderecobr::codigos_estados$codigo_estado[
          enderecobr::codigos_estados$abrev_estado == uf
        ]
        municipios_uf <- enderecobr::codigos_municipios[
          enderecobr::codigos_municipios$codigo_estado == codigo_estado_uf,
        ]
        nomes_padronizados <- enderecobr::padronizar_municipios(valores_mun)
        # CD_MUN na CNEFE é o código IBGE de 6 dígitos (sem o dígito
        # verificador) -- mesmo truncamento já usado para o código digitado
        # manualmente no filtro de "escopo município".
        lookup <- setNames(substr(municipios_uf$codigo_muni, 1, 6), municipios_uf$nome_muni)
        valores_mun[!eh_codigo] <- unname(lookup[nomes_padronizados[!eh_codigo]])
      }

      df$MUNIRESAT <- valores_mun
      # Linhas sem código de município (NA ou vazio) viram "Ignorado", igual ao
      # tratamento de quem não informa a coluna inteira — sem isso, essas linhas
      # ficam com MUNIRESAT = NA, que o SQL do pipeline não reconhece nem como
      # "com município" nem como "Ignorado", e acabam descartadas silenciosamente.
      df$MUNIRESAT <- ifelse(is.na(df$MUNIRESAT) | df$MUNIRESAT == "", "Ignorado", df$MUNIRESAT)

      # Só recalcula QUALIDADE_ENDERECO aqui se o usuário não declarou uma
      # coluna de endereço completo — senão isso sobrescreveria o "Endereço
      # completo declarado." definido no CASO 1 (NM_LOGRADO/NU_NUMERO/
      # NM_COMPLEM ficam NA nesse caso, o que faria o case_when abaixo cair
      # sempre em "bairro" ou "UF").
      if (!endereco_completo) {
      df <- df %>%
        mutate(
          # Critério agora baseado em NM_BAIRRO, igual ao etl_sinan() — antes
          # usava MUNIRESAT aqui, o que não fazia sentido já que MUNIRESAT é o
          # filtro de município da consulta SQL, não uma medida de completude
          # do endereço em si.
          QUALIDADE_ENDERECO = case_when(

            # 2º nível: bairro + logradouro + número + complemento
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + logradouro + numero + complemento",

            # 3º nível: bairro + logradouro + número
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "bairro + logradouro + numero",

            # 4º nível: bairro + logradouro + complemento
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + logradouro + complemento",

            # 5º nível: bairro + número + complemento
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + numero + complemento",

            # 6º nível: logradouro + número + complemento
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + numero + complemento",

            # 7º nível: bairro + logradouro
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "bairro + logradouro",

            # 8º nível: bairro + número
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "bairro + numero",

            # 9º nível: bairro + complemento
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + complemento",

            # 10º nível: logradouro + número
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "logradouro + numero",

            # 11º nível: logradouro + complemento
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + complemento",

            # 12º nível: número + complemento
            !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "numero + complemento",

            # 13º nível: apenas bairro
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" ~ "bairro",

            # 14º nível: apenas logradouro
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "logradouro",

            # 15º nível: apenas número
            !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "numero",

            # 16º nível: apenas complemento
            !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "complemento",

            # 17º nível: nenhum campo — apenas UF disponível
            TRUE ~ "UF"
          )
        )
      }

    } else if (uf != "DF") {
      df$MUNIRESAT <- "Ignorado"

      if (!endereco_completo) {
      df <- df %>%
        mutate(
          QUALIDADE_ENDERECO = case_when(

            # 2º nível: bairro + logradouro + número + complemento
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + logradouro + numero + complemento",

            # 3º nível: bairro + logradouro + número
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "bairro + logradouro + numero",

            # 4º nível: bairro + logradouro + complemento
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + logradouro + complemento",

            # 5º nível: bairro + número + complemento
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + numero + complemento",

            # 6º nível: logradouro + número + complemento
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + numero + complemento",

            # 7º nível: bairro + logradouro
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "bairro + logradouro",

            # 8º nível: bairro + número
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "bairro + numero",

            # 9º nível: bairro + complemento
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + complemento",

            # 10º nível: logradouro + número
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "logradouro + numero",

            # 11º nível: logradouro + complemento
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + complemento",

            # 12º nível: número + complemento
            !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "numero + complemento",

            # 13º nível: apenas bairro
            !is.na(NM_BAIRRO) & NM_BAIRRO != "" ~ "bairro",

            # 14º nível: apenas logradouro
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "logradouro",

            # 15º nível: apenas número
            !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "numero",

            # 16º nível: apenas complemento
            !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "complemento",

            # 17º nível: nenhum campo — apenas UF disponível
            TRUE ~ "UF"
          )
        )
      }

    }
    
    # --------------------
    # Criação de ID_DISTRIT (usu_ra)
    # --------------------
    if (!is.null(ra) && ra != "" && ra %in% names(df) && uf == "DF") {
      df$ID_DISTRIT <- as.character(df[[ra]])
      
      df <- df %>%
        mutate(
          ID_DISTRIT = as.numeric(ID_DISTRIT),
          Regiao_adm = case_when(
            ID_DISTRIT == 1   ~ "Ceilândia",
            ID_DISTRIT == 9   ~ "Taguatinga",
            ID_DISTRIT == 10  ~ "Gama",
            ID_DISTRIT == 11  ~ "Sobradinho",
            ID_DISTRIT == 12  ~ "Candangolândia",
            ID_DISTRIT == 13  ~ "Águas Claras",
            ID_DISTRIT == 14  ~ "Brazlândia",
            ID_DISTRIT == 15  ~ "Cruzeiro",
            ID_DISTRIT == 16  ~ "Recanto das Emas",
            ID_DISTRIT == 17  ~ "Planaltina",
            ID_DISTRIT == 18  ~ "Samambaia",
            ID_DISTRIT == 19  ~ "Santa Maria",
            ID_DISTRIT == 20  ~ "Riacho Fundo I",
            ID_DISTRIT == 21  ~ "Riacho Fundo II",
            ID_DISTRIT == 22  ~ "Guará",
            ID_DISTRIT == 23  ~ "São Sebastião",
            ID_DISTRIT == 24  ~ "Paranoá",
            ID_DISTRIT == 25  ~ "Lago Norte",
            ID_DISTRIT == 26  ~ "Lago Sul",
            ID_DISTRIT == 27  ~ "Plano Piloto",
            ID_DISTRIT == 28  ~ "Plano Piloto",
            ID_DISTRIT == 29  ~ "Núcleo Bandeirante",
            ID_DISTRIT == 30  ~ "SIA",
            ID_DISTRIT == 31  ~ "Park Way",
            ID_DISTRIT == 32  ~ "Sudoeste/Octogonal",
            ID_DISTRIT == 33  ~ "Varjão",
            ID_DISTRIT == 34  ~ "Sobradinho II",
            ID_DISTRIT == 35  ~ "Jardim Botânico",
            ID_DISTRIT == 96  ~ "SCIA",
            ID_DISTRIT == 97  ~ "Itapoã",
            ID_DISTRIT == 364 ~ "Vicente Pires",
            ID_DISTRIT == 435 ~ "Fercal",
            ID_DISTRIT == 560 ~ "Plano Piloto",
            ID_DISTRIT == 561 ~ "Arniqueira",
            ID_DISTRIT == 562 ~ "Sol Nascente/Pôr do Sol",
            ID_DISTRIT == 566 ~ "Água Quente",
            ID_DISTRIT == 568 ~ "Arapoanga",
            TRUE ~ "Ignorado"
          )
        )
      
      df <- df %>%
        mutate(
          Regiao_sau1 = case_when(
            Regiao_adm == "Ceilândia" ~ "Região Oeste",
            Regiao_adm == "Taguatinga" ~ "Região Sudoeste",
            Regiao_adm == "Gama" ~ "Região Sul",
            Regiao_adm == "Sobradinho" ~ "Região Norte",
            Regiao_adm == "Candangolândia" ~ "Região Centro-Sul",
            Regiao_adm == "Águas Claras" ~ "Região Sudoeste",
            Regiao_adm == "Brazlândia" ~ "Região Oeste",
            Regiao_adm == "Cruzeiro" ~ "Região Central",
            Regiao_adm == "Recanto das Emas" ~ "Região Sudoeste",
            Regiao_adm == "Planaltina" ~ "Região Norte",
            Regiao_adm == "Samambaia" ~ "Região Sudoeste",
            Regiao_adm == "Santa Maria" ~ "Região Sul",
            Regiao_adm == "Riacho Fundo I" ~ "Região Centro-Sul",
            Regiao_adm == "Riacho Fundo II" ~ "Região Centro-Sul",
            Regiao_adm == "Guará" ~ "Região Centro-Sul",
            Regiao_adm == "São Sebastião" ~ "Região Leste",
            Regiao_adm == "Paranoá" ~ "Região Leste",
            Regiao_adm == "Lago Norte" ~ "Região Central",
            Regiao_adm == "Lago Sul" ~ "Região Central",
            Regiao_adm == "Plano Piloto" ~ "Região Central",
            Regiao_adm == "Núcleo Bandeirante" ~ "Região Centro-Sul",
            Regiao_adm == "SIA" ~ "Região Centro-Sul",
            Regiao_adm == "Park Way" ~ "Região Centro-Sul",
            Regiao_adm == "Sudoeste/Octogonal" ~ "Região Central",
            Regiao_adm == "Varjão" ~ "Região Central",
            Regiao_adm == "Sobradinho II" ~ "Região Norte",
            Regiao_adm == "Jardim Botânico" ~ "Região Leste",
            Regiao_adm == "Estrutural (SCIA)" ~ "Região Centro-Sul",
            Regiao_adm == "Itapoã" ~ "Região Leste",
            Regiao_adm == "Vicente Pires" ~ "Região Sudoeste",
            Regiao_adm == "Fercal" ~ "Região Norte",
            Regiao_adm == "Arniqueira" ~ "Região Sudoeste",
            Regiao_adm == "Sol Nascente/Pôr do Sol" ~ "Região Oeste",
            Regiao_adm == "Água Quente" ~ "Região Sudoeste",
            Regiao_adm == "Arapoanga" ~ "Região Norte",
            TRUE ~ "Ignorado"
          )
        )
      
      df <- df %>%
        mutate(
          QUALIDADE_ENDERECO = case_when(
            
            # 2º nível: RA + logradouro + número + complemento
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "RA + logradouro + numero + complemento",
            
            # 3º nível: RA + logradouro + número
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "RA + logradouro + numero",
            
            # 4º nível: RA + logradouro + complemento
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "RA + logradouro + complemento",
            
            # 5º nível: RA + número + complemento
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "RA + numero + complemento",
            
            # 6º nível: logradouro + número + complemento
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + numero + complemento",
            
            # 7º nível: RA + logradouro
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "RA + logradouro",
            
            # 8º nível: RA + número
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "RA + numero",
            
            # 9º nível: RA + complemento
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "RA + complemento",
            
            # 10º nível: logradouro + número
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "logradouro + numero",
            
            # 11º nível: logradouro + complemento
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + complemento",
            
            # 12º nível: número + complemento
            !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "numero + complemento",
            
            # 13º nível: apenas RA
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" ~ "RA",
            
            # 14º nível: apenas logradouro
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "logradouro",
            
            # 15º nível: apenas número
            !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "numero",
            
            # 16º nível: apenas complemento
            !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "complemento",
            
            # 17º nível: nenhum campo — apenas UF disponível
            TRUE ~ "UF"
          )
        )
      
    } else if (uf == "DF") {
      df$ID_DISTRIT <- "Ignorado"
      
      df <- df %>%
        mutate(
          ID_DISTRIT = as.numeric(ID_DISTRIT),
          Regiao_adm = case_when(
            ID_DISTRIT == 1   ~ "Ceilândia",
            ID_DISTRIT == 9   ~ "Taguatinga",
            ID_DISTRIT == 10  ~ "Gama",
            ID_DISTRIT == 11  ~ "Sobradinho",
            ID_DISTRIT == 12  ~ "Candangolândia",
            ID_DISTRIT == 13  ~ "Águas Claras",
            ID_DISTRIT == 14  ~ "Brazlândia",
            ID_DISTRIT == 15  ~ "Cruzeiro",
            ID_DISTRIT == 16  ~ "Recanto das Emas",
            ID_DISTRIT == 17  ~ "Planaltina",
            ID_DISTRIT == 18  ~ "Samambaia",
            ID_DISTRIT == 19  ~ "Santa Maria",
            ID_DISTRIT == 20  ~ "Riacho Fundo I",
            ID_DISTRIT == 21  ~ "Riacho Fundo II",
            ID_DISTRIT == 22  ~ "Guará",
            ID_DISTRIT == 23  ~ "São Sebastião",
            ID_DISTRIT == 24  ~ "Paranoá",
            ID_DISTRIT == 25  ~ "Lago Norte",
            ID_DISTRIT == 26  ~ "Lago Sul",
            ID_DISTRIT == 27  ~ "Plano Piloto",
            ID_DISTRIT == 28  ~ "Plano Piloto",
            ID_DISTRIT == 29  ~ "Núcleo Bandeirante",
            ID_DISTRIT == 30  ~ "SIA",
            ID_DISTRIT == 31  ~ "Park Way",
            ID_DISTRIT == 32  ~ "Sudoeste/Octogonal",
            ID_DISTRIT == 33  ~ "Varjão",
            ID_DISTRIT == 34  ~ "Sobradinho II",
            ID_DISTRIT == 35  ~ "Jardim Botânico",
            ID_DISTRIT == 96  ~ "SCIA",
            ID_DISTRIT == 97  ~ "Itapoã",
            ID_DISTRIT == 364 ~ "Vicente Pires",
            ID_DISTRIT == 435 ~ "Fercal",
            ID_DISTRIT == 560 ~ "Plano Piloto",
            ID_DISTRIT == 561 ~ "Arniqueira",
            ID_DISTRIT == 562 ~ "Sol Nascente/Pôr do Sol",
            ID_DISTRIT == 566 ~ "Água Quente",
            ID_DISTRIT == 568 ~ "Arapoanga",
            TRUE ~ "Ignorado"
          )
        )
      
      df <- df %>%
        mutate(
          Regiao_sau1 = case_when(
            Regiao_adm == "Ceilândia" ~ "Região Oeste",
            Regiao_adm == "Taguatinga" ~ "Região Sudoeste",
            Regiao_adm == "Gama" ~ "Região Sul",
            Regiao_adm == "Sobradinho" ~ "Região Norte",
            Regiao_adm == "Candangolândia" ~ "Região Centro-Sul",
            Regiao_adm == "Águas Claras" ~ "Região Sudoeste",
            Regiao_adm == "Brazlândia" ~ "Região Oeste",
            Regiao_adm == "Cruzeiro" ~ "Região Central",
            Regiao_adm == "Recanto das Emas" ~ "Região Sudoeste",
            Regiao_adm == "Planaltina" ~ "Região Norte",
            Regiao_adm == "Samambaia" ~ "Região Sudoeste",
            Regiao_adm == "Santa Maria" ~ "Região Sul",
            Regiao_adm == "Riacho Fundo I" ~ "Região Centro-Sul",
            Regiao_adm == "Riacho Fundo II" ~ "Região Centro-Sul",
            Regiao_adm == "Guará" ~ "Região Centro-Sul",
            Regiao_adm == "São Sebastião" ~ "Região Leste",
            Regiao_adm == "Paranoá" ~ "Região Leste",
            Regiao_adm == "Lago Norte" ~ "Região Central",
            Regiao_adm == "Lago Sul" ~ "Região Central",
            Regiao_adm == "Plano Piloto" ~ "Região Central",
            Regiao_adm == "Núcleo Bandeirante" ~ "Região Centro-Sul",
            Regiao_adm == "SIA" ~ "Região Centro-Sul",
            Regiao_adm == "Park Way" ~ "Região Centro-Sul",
            Regiao_adm == "Sudoeste/Octogonal" ~ "Região Central",
            Regiao_adm == "Varjão" ~ "Região Central",
            Regiao_adm == "Sobradinho II" ~ "Região Norte",
            Regiao_adm == "Jardim Botânico" ~ "Região Leste",
            Regiao_adm == "Estrutural (SCIA)" ~ "Região Centro-Sul",
            Regiao_adm == "Itapoã" ~ "Região Leste",
            Regiao_adm == "Vicente Pires" ~ "Região Sudoeste",
            Regiao_adm == "Fercal" ~ "Região Norte",
            Regiao_adm == "Arniqueira" ~ "Região Sudoeste",
            Regiao_adm == "Sol Nascente/Pôr do Sol" ~ "Região Oeste",
            Regiao_adm == "Água Quente" ~ "Região Sudoeste",
            Regiao_adm == "Arapoanga" ~ "Região Norte",
            TRUE ~ "Ignorado"
          )
        )
      
      df <- df %>%
        mutate(
          QUALIDADE_ENDERECO = case_when(
            
            # 2º nível: RA + logradouro + número + complemento
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "RA + logradouro + numero + complemento",
            
            # 3º nível: RA + logradouro + número
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "RA + logradouro + numero",
            
            # 4º nível: RA + logradouro + complemento
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "RA + logradouro + complemento",
            
            # 5º nível: RA + número + complemento
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "RA + numero + complemento",
            
            # 6º nível: logradouro + número + complemento
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + numero + complemento",
            
            # 7º nível: RA + logradouro
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "RA + logradouro",
            
            # 8º nível: RA + número
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "RA + numero",
            
            # 9º nível: RA + complemento
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "RA + complemento",
            
            # 10º nível: logradouro + número
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "logradouro + numero",
            
            # 11º nível: logradouro + complemento
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + complemento",
            
            # 12º nível: número + complemento
            !is.na(NU_NUMERO) & NU_NUMERO != "" &
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "numero + complemento",
            
            # 13º nível: apenas RA
            !is.na(ID_DISTRIT) & ID_DISTRIT != "" ~ "RA",
            
            # 14º nível: apenas logradouro
            !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "logradouro",
            
            # 15º nível: apenas número
            !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "numero",
            
            # 16º nível: apenas complemento
            !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "complemento",
            
            # 17º nível: nenhum campo — apenas UF disponível
            TRUE ~ "UF"
          )
        )
      
    }
    
    return(df)
  }
  
  # FUNÇÃO DE CORREÇÃO DE BYTES INCONSISTENTES
  corrigir_bytes <- function(x) {
    if (is.factor(x)) x <- as.character(x)
    
    # 1. Remover qualquer byte inválido ANTES de tudo
    x <- iconv(x, from = "", to = "UTF-8", sub = "")   # remove bytes inválidos
    x <- enc2utf8(x)
    
    # 2. Agora os gsub() são seguros porque tudo já é UTF-8 válido
    x <- gsub("\\\\xBA", "º", x, perl = TRUE)
    x <- gsub("\\\\xAA", "ª", x, perl = TRUE)
    x <- gsub("\\\\xB0", "°", x, perl = TRUE)
    x <- gsub("\\\\xB4", "´", x, perl = TRUE)
    
    x <- gsub("\\\\xba", "º", x, perl = TRUE)
    x <- gsub("\\\\xaa", "ª", x, perl = TRUE)
    x <- gsub("\\\\xb0", "°", x, perl = TRUE)
    x <- gsub("\\\\xb4", "´", x, perl = TRUE)
    
    # 3. Remove qualquer outro lixo tipo \\xNN
    x <- gsub("\\\\x[0-9A-Fa-f]{2}", "", x, perl = TRUE)
    
    return(x)
  }
  
  # FUNÇÃO PARA SANITIZAR OS DADOS
  sanitizar_utf8 <- function(x) {
    if (!is.character(x)) return(x)
    # Tenta converter de Latin-1 para UTF-8 onde necessário
    x <- iconv(x, from = "latin1", to = "UTF-8", sub = "byte")
    # Remove qualquer byte ainda inválido (substitui por "")
    x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
    x
  }
  
  # FUNÇÃO DE ETL - SINAN DF
  etl_sinan <- function(doenca, uf, codigo_municipio = NULL){
    if(uf == "DF"){
      withProgress(message = 'Pré-processando dados...', value = 0, {
        
        #  doenca <- import('amostra.csv')
        
        # limiar_similaridade = 0.5
        
        doenca$NU_NOTIFIC <- str_pad(doenca$NU_NOTIFIC, width = 7, side = "left", pad = "0")
        
        doenca$NM_LOGRADO <- iconv(doenca$NM_LOGRADO, from = "", to = "UTF-8", sub = " ")
        
        # 2. Garantir que tudo está em UTF-8
        doenca$NM_LOGRADO <- enc2utf8(doenca$NM_LOGRADO)
        
        # Tratamento de abreviações e caracteres
        doenca <- doenca %>% mutate(
          NM_LOGRADO = padronizar_logradouros(NM_LOGRADO),
          NM_COMPLEM = padronizar_logradouros(NM_COMPLEM),
          NU_NUMERO = padronizar_numeros(NU_NUMERO)
        )
        
        #===============================
        # 1. Preparação dos endereços
        #===============================
        doenca <- doenca %>%
          rowwise() %>%
          mutate(
            address = case_when(
              # Caso 1: NM_LOGRADO é nulo/vazio
              is.na(NM_LOGRADO) | NM_LOGRADO == "" ~
                ifelse(is.na(NU_NUMERO) | NU_NUMERO == "",
                       NM_COMPLEM,
                       juntar_endereco(NM_COMPLEM, NU_NUMERO)),

              # Caso 2: NM_LOGRADO existe
              TRUE ~
                ifelse(is.na(NU_NUMERO) | NU_NUMERO == "",
                       NM_LOGRADO,
                       juntar_endereco(NM_LOGRADO, NU_NUMERO))
            )
          ) %>%
          ungroup()
        
        doenca <- doenca %>%
          mutate(
            address = corrigir_bytes(address),
            address = stringi::stri_trans_toupper(address),
            address = gsub("[.-]", "", address),
            
            MUNIRESAT = if ("MUNIRESAT" %in% names(.)) {
              ifelse(is.na(MUNIRESAT), "Ignorado", MUNIRESAT)
            } else {
              ID_MUNICIP
            }
          )
        
        doenca <- doenca %>%
          mutate(
            TIPO_ENDERECO = case_when(
              grepl("\\bRUA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RUA",
              grepl("\\bAVENIDA\\b|\\bAV\\b", NM_LOGRADO, ignore.case = TRUE) ~ "AVENIDA",
              grepl("\\bQUADRA\\b|\\bQD\\b|\\bQ\\b", NM_LOGRADO, ignore.case = TRUE) ~ "QUADRA",
              grepl("\\bCONDOMINIO\\b|\\bCOND\\b|\\bCD\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CONDOMINIO",
              grepl("\\bVILA\\b|\\bVL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "VILA",
              grepl("\\bCHACARA\\b|\\bCH\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CHACARA",
              grepl("\\bCONJUNTO\\b|\\bCJ\\b|\\bCONJ\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CONJUNTO",
              grepl("\\bSETOR\\b|\\bST\\b", NM_LOGRADO, ignore.case = TRUE) ~ "SETOR",
              grepl("\\bALAMEDA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "ALAMEDA",
              grepl("\\bTRAVESSA\\b|\\bTV\\b", NM_LOGRADO, ignore.case = TRUE) ~ "TRAVESSA",
              grepl("\\bPRACA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "PRACA",
              grepl("\\bRODOVIA\\b|\\bROD\\b|\\bBR\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RODOVIA",
              grepl("\\bLOTE\\b|\\bLT\\b", NM_LOGRADO, ignore.case = TRUE) ~ "LOTE",
              grepl("\\bBLOCO\\b|\\bBL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "BLOCO",
              grepl("\\bAPARTAMENTO\\b|\\bAPT\\b", NM_LOGRADO, ignore.case = TRUE) ~ "APARTAMENTO",
              grepl("\\bCASA\\b|\\bCS\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CASA",
              grepl("\\bNUCLEO\\b|\\bN\\b", NM_LOGRADO, ignore.case = TRUE) ~ "NUCLEO",
              grepl("\\bAREA\\s+RESIDENCIAL\\b|\\bAR\\b", NM_LOGRADO, ignore.case = TRUE) ~ "AREA_RESIDENCIAL",
              grepl("\\bRESIDENCIAL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RESIDENCIAL",
              grepl("\\bLOTEAMENTO\\b", NM_LOGRADO, ignore.case = TRUE) ~ "LOTEAMENTO",
              grepl("\\bPARQUE\\b", NM_LOGRADO, ignore.case = TRUE) ~ "PARQUE",
              grepl("\\bJARDIM\\b", NM_LOGRADO, ignore.case = TRUE) ~ "JARDIM",
              grepl("\\bVIELA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "VIELA",
              grepl("\\bBECO\\b", NM_LOGRADO, ignore.case = TRUE) ~ "BECO",
              grepl("\\bESTRADA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "ESTRADA",
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "OUTRO",
              TRUE ~ "NAO_INFORMADO"
            )
          )
        
        doenca <- doenca %>%
          mutate(
            QUALIDADE_ENDERECO = case_when(
              # 1º nível: CEP consta na base CNEFE
              !is.na(NU_CEP) & NU_CEP != "" ~ "CEP CNEFE",
              
              # 2º nível: bairro + logradouro + número + complemento
              !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
                !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
                !is.na(NU_NUMERO) & NU_NUMERO != "" &
                !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + logradouro + numero + complemento",
              
              # 3º nível: bairro + logradouro + número
              !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
                !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
                !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "bairro + logradouro + numero",
              
              # 4º nível: bairro + logradouro + complemento
              !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
                !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
                !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + logradouro + complemento",
              
              # 5º nível: bairro + número + complemento
              !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
                !is.na(NU_NUMERO) & NU_NUMERO != "" &
                !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + numero + complemento",
              
              # 6º nível: logradouro + número + complemento
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
                !is.na(NU_NUMERO) & NU_NUMERO != "" &
                !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + numero + complemento",
              
              # 7º nível: bairro + logradouro
              !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
                !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "bairro + logradouro",
              
              # 8º nível: bairro + número
              !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
                !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "bairro + numero",
              
              # 9º nível: bairro + complemento
              !is.na(NM_BAIRRO) & NM_BAIRRO != "" &
                !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + complemento",
              
              # 10º nível: logradouro + número
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
                !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "logradouro + numero",
              
              # 11º nível: logradouro + complemento
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" &
                !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + complemento",
              
              # 12º nível: número + complemento
              !is.na(NU_NUMERO) & NU_NUMERO != "" &
                !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "numero + complemento",
              
              # 13º nível: apenas bairro
              !is.na(NM_BAIRRO) & NM_BAIRRO != "" ~ "bairro",
              
              # 14º nível: apenas logradouro
              !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "logradouro",
              
              # 15º nível: apenas número
              !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "numero",
              
              # 16º nível: apenas complemento
              !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "complemento",
              
              # 17º nível: nenhum campo — apenas UF disponível
              TRUE ~ "UF"
            )
          )
        
        doenca <- doenca %>%
          mutate(
            ID_DISTRIT = as.numeric(ID_DISTRIT),
            Regiao_adm = case_when(
              ID_DISTRIT == 1   ~ "Ceilândia",
              ID_DISTRIT == 9   ~ "Taguatinga",
              ID_DISTRIT == 10  ~ "Gama",
              ID_DISTRIT == 11  ~ "Sobradinho",
              ID_DISTRIT == 12  ~ "Candangolândia",
              ID_DISTRIT == 13  ~ "Águas Claras",
              ID_DISTRIT == 14  ~ "Brazlândia",
              ID_DISTRIT == 15  ~ "Cruzeiro",
              ID_DISTRIT == 16  ~ "Recanto das Emas",
              ID_DISTRIT == 17  ~ "Planaltina",
              ID_DISTRIT == 18  ~ "Samambaia",
              ID_DISTRIT == 19  ~ "Santa Maria",
              ID_DISTRIT == 20  ~ "Riacho Fundo I",
              ID_DISTRIT == 21  ~ "Riacho Fundo II",
              ID_DISTRIT == 22  ~ "Guará",
              ID_DISTRIT == 23  ~ "São Sebastião",
              ID_DISTRIT == 24  ~ "Paranoá",
              ID_DISTRIT == 25  ~ "Lago Norte",
              ID_DISTRIT == 26  ~ "Lago Sul",
              ID_DISTRIT == 27  ~ "Plano Piloto",
              ID_DISTRIT == 28  ~ "Plano Piloto",
              ID_DISTRIT == 29  ~ "Núcleo Bandeirante",
              ID_DISTRIT == 30  ~ "SIA",
              ID_DISTRIT == 31  ~ "Park Way",
              ID_DISTRIT == 32  ~ "Sudoeste/Octogonal",
              ID_DISTRIT == 33  ~ "Varjão",
              ID_DISTRIT == 34  ~ "Sobradinho II",
              ID_DISTRIT == 35  ~ "Jardim Botânico",
              ID_DISTRIT == 96  ~ "SCIA",
              ID_DISTRIT == 97  ~ "Itapoã",
              ID_DISTRIT == 364 ~ "Vicente Pires",
              ID_DISTRIT == 435 ~ "Fercal",
              ID_DISTRIT == 560 ~ "Plano Piloto",
              ID_DISTRIT == 561 ~ "Arniqueira",
              ID_DISTRIT == 562 ~ "Sol Nascente/Pôr do Sol",
              ID_DISTRIT == 566 ~ "Água Quente",
              ID_DISTRIT == 568 ~ "Arapoanga",
              TRUE ~ "Ignorado"
            )
          )
        
        doenca <- doenca %>%
          mutate(
            Regiao_sau1 = case_when(
              Regiao_adm == "Ceilândia" ~ "Região Oeste",
              Regiao_adm == "Taguatinga" ~ "Região Sudoeste",
              Regiao_adm == "Gama" ~ "Região Sul",
              Regiao_adm == "Sobradinho" ~ "Região Norte",
              Regiao_adm == "Candangolândia" ~ "Região Centro-Sul",
              Regiao_adm == "Águas Claras" ~ "Região Sudoeste",
              Regiao_adm == "Brazlândia" ~ "Região Oeste",
              Regiao_adm == "Cruzeiro" ~ "Região Central",
              Regiao_adm == "Recanto das Emas" ~ "Região Sudoeste",
              Regiao_adm == "Planaltina" ~ "Região Norte",
              Regiao_adm == "Samambaia" ~ "Região Sudoeste",
              Regiao_adm == "Santa Maria" ~ "Região Sul",
              Regiao_adm == "Riacho Fundo I" ~ "Região Centro-Sul",
              Regiao_adm == "Riacho Fundo II" ~ "Região Centro-Sul",
              Regiao_adm == "Guará" ~ "Região Centro-Sul",
              Regiao_adm == "São Sebastião" ~ "Região Leste",
              Regiao_adm == "Paranoá" ~ "Região Leste",
              Regiao_adm == "Lago Norte" ~ "Região Central",
              Regiao_adm == "Lago Sul" ~ "Região Central",
              Regiao_adm == "Plano Piloto" ~ "Região Central",
              Regiao_adm == "Núcleo Bandeirante" ~ "Região Centro-Sul",
              Regiao_adm == "SIA" ~ "Região Centro-Sul",
              Regiao_adm == "Park Way" ~ "Região Centro-Sul",
              Regiao_adm == "Sudoeste/Octogonal" ~ "Região Central",
              Regiao_adm == "Varjão" ~ "Região Central",
              Regiao_adm == "Sobradinho II" ~ "Região Norte",
              Regiao_adm == "Jardim Botânico" ~ "Região Leste",
              Regiao_adm == "Estrutural (SCIA)" ~ "Região Centro-Sul",
              Regiao_adm == "Itapoã" ~ "Região Leste",
              Regiao_adm == "Vicente Pires" ~ "Região Sudoeste",
              Regiao_adm == "Fercal" ~ "Região Norte",
              Regiao_adm == "Arniqueira" ~ "Região Sudoeste",
              Regiao_adm == "Sol Nascente/Pôr do Sol" ~ "Região Oeste",
              Regiao_adm == "Água Quente" ~ "Região Sudoeste",
              Regiao_adm == "Arapoanga" ~ "Região Norte",
              TRUE ~ "Ignorado"
            )
          )
        
        incProgress(0.25, detail = "Arquivo do SINAN preparado para georreferenciamento.")
      })
    } else{
      withProgress(message = 'Pré-processando dados...', value = 0, {
    
    #doenca <- import("amostraPE.csv")        
    doenca$NU_NOTIFIC <- str_pad(doenca$NU_NOTIFIC, width = 7, side = "left", pad = "0")
    doenca$NM_LOGRADO <- iconv(doenca$NM_LOGRADO, from = "", to = "UTF-8", sub = " ")
    doenca$NM_LOGRADO <- enc2utf8(doenca$NM_LOGRADO)
    doenca$NM_LOGRADO <- gsub(";", "", doenca$NM_LOGRADO)

    doenca <- doenca %>%
      mutate(
        NM_LOGRADO = corrigir_bytes(NM_LOGRADO),
        NM_LOGRADO = stringi::stri_trans_toupper(NM_LOGRADO),
        NM_LOGRADO = gsub("[.-]", "", NM_LOGRADO),
        NM_LOGRADO = gsub(";", "", NM_LOGRADO),
        NM_LOGRADO = gsub(":", "", NM_LOGRADO),
        NM_LOGRADO = gsub("TRV", "TRAVESSA", NM_LOGRADO),
        NM_LOGRADO = gsub("¬", "", NM_LOGRADO),
        NU_NUMERO = gsub("'", "", NU_NUMERO)
      )

    doenca <- doenca %>%
      mutate(
        NM_LOGRADO = padronizar_logradouros(NM_LOGRADO),
        NM_COMPLEM = padronizar_logradouros(NM_COMPLEM),
        NU_NUMERO = padronizar_numeros(NU_NUMERO),
        NM_BAIRRO = padronizar_bairros(NM_BAIRRO)
      )

    doenca <- doenca %>%
      mutate(
        TIPO_ENDERECO = case_when(
          grepl("\\bRUA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RUA",
          grepl("\\bAVENIDA\\b|\\bAV\\b", NM_LOGRADO, ignore.case = TRUE) ~ "AVENIDA",
          grepl("\\bQUADRA\\b|\\bQD\\b|\\bQ\\b", NM_LOGRADO, ignore.case = TRUE) ~ "QUADRA",
          grepl("\\bCONDOMINIO\\b|\\bCOND\\b|\\bCD\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CONDOMINIO",
          grepl("\\bVILA\\b|\\bVL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "VILA",
          grepl("\\bCHACARA\\b|\\bCH\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CHACARA",
          grepl("\\bCONJUNTO\\b|\\bCJ\\b|\\bCONJ\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CONJUNTO",
          grepl("\\bSETOR\\b|\\bST\\b", NM_LOGRADO, ignore.case = TRUE) ~ "SETOR",
          grepl("\\bALAMEDA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "ALAMEDA",
          grepl("\\bTRAVESSA\\b|\\bTV\\b", NM_LOGRADO, ignore.case = TRUE) ~ "TRAVESSA",
          grepl("\\bPRACA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "PRACA",
          grepl("\\bRODOVIA\\b|\\bROD\\b|\\bBR\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RODOVIA",
          grepl("\\bLOTE\\b|\\bLT\\b", NM_LOGRADO, ignore.case = TRUE) ~ "LOTE",
          grepl("\\bBLOCO\\b|\\bBL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "BLOCO",
          grepl("\\bAPARTAMENTO\\b|\\bAPT\\b", NM_LOGRADO, ignore.case = TRUE) ~ "APARTAMENTO",
          grepl("\\bCASA\\b|\\bCS\\b", NM_LOGRADO, ignore.case = TRUE) ~ "CASA",
          grepl("\\bNUCLEO\\b|\\bN\\b", NM_LOGRADO, ignore.case = TRUE) ~ "NUCLEO",
          grepl("\\bAREA\\s+RESIDENCIAL\\b|\\bAR\\b", NM_LOGRADO, ignore.case = TRUE) ~ "AREA_RESIDENCIAL",
          grepl("\\bRESIDENCIAL\\b", NM_LOGRADO, ignore.case = TRUE) ~ "RESIDENCIAL",
          grepl("\\bLOTEAMENTO\\b", NM_LOGRADO, ignore.case = TRUE) ~ "LOTEAMENTO",
          grepl("\\bPARQUE\\b", NM_LOGRADO, ignore.case = TRUE) ~ "PARQUE",
          grepl("\\bJARDIM\\b", NM_LOGRADO, ignore.case = TRUE) ~ "JARDIM",
          grepl("\\bVIELA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "VIELA",
          grepl("\\bBECO\\b", NM_LOGRADO, ignore.case = TRUE) ~ "BECO",
          grepl("\\bESTRADA\\b", NM_LOGRADO, ignore.case = TRUE) ~ "ESTRADA",
          !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "OUTRO",
          TRUE ~ "NAO_INFORMADO"
        )
      )

    doenca <- doenca %>%
      rowwise() %>%
      mutate(
        address = case_when(
          is.na(NM_LOGRADO) | NM_LOGRADO == "" ~ ifelse(
            is.na(NU_NUMERO) | NU_NUMERO == "",
            juntar_endereco(NM_COMPLEM, NM_BAIRRO),
            juntar_endereco(NM_COMPLEM, NU_NUMERO, NM_BAIRRO)
          ),
          TRUE ~ ifelse(
            is.na(NU_NUMERO) | NU_NUMERO == "",
            juntar_endereco(NM_LOGRADO, NM_BAIRRO),
            juntar_endereco(NM_LOGRADO, NU_NUMERO, NM_BAIRRO)
          )
        ),
        address2 = case_when(
          is.na(NM_LOGRADO) | NM_LOGRADO == "" ~ ifelse(
            is.na(NU_NUMERO) | NU_NUMERO == "",
            NM_COMPLEM,
            juntar_endereco(NM_COMPLEM, NU_NUMERO)
          ),
          TRUE ~ ifelse(
            is.na(NU_NUMERO) | NU_NUMERO == "",
            NM_LOGRADO,
            juntar_endereco(NM_LOGRADO, NU_NUMERO)
          )
        ),
        address3 = case_when(
          is.na(NM_LOGRADO) | NM_LOGRADO == "" ~ juntar_endereco(NM_COMPLEM, NM_BAIRRO),
          TRUE ~ juntar_endereco(NM_LOGRADO, NM_BAIRRO)
        )
      ) %>%
      ungroup()

    doenca <- doenca %>%
      mutate(
        across(
          c(address, address2, address3),
          ~ {
            x <- corrigir_bytes(.x)
            x <- stringi::stri_trans_toupper(x)
            x <- gsub("[.-]", "", x)
            x <- gsub(";", "", x)
            x <- gsub("TRV", "TRAVESSA", x)
            x <- gsub("¬", "", x)
            x <- gsub("'", "", x)
            x
          }
        ),
        MUNIRESAT = if ("MUNIRESAT" %in% names(.)) {
          ifelse(is.na(MUNIRESAT), "Ignorado", MUNIRESAT)
        } else {
          ID_MUNICIP
        }
      )

    doenca <- doenca %>%
      mutate(
        QUALIDADE_ENDERECO = case_when(
          !is.na(NU_CEP) & NU_CEP != "" ~ "CEP CNEFE",
          !is.na(NM_BAIRRO) & NM_BAIRRO != "" & !is.na(NM_LOGRADO) & NM_LOGRADO != "" & !is.na(NU_NUMERO) & NU_NUMERO != "" & !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + logradouro + numero + complemento",
          !is.na(NM_BAIRRO) & NM_BAIRRO != "" & !is.na(NM_LOGRADO) & NM_LOGRADO != "" & !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "bairro + logradouro + numero",
          !is.na(NM_BAIRRO) & NM_BAIRRO != "" & !is.na(NM_LOGRADO) & NM_LOGRADO != "" & !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + logradouro + complemento",
          !is.na(NM_BAIRRO) & NM_BAIRRO != "" & !is.na(NU_NUMERO) & NU_NUMERO != "" & !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + numero + complemento",
          !is.na(NM_LOGRADO) & NM_LOGRADO != "" & !is.na(NU_NUMERO) & NU_NUMERO != "" & !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + numero + complemento",
          !is.na(NM_BAIRRO) & NM_BAIRRO != "" & !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "bairro + logradouro",
          !is.na(NM_BAIRRO) & NM_BAIRRO != "" & !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "bairro + numero",
          !is.na(NM_BAIRRO) & NM_BAIRRO != "" & !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "bairro + complemento",
          !is.na(NM_LOGRADO) & NM_LOGRADO != "" & !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "logradouro + numero",
          !is.na(NM_LOGRADO) & NM_LOGRADO != "" & !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "logradouro + complemento",
          !is.na(NU_NUMERO) & NU_NUMERO != "" & !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "numero + complemento",
          !is.na(NM_BAIRRO) & NM_BAIRRO != "" ~ "bairro",
          !is.na(NM_LOGRADO) & NM_LOGRADO != "" ~ "logradouro",
          !is.na(NU_NUMERO) & NU_NUMERO != "" ~ "numero",
          !is.na(NM_COMPLEM) & NM_COMPLEM != "" ~ "complemento",
          TRUE ~ "UF"
        )
      )

    doenca[] <- lapply(doenca, sanitizar_utf8)
        
        incProgress(0.25, detail = "Arquivo do SINAN preparado para georreferenciamento.")
      })
    }
    return(doenca)
  }
  
  # FUNÇÃO DE GEOPROCESSAMENTO AGRUPADO POR MUNICÍPIO (AM, SP, RS, PE E TO) -- TROCAR POR FUNÇÃO DE FASES
  georreferenciar_UF <- function(doenca, codigo_municipio = NULL, uf_selecionada, limiar_similaridade) {
    
    begin <- Sys.time()
    
    # Verificar se devemos filtrar por município
    if(!is.null(codigo_municipio) && !is.na(codigo_municipio)){
      codigo_municipio <- as.character(codigo_municipio)
      codigo_municipio <- substr(codigo_municipio, 1, 6)
      codigo_municipio <- as.integer(codigo_municipio)

      # Sem essas duas checagens, um código inválido ou que não bate com
      # nenhum registro deixa `doenca` com 0 linhas; mais adiante,
      # uf_detectada <- doenca$SG_UF[...][1] num vetor vazio vira NA, o
      # case_when propaga esse NA para uf_selecionada, e
      # "if (uf_selecionada == 'DF')" quebra com "valor ausente onde
      # TRUE/FALSE necessário" -- em vez de avisar claramente que o
      # código do município está errado.
      if (is.na(codigo_municipio)) {
        stop("Código de município inválido — informe apenas os dígitos do código IBGE.")
      }

      doenca <- doenca %>% filter(MUNIRESAT == codigo_municipio)

      if (nrow(doenca) == 0) {
        stop(paste0(
          "Nenhum registro encontrado para o município ", codigo_municipio,
          " no arquivo carregado. Verifique se o código está correto."
        ))
      }
    }
    
    quantidade_de_casos <- nrow(doenca)
    
    if (input$metodo == "cnefe"){
      
      # ==========================================
      # INICIALIZA MONITORAMENTO DE PROGRESSO COM SHINY
      # ==========================================
      
      withProgress(message = 'Iniciando georreferenciamento...', value = 0, {
        
        # -------------------------------------------------------
        # IDENTIFICAÇÃO AUTOMÁTICA DA UF
        # -------------------------------------------------------
        
        setProgress(
          value = 0.02,
          message = 'Identificando Unidade Federativa...',
          detail = 'Analisando dados de entrada'
        )
        
        # Auto-detecção só se SG_UF existir (dados no formato SINAN). Dados com
        # colunas customizadas (padronizar_colunas_usuario) não têm SG_UF — nesse
        # caso mantemos o uf_selecionada já recebido como argumento (input$uf).
        # Sem essa checagem, doenca$SG_UF em dados sem essa coluna vira NULL, o
        # case_when abaixo retorna vetor de tamanho zero, e o "if" mais adiante
        # quebra com "argument is of length zero".
        if ("SG_UF" %in% names(doenca)) {
          uf_detectada <- doenca$SG_UF[!is.na(doenca$SG_UF) & doenca$SG_UF != ""][1]

          uf_selecionada <- dplyr::case_when(
            uf_detectada %in% c(53, "53") ~ "DF",
            uf_detectada %in% c(26, "26") ~ "PE",
            uf_detectada %in% c(35, "35") ~ "SP",
            uf_detectada %in% c(13, "13") ~ "AM",
            uf_detectada %in% c(17, "17") ~ "TO",
            uf_detectada %in% c(43, "43") ~ "RS",
            uf_detectada %in% c(52, "52") ~ "GO",
            uf_detectada %in% c(29, "29") ~ "BA",
            uf_detectada %in% c(32, "32") ~ "ES",
            uf_detectada %in% c(31, "31") ~ "MG",
            uf_detectada %in% c(33, "33") ~ "RJ",
            uf_detectada %in% c(27, "27") ~ "AL",
            uf_detectada %in% c(23, "23") ~ "CE",
            uf_detectada %in% c(50, "50") ~ "MS",
            uf_detectada %in% c(51, "51") ~ "MT",
            uf_detectada %in% c(25, "25") ~ "PB",
            uf_detectada %in% c(41, "41") ~ "PR",
            uf_detectada %in% c(24, "24") ~ "RN",
            uf_detectada %in% c(42, "42") ~ "SC",
            uf_detectada %in% c(28, "28") ~ "SE",
            uf_detectada %in% c(21, "21") ~ "MA",
            uf_detectada %in% c(11, "11") ~ "RO",
            TRUE ~ toupper(as.character(uf_detectada))
          )
        }

        if(uf_selecionada == "DF"){
          filtro_sem_ignorado <- paste0("Regiao_adm != 'Ignorado'")
          filtro_com_ignorado <- paste0("Regiao_adm = 'Ignorado'")
          filtro_mun_ra <- paste0("C.NM_SUBDIST = S.Regiao_adm")
          col_mun_ra <- paste0("C.NM_SUBDIST, C.CD_SUBDIST")
          col_mun_ra2 <- paste0("NM_SUBDIST, CD_SUBDIST")
          col_a12_aux <- paste0("COUNT(DISTINCT C.NM_SUBDIST) AS QTD_RA")
          filtro_a12 <- paste0("S.QTD_RA")
          mun_ra <- paste0("Regiao_adm")
          meio_filtro_mun_ra <- paste0("NM_SUBDIST")
          meio_filtro_mun_ra2 <- paste0("CD_SUBDIST")
        } else{
          filtro_sem_ignorado <- paste0("MUNIRESAT != 'Ignorado'")
          filtro_com_ignorado <- paste0("MUNIRESAT = 'Ignorado'")
          filtro_mun_ra <- paste0("C.CD_MUN = S.MUNIRESAT")
          col_mun_ra <- paste0("C.NM_MUN, C.CD_MUN")
          col_mun_ra2 <- paste0("NM_MUN, CD_MUN")
          col_a12_aux <- paste0("COUNT(DISTINCT C.CD_MUN) AS QTD_MUN")
          filtro_a12 <- paste0("S.QTD_MUN")
          mun_ra <- paste0("MUNIRESAT")
          meio_filtro_mun_ra <- paste0("CD_MUN")
          meio_filtro_mun_ra2 <- paste0("NM_MUN")
        }
        
        tabela_cnefe <- paste0("CNEFE", uf_selecionada)

        # Atualiza progresso com UF identificada
        setProgress(
          value = 0.05,
          message = paste('UF identificada:', uf_selecionada),
          detail = paste('Tabela CNEFE:', tabela_cnefe)
        )
        
        cat(paste0("UF identificada: ", uf_selecionada,
                   " | Tabela CNEFE: ", tabela_cnefe, "\n"))
        
        # -------------------------------------------------------
        # FILTRO OPCIONAL POR MUNICÍPIO
        # -------------------------------------------------------
        
        if (!is.null(codigo_municipio) && !is.na(codigo_municipio)) {
          codigo_municipio <- as.character(codigo_municipio)
          codigo_municipio <- substr(codigo_municipio, 1, 6)
          codigo_municipio <- as.integer(codigo_municipio)

          # Mesma checagem do primeiro filtro (linha ~1798) -- ver nota lá.
          if (is.na(codigo_municipio)) {
            stop("Código de município inválido — informe apenas os dígitos do código IBGE.")
          }

          doenca <- doenca %>% filter(MUNIRESAT == codigo_municipio)

          if (nrow(doenca) == 0) {
            stop(paste0(
              "Nenhum registro encontrado para o município ", codigo_municipio,
              " no arquivo carregado. Verifique se o código está correto."
            ))
          }
        }
        
        quantidade_de_casos <- nrow(doenca)
        
        # Atualiza progresso com total de casos
        setProgress(
          value = 0.08,
          message = 'Preparando dados...',
          detail = paste('Total de casos:', quantidade_de_casos)
        )
        
        if(uf_selecionada == "DF"){
          doenca_temp <- data.frame(
            address    = doenca$address,
            NU_NOTIFIC = as.character(doenca$NU_NOTIFIC),
            NU_NUMERO  = doenca$NU_NUMERO,
            NM_LOGRADO = doenca$NM_LOGRADO,
            Regiao_adm  = as.character(doenca$Regiao_adm),
            NM_BAIRRO  = doenca$NM_BAIRRO,
            address3   = doenca$address3,
            stringsAsFactors = FALSE
          )
        } else{
          doenca_temp <- data.frame(
            address    = doenca$address,
            NU_NOTIFIC = as.character(doenca$NU_NOTIFIC),
            NU_NUMERO  = doenca$NU_NUMERO,
            NM_LOGRADO = doenca$NM_LOGRADO,
            MUNIRESAT  = as.character(doenca$MUNIRESAT),
            NM_BAIRRO  = doenca$NM_BAIRRO,
            address3   = doenca$address3,
            stringsAsFactors = FALSE
          )
        }
        
        doenca$NU_NOTIFIC <- as.character(doenca$NU_NOTIFIC)
        
        if(uf_selecionada == "DF"){
          cat("Processando DF.\n")
        } else{
          doenca$MUNIRESAT  <- as.character(doenca$MUNIRESAT)
        }
        
        # -------------------------------------------------------
        # CONEXÃO DUCKDB
        # -------------------------------------------------------
        
        setProgress(
          value = 0.10,
          message = 'Conectando ao duckDB...',
          detail = 'Carregando base CNEFE'
        )
        
        con <- dbConnect(duckdb(), dbdir = "CNEFE.duckdb", read_only = TRUE)

        # Fecha a conexão aconteça o que acontecer. Sem isto, uma execução
        # interrompida por erro deixava a conexão aberta e a instância do banco
        # viva; a execução seguinte pegava essa mesma instância e quebrava em
        # "Cannot switch temporary directory after the current one has been
        # used". Sem shutdown: outras sessões do Shiny no mesmo processo R
        # compartilham a instância, e derrubá-la interromperia o processamento
        # delas no meio.
        on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

        # Mesmo limite já usado no pipeline do DF (georreferenciar_DF, em
        # georreferenciamento_funcoes.R) -- sem isso, cada sessão usa o
        # padrão do DuckDB (tipicamente ~80% da RAM disponível), o que fica
        # arriscado com múltiplas sessões concorrentes na mesma máquina à
        # medida que o CNEFE.duckdb cresce (hoje com CNEFEs de várias UFs,
        # ~10.7GB). A conexão é read_only, então isso não afeta o arquivo em
        # si, só limita o quanto essa sessão pode usar durante o
        # processamento (tabelas TEMP, joins, etc.).
        dbExecute(con, "SET memory_limit = '4GB'")

        # Diretório de "spill to disk" do DuckDB (usado quando uma operação
        # ultrapassa o memory_limit acima), um por processo R -- ver
        # configurar_temp_duckdb() em georreferenciamento_funcoes.R.
        configurar_temp_duckdb(con)

        dbExecute(con, "SET threads = 2")

        # Mesmo ajuste do pipeline do DF: sem isto o DuckDB gasta memória
        # mantendo a ordem original das linhas nos resultados intermediários,
        # ordem que nenhuma fase daqui usa. É uma das saídas que o próprio
        # DuckDB sugere na mensagem de "Out of Memory".
        dbExecute(con, "SET preserve_insertion_order = false")

        # Tabela auxiliar com as ruas distintas por município (MUN_FILTRO =
        # CD_MUN ou NM_SUBDIST no caso do DF), usada nas FASES 4A/4B pra não
        # ter que escanear a CNEFE inteira toda vez que um registro precisa
        # descobrir quais ruas existem no seu município. Se já existir uma
        # versão PERSISTENTE (criada uma vez pela manutenção do CNEFE.duckdb,
        # ver PREP_RUAS_DISTINTAS.R), usa ela direto -- senão, monta uma
        # versão temporária aqui, uma única vez por execução (não por
        # registro), o que já ajuda bastante mesmo sem a manutenção rodada.
        tabela_ruas_cnefe <- paste0(tabela_cnefe, "_RUAS")
        if (!DBI::dbExistsTable(con, tabela_ruas_cnefe)) {
          dbExecute(con, sprintf("
      CREATE TEMP TABLE %s AS
      SELECT DISTINCT %s AS MUN_FILTRO, ENDERECO_CNEFE3
      FROM %s
    ", tabela_ruas_cnefe, meio_filtro_mun_ra, tabela_cnefe))
        }

        # -------------------------------------------------------
        # DROP DE TABELAS TEMPORÁRIAS (segurança)
        # -------------------------------------------------------
        
        setProgress(
          value = 0.12,
          message = 'Limpando tabelas temporárias...',
          detail = 'Preparando ambiente de processamento'
        )
        
        tabelas_temp <- c(
          "temp0", "tempA", "tempB",
          "tempA11", "tempA12_AUX", "tempA12", "tempA1",
          "tempB1", "temp1",
          "tempA2_AUX", "tempA21", "tempA22", "tempA2",
          "tempB2_AUX", "tempB21", "tempB22", "tempB2", "temp2",
          "tempA3_AUX", "tempA3_AUX2", "tempA31", "tempA32", "tempA33", "tempA3",
          "tempB3_AUX", "tempB3_AUX2", "tempB31", "tempB32", "tempB33", "tempB3", "temp3",
          "tempA41", "tempA42", "tempA43", "tempA4",
          "tempB41_raw", "tempB41", "tempB42", "tempB43", "tempB4", "temp4",
          "tempA51", "tempA5",
          "tempB51", "tempB5", "temp5",
          "TEMPGEO", "FINALGEO", "doenca_join"
        )
        
        for (tb in tabelas_temp) {
          dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", tb))
        }
        
        # -------------------------------------------------------
        # TEMP0 – dados SINAN de interesse
        # -------------------------------------------------------
        
        setProgress(
          value = 0.15,
          message = 'Carregando dados na base temporária...',
          detail = 'Criando temp0'
        )
        
        dbWriteTable(con, "temp0", doenca_temp, temporary = TRUE, overwrite = TRUE)
        
        total_inicial <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp0")$n
        
        setProgress(
          value = 0.18,
          message = 'Dados carregados',
          detail = paste('Total de registros:', total_inicial)
        )
        
        cat(paste0("Total de registros para georreferenciar: ", total_inicial, "\n"))
        
        # -------------------------------------------------------
        # DIVISÃO A/B: casos com e sem município
        # -------------------------------------------------------
        
        setProgress(
          value = 0.20,
          message = 'Dividindo casos por município...',
          detail = 'Filtrando dados'
        )
        
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA AS
    SELECT * FROM temp0 WHERE %s
  ", filtro_sem_ignorado))
        
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB AS
    SELECT * FROM temp0 WHERE %s
  ", filtro_com_ignorado))
        
        total_A <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA")$n
        total_B <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB")$n
        
        setProgress(
          value = 0.22,
          message = 'Casos divididos',
          detail = paste('Com município:', total_A, '| Sem município:', total_B)
        )
        
        cat(paste0("Com município ou RA: ", total_A,
                   " | Sem município ou RA (Ignorado): ", total_B, "\n"))
        
        # ==============================================================
        # FASE 1 – Determinístico: ADDRESS == ENDERECO_CNEFE
        # ==============================================================
        
        setProgress(
          value = 0.25,
          message = 'Executando FASE 1 - Match exato com endereço...',
          detail = 'Processando A.1.1'
        )
        
        # --- A.1.1: match exato com município correto ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA11 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.ADDRESS,
      S.NU_NUMERO,
      C.ENDERECO_CNEFE,
      1            AS SIMILARITY,
      0            AS DISTANCIA_NUM,
      'FASE A.1.1' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempA S
    INNER JOIN %s C ON C.ENDERECO_CNEFE = S.ADDRESS
    WHERE
    %s
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe, filtro_mun_ra))
        
        ### COMPONENTES DINÂMICOS NECESSÁRIOS:
        #
        
        total_A11 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA11")$n
        dbExecute(con, "DELETE FROM tempA WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA11)")
        
        # --- A.1.2: match exato com município errado
        #            (endereço existe em apenas 1 município na CNEFE) ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA12_AUX AS
    SELECT DISTINCT
      S.NU_NOTIFIC,
      S.ADDRESS,
      S.NU_NUMERO,
      %s
    FROM tempA S
    INNER JOIN %s C ON C.ENDERECO_CNEFE = S.ADDRESS
    GROUP BY S.NU_NOTIFIC, S.ADDRESS, S.NU_NUMERO
  ", col_a12_aux, tabela_cnefe))
        
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA12 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.ADDRESS,
      S.NU_NUMERO,
      C.ENDERECO_CNEFE,
      1            AS SIMILARITY,
      0            AS DISTANCIA_NUM,
      'FASE A.1.2' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempA12_AUX S
    INNER JOIN %s C ON C.ENDERECO_CNEFE = S.ADDRESS
    WHERE %s = 1
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe, filtro_a12))
        
        total_A12 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA12")$n
        dbExecute(con, "DELETE FROM tempA WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA12)")
        
        # --- Consolida A.1 ---
        dbExecute(con, "
    CREATE TEMP TABLE tempA1 AS
    SELECT * FROM tempA11
    UNION ALL
    SELECT * FROM tempA12
  ")
        
        # --- B.1: casos sem município, match exato (sem filtro de CD_MUN) ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB1 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.ADDRESS,
      S.NU_NUMERO,
      C.ENDERECO_CNEFE,
      1          AS SIMILARITY,
      0          AS DISTANCIA_NUM,
      'FASE B.1' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempB S
    INNER JOIN %s C ON C.ENDERECO_CNEFE = S.ADDRESS
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe))
        
        total_B1 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB1")$n
        dbExecute(con, "DELETE FROM tempB WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempB1)")
        
        # --- Consolida Fase 1 ---
        dbExecute(con, "
    CREATE TEMP TABLE temp1 AS
    SELECT *
    FROM tempA1
    UNION ALL
    SELECT *
    FROM tempB1
  ")
        
        total_fase1  <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp1")$n
        dbExecute(con, "DELETE FROM temp0 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM temp1)")
        pendentes_f1 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp0")$n
        
        setProgress(
          value = 0.35,
          message = 'FASE 1 concluída',
          detail = paste(total_fase1, 'georreferenciados |', pendentes_f1, 'pendentes')
        )
        
        cat(paste0("FASE 1 concluída: ", total_fase1,
                   " georreferenciados | ", pendentes_f1, " pendentes",
                   " (A.1.1 = ", total_A11,
                   " | A.1.2 = ", total_A12,
                   " | B.1 = ",   total_B1, ")\n"))
        
        # ==============================================================
        # FASE 2 – Contingência números: ADDRESS3 == ENDERECO_CNEFE3
        # ==============================================================
        
        setProgress(
          value = 0.40,
          message = 'Executando FASE 2 - Match por endereço sem número...',
          detail = 'Processando contingência'
        )
        
        # --- A.2 auxiliar: match exato em address3 filtrado por município ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA2_AUX AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.ADDRESS3,
      S.NU_NUMERO,
      C.ENDERECO_CNEFE3,
      1 AS SIMILARITY,
      %s
    FROM tempA S
    INNER JOIN %s C ON C.ENDERECO_CNEFE3 = S.ADDRESS3
    WHERE %s
    ORDER BY S.NU_NOTIFIC
  ", mun_ra, tabela_cnefe, filtro_mun_ra))
        
        # --- A.2.1: com número válido → número mais próximo ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA21 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.ADDRESS3,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      C.ENDERECO_CNEFE,
      C.ENDERECO_CNEFE3,
      ABS(C.NUM_ENDERECO - TRY_CAST(
          REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
      ) AS DISTANCIA_NUM,
      GREATEST(
          1.0 - (
              FLOOR(
                  ABS(C.NUM_ENDERECO - TRY_CAST(
                      REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
                  ) / 5.0
              ) * 0.0003
          ),
          0.0
      ) AS SIMILARITY,
      'FASE A.2.1' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempA2_AUX S
    INNER JOIN %s C ON C.ENDERECO_CNEFE3 = S.ADDRESS3
    WHERE S.NU_NUMERO IS NOT NULL
      AND S.NU_NUMERO NOT IN ('S/N', 'NA', '') AND
      %s
    ORDER BY
      S.NU_NOTIFIC,
      ABS(C.NUM_ENDERECO - TRY_CAST(
          REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
      ) ASC
  ", col_mun_ra, tabela_cnefe, filtro_mun_ra))
        
        total_A21 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA21")$n
        dbExecute(con, "DELETE FROM tempA2_AUX WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA21)")
        
        # --- A.2.2: sem número → ancora no número 0 ou 1 do logradouro ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA22 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      S.ADDRESS3,
      C.ENDERECO_CNEFE,
      C.ENDERECO_CNEFE3,
      1            AS SIMILARITY,
      0            AS DISTANCIA_NUM,
      'FASE A.2.2' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempA2_AUX S
    INNER JOIN %s C ON C.ENDERECO_CNEFE3 = S.ADDRESS3
      AND C.NUM_ENDERECO IN (0, 1)
    WHERE S.NU_NUMERO IS NULL OR S.NU_NUMERO IN ('S/N', 'NA', '')
    AND %s
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe, filtro_mun_ra))
        
        total_A22 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA22")$n
        dbExecute(con, "DELETE FROM tempA2_AUX WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA22)")
        
        # --- Consolida A.2 ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA2 AS
    SELECT
      NU_NOTIFIC,
      ADDRESS3 AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempA21
    UNION ALL
    SELECT
      NU_NOTIFIC,
      ADDRESS3 AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempA22
  ", col_mun_ra2, col_mun_ra2))
        
        total_A2 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA2")$n
        dbExecute(con, "DELETE FROM tempA WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA2)")
        
        # --- B.2 auxiliar: match exato em address3 sem filtro de município ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB2_AUX AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.ADDRESS3,
      S.NU_NUMERO,
      C.ENDERECO_CNEFE3,
      1 AS SIMILARITY
    FROM tempB S
    INNER JOIN %s C ON C.ENDERECO_CNEFE3 = S.ADDRESS3
    ORDER BY S.NU_NOTIFIC
  ", tabela_cnefe))
        
        # --- B.2.1: com número válido → número mais próximo ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB21 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.ADDRESS3,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      C.ENDERECO_CNEFE,
      C.ENDERECO_CNEFE3,
      ABS(C.NUM_ENDERECO - TRY_CAST(
          REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
      ) AS DISTANCIA_NUM,
      GREATEST(
          1.0 - (
              FLOOR(
                  ABS(C.NUM_ENDERECO - TRY_CAST(
                      REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
                  ) / 5.0
              ) * 0.0003
          ),
          0.0
      ) AS SIMILARITY,
      'FASE B.2.1' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempB2_AUX S
    INNER JOIN %s C ON C.ENDERECO_CNEFE3 = S.ADDRESS3
    WHERE S.NU_NUMERO IS NOT NULL
      AND S.NU_NUMERO NOT IN ('S/N', 'NA', '')
    ORDER BY
      S.NU_NOTIFIC,
      ABS(C.NUM_ENDERECO - TRY_CAST(
          REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
      ) ASC
  ", col_mun_ra, tabela_cnefe))
        
        total_B21 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB21")$n
        dbExecute(con, "DELETE FROM tempB2_AUX WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempB21)")
        
        # --- B.2.2: sem número → ancora no número 0 ou 1 ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB22 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      S.ADDRESS3,
      C.ENDERECO_CNEFE,
      C.ENDERECO_CNEFE3,
      1            AS SIMILARITY,
      0            AS DISTANCIA_NUM,
      'FASE B.2.2' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempB2_AUX S
    INNER JOIN %s C ON C.ENDERECO_CNEFE3 = S.ADDRESS3
      AND C.NUM_ENDERECO IN (0, 1)
    WHERE S.NU_NUMERO IS NULL OR S.NU_NUMERO IN ('S/N', 'NA', '')
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe))
        
        total_B22 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB22")$n
        dbExecute(con, "DELETE FROM tempB2_AUX WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempB22)")
        
        # --- Consolida B.2 ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB2 AS
    SELECT
      NU_NOTIFIC,
      ADDRESS3 AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempB21
    UNION ALL
    SELECT
      NU_NOTIFIC,
      ADDRESS3 AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempB22
  ", col_mun_ra2, col_mun_ra2))
        
        total_B2 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB2")$n
        dbExecute(con, "DELETE FROM tempB WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempB2)")
        
        # --- Consolida Fase 2 ---
        dbExecute(con, "
    CREATE TEMP TABLE temp2 AS
    SELECT *
    FROM tempA2
    UNION ALL
    SELECT *
    FROM tempB2
  ")
        
        total_fase2  <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp2")$n
        dbExecute(con, "DELETE FROM temp0 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM temp2)")
        pendentes_f2 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp0")$n
        
        setProgress(
          value = 0.50,
          message = 'FASE 2 concluída',
          detail = paste(total_fase2, 'georreferenciados |', pendentes_f2, 'pendentes')
        )
        
        cat(paste0("FASE 2 concluída: ", total_fase2,
                   " georreferenciados | ", pendentes_f2, " pendentes",
                   " (A.2.1 = ", total_A21, " | A.2.2 = ", total_A22,
                   " | B.2.1 = ", total_B21, " | B.2.2 = ", total_B22, ")\n"))
        
        # ==============================================================
        # FASE 3 – Determinístico: NM_LOGRADO == LOGR_CNEFE
        # ==============================================================
        
        setProgress(
          value = 0.55,
          message = 'Executando FASE 3 - Match por logradouro...',
          detail = 'Processando nomes de rua'
        )
        
        # --- A.3 auxiliar: logradouros que existem em apenas 1 bairro no município ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA3_AUX AS
    SELECT DISTINCT
      S.NU_NOTIFIC,
      S.NM_LOGRADO,
      COUNT(DISTINCT C.DSC_LOCALIDADE) AS QTD_BAIRROS,
      S.NU_NUMERO,
      %s
    FROM tempA S
    INNER JOIN %s C ON C.LOGR_CNEFE = S.NM_LOGRADO
    WHERE %s
    GROUP BY S.NM_LOGRADO, S.NU_NOTIFIC, S.NU_NUMERO, %s
  ", mun_ra, tabela_cnefe, filtro_mun_ra, mun_ra))
        
        dbExecute(con, "
    CREATE TEMP TABLE tempA3_AUX2 AS
    SELECT * FROM tempA3_AUX WHERE QTD_BAIRROS = 1
  ")
        
        # --- A.3.1: número exato ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA31 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.NM_LOGRADO,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      C.ENDERECO_CNEFE,
      0            AS DISTANCIA_NUM,
      1            AS SIMILARITY,
      'FASE A.3.1' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempA3_AUX2 S
    INNER JOIN %s C ON C.LOGR_CNEFE = S.NM_LOGRADO
    WHERE S.NU_NUMERO = CAST(C.NUM_ENDERECO AS VARCHAR)
    AND %s
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe, filtro_mun_ra))
        
        total_A31 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA31")$n
        dbExecute(con, "DELETE FROM tempA3_AUX2 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA31)")
        
        # --- A.3.2: aproximação numérica ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA32 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      S.NM_LOGRADO,
      C.ENDERECO_CNEFE,
      ABS(C.NUM_ENDERECO - TRY_CAST(
          REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
      ) AS DISTANCIA_NUM,
      GREATEST(
          1.0 - (
              FLOOR(
                  ABS(C.NUM_ENDERECO - TRY_CAST(
                      REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
                  ) / 5.0
              ) * 0.0003
          ),
          0.0
      ) AS SIMILARITY,
      'FASE A.3.2' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempA3_AUX2 S
    INNER JOIN %s C ON C.LOGR_CNEFE = S.NM_LOGRADO
    WHERE S.NU_NUMERO IS NOT NULL
      AND S.NU_NUMERO NOT IN ('S/N', 'NA', '')
      AND %s
    ORDER BY
      S.NU_NOTIFIC,
      ABS(C.NUM_ENDERECO - TRY_CAST(
          REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
      ) ASC
  ", col_mun_ra, tabela_cnefe, filtro_mun_ra))
        
        total_A32 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA32")$n
        dbExecute(con, "DELETE FROM tempA3_AUX2 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA32)")
        
        # --- A.3.3: sem número → ancora no 0 ou 1 ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA33 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      S.NM_LOGRADO,
      C.ENDERECO_CNEFE,
      0            AS DISTANCIA_NUM,
      1            AS SIMILARITY,
      'FASE A.3.3' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempA3_AUX2 S
    INNER JOIN %s C ON C.LOGR_CNEFE = S.NM_LOGRADO
      AND C.NUM_ENDERECO IN (0, 1)
    WHERE S.NU_NUMERO IS NULL OR S.NU_NUMERO IN ('S/N', 'NA', '')
    AND %s
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe, filtro_mun_ra))
        
        total_A33 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA33")$n
        dbExecute(con, "DELETE FROM tempA3_AUX2 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA33)")
        
        # --- Consolida A.3 ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempA3 AS
    SELECT
      NU_NOTIFIC,
      NM_LOGRADO AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempA31
    UNION ALL
    SELECT
      NU_NOTIFIC,
      NM_LOGRADO AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempA32
    UNION ALL
    SELECT
      NU_NOTIFIC,
      NM_LOGRADO AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempA33
  ", col_mun_ra2, col_mun_ra2, col_mun_ra2))
        
        total_A3 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA3")$n
        dbExecute(con, "DELETE FROM tempA WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA3)")
        
        # --- B.3 auxiliar: logradouros que existem em apenas 1 bairro (sem filtro de município) ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB3_AUX AS
    SELECT DISTINCT
      S.NU_NOTIFIC,
      S.NM_LOGRADO,
      COUNT(DISTINCT C.DSC_LOCALIDADE) AS QTD_BAIRROS,
      S.NU_NUMERO,
      %s
    FROM tempB S
    INNER JOIN %s C ON C.LOGR_CNEFE = S.NM_LOGRADO
    GROUP BY S.NM_LOGRADO, S.NU_NOTIFIC, S.NU_NUMERO, %s
  ", mun_ra, tabela_cnefe, mun_ra))
        
        dbExecute(con, "
    CREATE TEMP TABLE tempB3_AUX2 AS
    SELECT * FROM tempB3_AUX WHERE QTD_BAIRROS = 1
  ")
        
        # --- B.3.1: número exato ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB31 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.NM_LOGRADO,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      C.ENDERECO_CNEFE,
      0            AS DISTANCIA_NUM,
      1            AS SIMILARITY,
      'FASE B.3.1' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempB3_AUX2 S
    INNER JOIN %s C ON C.LOGR_CNEFE = S.NM_LOGRADO
    WHERE S.NU_NUMERO = CAST(C.NUM_ENDERECO AS VARCHAR)
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe))
        
        total_B31 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB31")$n
        dbExecute(con, "DELETE FROM tempB3_AUX2 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempB31)")
        
        # --- B.3.2: aproximação numérica ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB32 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      S.NM_LOGRADO,
      C.ENDERECO_CNEFE,
      ABS(C.NUM_ENDERECO - TRY_CAST(
          REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
      ) AS DISTANCIA_NUM,
      GREATEST(
          1.0 - (
              FLOOR(
                  ABS(C.NUM_ENDERECO - TRY_CAST(
                      REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
                  ) / 5.0
              ) * 0.0003
          ),
          0.0
      ) AS SIMILARITY,
      'FASE B.3.2' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempB3_AUX2 S
    INNER JOIN %s C ON C.LOGR_CNEFE = S.NM_LOGRADO
    WHERE S.NU_NUMERO IS NOT NULL
      AND S.NU_NUMERO NOT IN ('S/N', 'NA', '')
    ORDER BY
      S.NU_NOTIFIC,
      ABS(C.NUM_ENDERECO - TRY_CAST(
          REGEXP_EXTRACT(S.NU_NUMERO, '^\\d+') AS INT32)
      ) ASC
  ", col_mun_ra, tabela_cnefe))
        
        total_B32 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB32")$n
        dbExecute(con, "DELETE FROM tempB3_AUX2 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempB32)")
        
        # --- B.3.3: sem número → ancora no 0 ou 1 ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB33 AS
    SELECT DISTINCT ON (S.NU_NOTIFIC)
      S.NU_NOTIFIC,
      S.NU_NUMERO,
      C.NUM_ENDERECO,
      S.NM_LOGRADO,
      C.ENDERECO_CNEFE,
      0            AS DISTANCIA_NUM,
      1            AS SIMILARITY,
      'FASE B.3.3' AS FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      %s
    FROM tempB3_AUX2 S
    INNER JOIN %s C ON C.LOGR_CNEFE = S.NM_LOGRADO
      AND C.NUM_ENDERECO IN (0, 1)
    WHERE S.NU_NUMERO IS NULL OR S.NU_NUMERO IN ('S/N', 'NA', '')
    ORDER BY S.NU_NOTIFIC
  ", col_mun_ra, tabela_cnefe))
        
        total_B33 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB33")$n
        dbExecute(con, "DELETE FROM tempB3_AUX2 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempB33)")
        
        # --- Consolida B.3 ---
        dbExecute(con, sprintf("
    CREATE TEMP TABLE tempB3 AS
    SELECT
      NU_NOTIFIC,
      NM_LOGRADO AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempB31
    UNION ALL
    SELECT
      NU_NOTIFIC,
      NM_LOGRADO AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempB32
    UNION ALL
    SELECT
      NU_NOTIFIC,
      NM_LOGRADO AS ADDRESS,
      NU_NUMERO,
      ENDERECO_CNEFE,
      SIMILARITY,
      DISTANCIA_NUM,
      FASE,
      LATITUDE,
      LONGITUDE,
      DSC_LOCALIDADE,
      COD_SETOR,
      %s
    FROM tempB33
  ", col_mun_ra2, col_mun_ra2, col_mun_ra2))
        
        total_B3 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempB3")$n
        dbExecute(con, "DELETE FROM tempB WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempB3)")
        
        # --- Consolida Fase 3 ---
        dbExecute(con, "
    CREATE TEMP TABLE temp3 AS
    SELECT *
    FROM tempA3
    UNION ALL
    SELECT *
    FROM tempB3
  ")
        
        total_fase3  <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp3")$n
        dbExecute(con, "DELETE FROM temp0 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM temp3)")
        pendentes_f3 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp0")$n
        
        setProgress(
          value = 0.65,
          message = 'FASE 3 concluída',
          detail = paste(total_fase3, 'georreferenciados |', pendentes_f3, 'pendentes')
        )
        
        cat(paste0("FASE 3 concluída: ", total_fase3,
                   " georreferenciados | ", pendentes_f3, " pendentes",
                   " (A.3.1 = ", total_A31, " | A.3.2 = ", total_A32, " | A.3.3 = ", total_A33,
                   " | B.3.1 = ", total_B31, " | B.3.2 = ", total_B32, " | B.3.3 = ", total_B33, ")\n"))
        
        dbGetQuery(con, "SELECT COUNT (*) FROM TEMP0")
        
        # ==============================================================
        # FASE 4 – Probabilístico Jaro-Winkler em lote: address3 ~ ENDERECO_CNEFE3
        #
        # Pendentes de tempA (com município) → filtro por CD_MUN
        # Pendentes de tempB (sem município) → sem filtro de município
        #
        # Todos os pendentes de uma vez (fase4_lote, abaixo) → melhor ENDERECO_CNEFE3 por
        # NU_NOTIFIC, depois ancoragem numérica (4.1 exato, 4.2 aproximado, 4.3 âncora 0/1).
        # ==============================================================
        
        
        setProgress(
          value = 0.70,
          message = 'Executando FASE 4 - Match probabilístico (Jaro-Winkler)...',
          detail = 'Processando similaridade de texto'
        )

        # Processa uma fase 4 inteira (A ou B) em LOTE: bloqueio por
        # 5-gramas (candidatos = ruas que compartilham pelo menos 2
        # substrings de 5 caracteres com o address3 do pendente -- robusto
        # a erro de digitação, testado com até 2 erros seguidos logo após
        # o tipo de logradouro, sem nenhuma divergência de resultado) ->
        # match de rua (jaro_winkler só nos candidatos pós-bloqueio, melhor
        # por registro via MAX(SIMILARITY)+JOIN -- mais rápido que
        # ROW_NUMBER()/PARTITION BY quando o cruzamento é grande) ->
        # ancoragem numérica (4.1 exato -> 4.2 aproximado -> 4.3 âncora
        # 0/1). Tudo processado de uma vez pro lote inteiro de pendentes,
        # substituindo o loop por registro (uma consulta ao banco por
        # registro, refeita do zero a cada iteração) que existia antes.
        # Técnica validada numa sessão separada, no script standalone
        # georreferenciamento.R (ver CONTEXTO_FIXES_PERFORMANCE.md) e
        # revalidada aqui contra a implementação anterior, com erros de
        # digitação injetados de propósito -- zero divergência.
        #
        # com_municipio: TRUE pra FASE A (filtra por município/RA), FALSE
        # pra FASE B (ignora município, varre a UF/RA inteira).
        fase4_lote <- function(con, pendentes_df, mun_col, tabela_ruas_cnefe, tabela_cnefe,
                                meio_filtro_mun_ra, meio_filtro_mun_ra2, col_mun_ra2,
                                limiar_similaridade,
                                com_municipio, sufixo_fase, rotulo_fase) {

          names(pendentes_df)[names(pendentes_df) == mun_col] <- "MUN_FILTRO"
          tabela_pend <- paste0("pend_", sufixo_fase)
          dbWriteTable(con, tabela_pend, pendentes_df, temporary = TRUE, overwrite = TRUE)

          filtro_ngram_mun <- if (com_municipio) "AND E.MUN_FILTRO = P.MUN_FILTRO" else ""

          # Na fase A, os n-gramas das ruas só precisam cobrir os municípios que
          # aparecem no lote -- o cruzamento logo abaixo é feito município a
          # município de qualquer forma. Sem este filtro, processar um arquivo
          # de Recife gerava os n-gramas de todas as ruas de Pernambuco, e um
          # de São Paulo, do estado inteiro: foi o que estourou o limite de
          # memória ("failed to pin block ... 3.7 GiB/3.7 GiB used") com SP.
          # Na fase B não há município informado, então a varredura da UF
          # inteira é intencional.
          #
          # EXISTS com "=", e não "IN (subconsulta)": a coluna de município do
          # CNEFE pode ser numérica (é o caso de SP) enquanto a do arquivo
          # chega como texto, e o DuckDB recusa comparar DOUBLE com VARCHAR
          # dentro de IN ("Binder Error: Cannot compare values of type DOUBLE
          # and VARCHAR in IN/ANY/ALL clause"). Na igualdade ele converte
          # sozinho -- é a mesma comparação que o join logo abaixo já fazia.
          filtro_ruas_mun <- if (com_municipio) {
            sprintf("AND EXISTS (SELECT 1 FROM %s P WHERE P.MUN_FILTRO = R.MUN_FILTRO)", tabela_pend)
          } else {
            ""
          }
          filtro_num_mun   <- if (com_municipio) sprintf("AND C.%s = P.MUN_FILTRO", meio_filtro_mun_ra) else ""

          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE ngramas_ruas_%s AS
      SELECT R.MUN_FILTRO, R.ENDERECO_CNEFE3, substr(R.ENDERECO_CNEFE3, i, 5) AS ngrama
      FROM %s AS R, range(1, length(R.ENDERECO_CNEFE3) - 3) AS t(i)
      WHERE length(R.ENDERECO_CNEFE3) > 4 %s
    ", sufixo_fase, tabela_ruas_cnefe, filtro_ruas_mun))

          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE ngramas_pend_%s AS
      SELECT NU_NOTIFIC, MUN_FILTRO, substr(address3, i, 5) AS ngrama
      FROM %s, range(1, length(address3) - 3) AS t(i)
      WHERE length(address3) > 4
    ", sufixo_fase, tabela_pend))

          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE candidatos_rua_%s AS
      SELECT P.NU_NOTIFIC, E.ENDERECO_CNEFE3
      FROM ngramas_pend_%s P
      INNER JOIN ngramas_ruas_%s E ON E.ngrama = P.ngrama %s
      GROUP BY P.NU_NOTIFIC, E.ENDERECO_CNEFE3
      HAVING COUNT(*) >= 2
    ", sufixo_fase, sufixo_fase, sufixo_fase, filtro_ngram_mun))

          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE sim_rua_%s AS
      SELECT C.NU_NOTIFIC, C.ENDERECO_CNEFE3,
        jaro_winkler_similarity(C.ENDERECO_CNEFE3, P.address3) AS SIMILARITY
      FROM candidatos_rua_%s C
      INNER JOIN %s P ON P.NU_NOTIFIC = C.NU_NOTIFIC
    ", sufixo_fase, sufixo_fase, tabela_pend))

          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE melhor_rua_%s AS
      SELECT NU_NOTIFIC, MAX(SIMILARITY) AS MAX_SIM FROM sim_rua_%s GROUP BY NU_NOTIFIC
    ", sufixo_fase, sufixo_fase))

          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE vencedores_rua_%s AS
      SELECT DISTINCT ON (s.NU_NOTIFIC) s.NU_NOTIFIC, s.ENDERECO_CNEFE3, s.SIMILARITY
      FROM sim_rua_%s s
      INNER JOIN melhor_rua_%s m ON m.NU_NOTIFIC = s.NU_NOTIFIC AND s.SIMILARITY = m.MAX_SIM
      WHERE m.MAX_SIM >= %s
      ORDER BY s.NU_NOTIFIC, s.ENDERECO_CNEFE3
    ", sufixo_fase, sufixo_fase, sufixo_fase, limiar_similaridade))

          # Até 14 candidatas por registro (mesmo corte de antes, "rank <
          # 15"), pra manter a tela de revisão manual (triagem) funcionando
          # igual -- calculado em lote, uma vez só.
          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE triagem_rank_%s AS
      SELECT NU_NOTIFIC, ENDERECO_CNEFE3, SIMILARITY,
        ROW_NUMBER() OVER (PARTITION BY NU_NOTIFIC ORDER BY SIMILARITY DESC, ENDERECO_CNEFE3) AS rank
      FROM sim_rua_%s
      WHERE SIMILARITY >= %s
    ", sufixo_fase, sufixo_fase, limiar_similaridade))

          # A triagem precisa das MESMAS colunas que a versão em loop
          # entregava (a consolidação mais abaixo faz mutate(address =
          # address3) e select(-c(NU_NUMERO, ...))): além do ranking de ruas
          # acima, o address3/NU_NUMERO/município do próprio registro e as
          # coordenadas do candidato. Como a similaridade é por rua e não
          # discrimina entre casas da mesma rua, cada rua candidata entra com
          # uma casa "representante" (a de menor NUM_ENDERECO) -- mesmo
          # critério da versão anterior. O join com a CNEFE é feito só depois
          # do corte em rank < 15, para resolver coordenada apenas das até 14
          # candidatas de cada registro.
          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE triagem_rep_%s AS
      SELECT
        T.NU_NOTIFIC,
        P.MUN_FILTRO AS %s,
        P.NU_NUMERO,
        P.address3,
        T.ENDERECO_CNEFE3,
        T.SIMILARITY,
        C.LATITUDE,
        C.LONGITUDE,
        C.%s,
        T.rank,
        ROW_NUMBER() OVER (
          PARTITION BY T.NU_NOTIFIC, T.ENDERECO_CNEFE3 ORDER BY C.NUM_ENDERECO
        ) AS rn_rep
      FROM triagem_rank_%s T
      INNER JOIN %s P ON P.NU_NOTIFIC = T.NU_NOTIFIC
      INNER JOIN %s C ON C.ENDERECO_CNEFE3 = T.ENDERECO_CNEFE3 %s
      WHERE T.rank < 15
    ", sufixo_fase, mun_col, meio_filtro_mun_ra2, sufixo_fase, tabela_pend,
       tabela_cnefe, filtro_num_mun))

          # ATENÇÃO À ORDEM DAS COLUNAS nas três tabelas abaixo (4.1, 4.2, 4.3):
          # tem que ser a mesma das demais fases (... ENDERECO_CNEFE,
          # SIMILARITY, DISTANCIA_NUM, FASE ...). O TEMPGEO junta as fases com
          # UNION ALL de SELECT *, que casa as colunas pela POSIÇÃO, não pelo
          # nome. Com DISTANCIA_NUM antes de SIMILARITY -- como ficou na
          # reescrita em lote --, os casos da fase 4 saíam com a distância do
          # número na coluna similarity e a similaridade em DISTANCIA_NUM.

          # --- 4.1: número exato após match de logradouro ---
          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE temp%s41 AS
      SELECT DISTINCT ON (V.NU_NOTIFIC)
        V.NU_NOTIFIC, P.address3 AS ADDRESS, P.NU_NUMERO,
        C.ENDERECO_CNEFE, V.SIMILARITY, 0 AS DISTANCIA_NUM, '%s' AS FASE,
        C.LATITUDE, C.LONGITUDE, C.DSC_LOCALIDADE, C.COD_SETOR, %s
      FROM vencedores_rua_%s V
      INNER JOIN %s P ON P.NU_NOTIFIC = V.NU_NOTIFIC
      INNER JOIN %s C ON C.ENDERECO_CNEFE3 = V.ENDERECO_CNEFE3 %s
      WHERE P.NU_NUMERO = CAST(C.NUM_ENDERECO AS VARCHAR)
      ORDER BY V.NU_NOTIFIC, V.SIMILARITY DESC, C.NUM_ENDERECO
    ", sufixo_fase, paste0("FASE ", rotulo_fase, ".4.1"), col_mun_ra2, sufixo_fase, tabela_pend, tabela_cnefe, filtro_num_mun))

          dbExecute(con, sprintf("DELETE FROM vencedores_rua_%s WHERE NU_NOTIFIC IN (SELECT NU_NOTIFIC FROM temp%s41)", sufixo_fase, sufixo_fase))

          # --- 4.2: aproximação numérica (só quem não bateu em 4.1) ---
          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE temp%s42 AS
      SELECT DISTINCT ON (V.NU_NOTIFIC)
        V.NU_NOTIFIC, P.address3 AS ADDRESS, P.NU_NUMERO,
        C.ENDERECO_CNEFE,
        GREATEST(
          V.SIMILARITY - (FLOOR(ABS(C.NUM_ENDERECO - TRY_CAST(REGEXP_EXTRACT(P.NU_NUMERO, '^\\d+') AS INT32)) / 5.0) * 0.0003),
          0.0
        ) AS SIMILARITY,
        ABS(C.NUM_ENDERECO - TRY_CAST(REGEXP_EXTRACT(P.NU_NUMERO, '^\\d+') AS INT32)) AS DISTANCIA_NUM,
        '%s' AS FASE,
        C.LATITUDE, C.LONGITUDE, C.DSC_LOCALIDADE, C.COD_SETOR, %s
      FROM vencedores_rua_%s V
      INNER JOIN %s P ON P.NU_NOTIFIC = V.NU_NOTIFIC
      INNER JOIN %s C ON C.ENDERECO_CNEFE3 = V.ENDERECO_CNEFE3 %s
      WHERE P.NU_NUMERO IS NOT NULL AND P.NU_NUMERO NOT IN ('S/N', 'NA', '')
      ORDER BY V.NU_NOTIFIC,
        ABS(C.NUM_ENDERECO - TRY_CAST(REGEXP_EXTRACT(P.NU_NUMERO, '^\\d+') AS INT32)) ASC,
        C.NUM_ENDERECO
    ", sufixo_fase, paste0("FASE ", rotulo_fase, ".4.2"), col_mun_ra2, sufixo_fase, tabela_pend, tabela_cnefe, filtro_num_mun))

          dbExecute(con, sprintf("DELETE FROM vencedores_rua_%s WHERE NU_NOTIFIC IN (SELECT NU_NOTIFIC FROM temp%s42)", sufixo_fase, sufixo_fase))

          # --- 4.3: sem número válido -> ancora em 0/1 (só quem sobrou) ---
          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE temp%s43 AS
      SELECT DISTINCT ON (V.NU_NOTIFIC)
        V.NU_NOTIFIC, P.address3 AS ADDRESS, P.NU_NUMERO,
        C.ENDERECO_CNEFE, V.SIMILARITY, 0 AS DISTANCIA_NUM, '%s' AS FASE,
        C.LATITUDE, C.LONGITUDE, C.DSC_LOCALIDADE, C.COD_SETOR, %s
      FROM vencedores_rua_%s V
      INNER JOIN %s P ON P.NU_NOTIFIC = V.NU_NOTIFIC
      INNER JOIN %s C ON C.ENDERECO_CNEFE3 = V.ENDERECO_CNEFE3 %s
        AND C.NUM_ENDERECO IN (0, 1)
      WHERE P.NU_NUMERO IS NULL OR P.NU_NUMERO IN ('S/N', 'NA', '')
      ORDER BY V.NU_NOTIFIC, V.SIMILARITY DESC, C.NUM_ENDERECO
    ", sufixo_fase, paste0("FASE ", rotulo_fase, ".4.3"), col_mun_ra2, sufixo_fase, tabela_pend, tabela_cnefe, filtro_num_mun))

          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE temp%s4 AS
      SELECT * FROM temp%s41
      UNION ALL SELECT * FROM temp%s42
      UNION ALL SELECT * FROM temp%s43
    ", sufixo_fase, sufixo_fase, sufixo_fase, sufixo_fase))

          triagem_rank <- dbGetQuery(con, sprintf("
      SELECT NU_NOTIFIC, %s, NU_NUMERO, address3,
             ENDERECO_CNEFE3, SIMILARITY, LATITUDE, LONGITUDE, %s, rank
      FROM triagem_rep_%s
      WHERE rn_rep = 1
      ORDER BY NU_NOTIFIC, rank
    ", mun_col, meio_filtro_mun_ra2, sufixo_fase))
          por_registro <- split(triagem_rank, triagem_rank$NU_NOTIFIC)
          triagem <- lapply(pendentes_df$NU_NOTIFIC, function(nu) {
            if (nu %in% names(por_registro)) por_registro[[nu]] else NULL
          })

          list(
            resultado = dbGetQuery(con, sprintf("SELECT * FROM temp%s4", sufixo_fase)),
            triagem   = triagem
          )
        }

        notificacoes_fA4 <- dbGetQuery(con, sprintf("
    SELECT NU_NOTIFIC, %s, NU_NUMERO, address3 FROM tempA
  ", mun_ra))

        total_notif_fA4    <- nrow(notificacoes_fA4)

        print(paste0("FASE 4 iniciada: ", total_notif_fA4, " registros pendentes"))

        # Diferente de B4/A5/G5, esse loop não tinha proteção para
        # total_notif_fA4 == 0 -- quando tempA já estava totalmente resolvido
        # pelas fases 1-3 (comum em bases pequenas, ex: onda4CE com 4 linhas),
        # o loop nunca rodava, e tempA41/tempA42/tempA43/tempA4 nunca eram
        # criadas. A consolidação da FASE 4 (linha ~3460) faz "SELECT * FROM
        # tempA4" incondicionalmente, quebrando com "Catalog Error: Table
        # with name tempA4 does not exist!". Cria o mesmo schema vazio que já
        # era usado internamente quando nenhuma sub-fase (4.1/4.2/4.3) tinha
        # match, dessa vez cobrindo o caso "nem chegou a tentar".
        if (total_notif_fA4 == 0) {
          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempA4 AS
      SELECT
        CAST(NULL AS VARCHAR) AS NU_NOTIFIC,
        CAST(NULL AS VARCHAR) AS ADDRESS,
        CAST(NULL AS VARCHAR) AS NU_NUMERO,
        CAST(NULL AS VARCHAR) AS ENDERECO_CNEFE,
        CAST(NULL AS DOUBLE)  AS SIMILARITY,
        CAST(NULL AS INT)     AS DISTANCIA_NUM,
        CAST(NULL AS VARCHAR) AS FASE,
        CAST(NULL AS DOUBLE)  AS LATITUDE,
        CAST(NULL AS DOUBLE)  AS LONGITUDE,
        CAST(NULL AS VARCHAR) AS DSC_LOCALIDADE,
        CAST(NULL AS VARCHAR) AS COD_SETOR,
        CAST(NULL AS VARCHAR) AS %s,
        CAST(NULL AS VARCHAR) AS %s
      WHERE 1=0
    ", meio_filtro_mun_ra, meio_filtro_mun_ra2))
          triagem_A4 <- list()
          total_A41 <- total_A42 <- total_A43 <- 0
          print("FASE 4A: nenhum registro pendente.")
        } else {

          resultado_4a <- fase4_lote(con, notificacoes_fA4, mun_ra, tabela_ruas_cnefe, tabela_cnefe,
                                      meio_filtro_mun_ra, meio_filtro_mun_ra2, col_mun_ra2,
                                      limiar_similaridade,
                                      com_municipio = TRUE, sufixo_fase = "A4", rotulo_fase = "A")
          triagem_A4 <- resultado_4a$triagem

          if (nrow(resultado_4a$resultado) > 0) {

            dbWriteTable(con, "tempA4", resultado_4a$resultado, temporary = TRUE, overwrite = TRUE)

            total_A41 <- sum(resultado_4a$resultado$FASE == "FASE A.4.1")
            total_A42 <- sum(resultado_4a$resultado$FASE == "FASE A.4.2")
            total_A43 <- sum(resultado_4a$resultado$FASE == "FASE A.4.3")
            print(paste0("Total fase 4 - número exato: ", total_A41))
            print(paste0("Total fase 4 - aproximação numérica: ", total_A42))
            print(paste0("Total fase 4 - sem número (âncora 0/1): ", total_A43))

            dbExecute(con, "DELETE FROM tempA WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA4)")

          } else {

            dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempA4 AS
      SELECT
        CAST(NULL AS VARCHAR) AS NU_NOTIFIC,
        CAST(NULL AS VARCHAR) AS ADDRESS,
        CAST(NULL AS VARCHAR) AS NU_NUMERO,
        CAST(NULL AS VARCHAR) AS ENDERECO_CNEFE,
        CAST(NULL AS DOUBLE)  AS SIMILARITY,
        CAST(NULL AS INT)     AS DISTANCIA_NUM,
        CAST(NULL AS VARCHAR) AS FASE,
        CAST(NULL AS DOUBLE)  AS LATITUDE,
        CAST(NULL AS DOUBLE)  AS LONGITUDE,
        CAST(NULL AS VARCHAR) AS DSC_LOCALIDADE,
        CAST(NULL AS VARCHAR) AS COD_SETOR,
        CAST(NULL AS VARCHAR) AS %s,
        CAST(NULL AS VARCHAR) AS %s
      WHERE 1=0
    ", meio_filtro_mun_ra, meio_filtro_mun_ra2))

            total_A41 <- total_A42 <- total_A43 <- 0
            print("FASE 4A: nenhum registro atingiu o limiar de similaridade.")
          }
        }

        # ==============================================================================
        # FASE 4 – Subfase B: lote Jaro-Winkler (address3 ~ ENDERECO_CNEFE3)
        #           SEM filtro de município (tempB / casos com município ignorado)
        # ==============================================================================
        
        notificacoes_fB4 <- dbGetQuery(con, sprintf("
    SELECT NU_NOTIFIC, %s, NU_NUMERO, address3 FROM tempB
  ", mun_ra))

        total_notif_fB4    <- nrow(notificacoes_fB4)

        print(paste0("FASE 4B iniciada: ", total_notif_fB4, " registros pendentes (tempB sem CD_MUN)"))

        if (total_notif_fB4 == 0) {

          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempB4 AS
      SELECT
        CAST(NULL AS VARCHAR) AS NU_NOTIFIC,
        CAST(NULL AS VARCHAR) AS ADDRESS,
        CAST(NULL AS VARCHAR) AS NU_NUMERO,
        CAST(NULL AS VARCHAR) AS ENDERECO_CNEFE,
        CAST(NULL AS DOUBLE)  AS SIMILARITY,
        CAST(NULL AS INT)     AS DISTANCIA_NUM,
        CAST(NULL AS VARCHAR) AS FASE,
        CAST(NULL AS DOUBLE)  AS LATITUDE,
        CAST(NULL AS DOUBLE)  AS LONGITUDE,
        CAST(NULL AS VARCHAR) AS DSC_LOCALIDADE,
        CAST(NULL AS VARCHAR) AS COD_SETOR,
        CAST(NULL AS VARCHAR) AS %s,
        CAST(NULL AS VARCHAR) AS %s
      WHERE 1=0
    ", meio_filtro_mun_ra, meio_filtro_mun_ra2))

          triagem_B4 <- list()
          total_B41 <- total_B42 <- total_B43 <- 0
        } else {

          resultado_4b <- fase4_lote(con, notificacoes_fB4, mun_ra, tabela_ruas_cnefe, tabela_cnefe,
                                      meio_filtro_mun_ra, meio_filtro_mun_ra2, col_mun_ra2,
                                      limiar_similaridade,
                                      com_municipio = FALSE, sufixo_fase = "B4", rotulo_fase = "B")
          triagem_B4 <- resultado_4b$triagem

          if (nrow(resultado_4b$resultado) > 0) {

            dbWriteTable(con, "tempB4", resultado_4b$resultado, temporary = TRUE, overwrite = TRUE)

            total_B41 <- sum(resultado_4b$resultado$FASE == "FASE B.4.1")
            total_B42 <- sum(resultado_4b$resultado$FASE == "FASE B.4.2")
            total_B43 <- sum(resultado_4b$resultado$FASE == "FASE B.4.3")
            print(paste0("Total fase 4B - número exato: ", total_B41))
            print(paste0("Total fase 4B - aproximação numérica: ", total_B42))
            print(paste0("Total fase 4B - sem número (âncora 0/1): ", total_B43))

          } else {
            dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempB4 AS
      SELECT
        CAST(NULL AS VARCHAR) AS NU_NOTIFIC,
        CAST(NULL AS VARCHAR) AS ADDRESS,
        CAST(NULL AS VARCHAR) AS NU_NUMERO,
        CAST(NULL AS VARCHAR) AS ENDERECO_CNEFE,
        CAST(NULL AS DOUBLE)  AS SIMILARITY,
        CAST(NULL AS INT)     AS DISTANCIA_NUM,
        CAST(NULL AS VARCHAR) AS FASE,
        CAST(NULL AS DOUBLE)  AS LATITUDE,
        CAST(NULL AS DOUBLE)  AS LONGITUDE,
        CAST(NULL AS VARCHAR) AS DSC_LOCALIDADE,
        CAST(NULL AS VARCHAR) AS COD_SETOR,
        CAST(NULL AS VARCHAR) AS %s,
        CAST(NULL AS VARCHAR) AS %s
      WHERE 1=0
    ", meio_filtro_mun_ra, meio_filtro_mun_ra2))

            total_B41 <- total_B42 <- total_B43 <- 0
            print("FASE 4B: nenhum registro atingiu o limiar de similaridade.")
          }
        }

        
        # --- Consolida Fase 4 (A + B) ---
        dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE temp4 AS
    SELECT * FROM tempA4
    UNION ALL
    SELECT * FROM tempB4
  ")
        
        total_fase4  <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp4")$n
        dbExecute(con, "DELETE FROM tempA WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM temp4)")
        dbExecute(con, "DELETE FROM tempB WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM temp4)")
        dbExecute(con, "DELETE FROM temp0 WHERE CAST(NU_NOTIFIC AS VARCHAR) IN (SELECT DISTINCT NU_NOTIFIC FROM temp4)")
        pendentes_f4 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp0")$n
        
        setProgress(
          value = 0.78,
          message = 'FASE 4 concluída',
          detail = paste(total_fase4, 'georreferenciados |', pendentes_f4, 'pendentes')
        )
        
        cat(paste0("FASE 4 concluída: ", total_fase4,
                   " georreferenciados | ", pendentes_f4, " pendentes",
                   " (A.4.1 = ", total_A41,
                   " | A.4.2 = ", total_A42,
                   " | A.4.3 = ", total_A43,
                   " | B.4.1 = ", total_B41,
                   " | B.4.2 = ", total_B42,
                   " | B.4.3 = ", total_B43, ")\n"))
        
        # ==============================================================================
        # FASE 5 – Subfase A: loop Jaro-Winkler (address ~ ENDERECO_CNEFE)
        #           COM filtro de município (tempA pendentes)
        # ==============================================================================
        
        setProgress(
          value = 0.82,
          message = 'Executando FASE 5 - Match por endereço completo...',
          detail = 'Etapa final de processamento'
        )
        
        notificacoes_fA5 <- dbGetQuery(con, sprintf("
    SELECT NU_NOTIFIC, %s, NU_NUMERO, address, NM_LOGRADO FROM tempA
  ", mun_ra))
        
        total_notif_fA5    <- nrow(notificacoes_fA5)
        resultados_faseA51 <- vector("list", total_notif_fA5)
        
        triagem_A5 <- list()
        length(triagem_A5) <- total_notif_fA5
        
        print(paste0("FASE 5A iniciada: ", total_notif_fA5, " registros pendentes"))
        
        if(length(resultados_faseA51) == 0){
          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempA5 AS
      SELECT
        CAST(NULL AS VARCHAR) AS NU_NOTIFIC,
        CAST(NULL AS VARCHAR) AS ADDRESS,
        CAST(NULL AS VARCHAR) AS NU_NUMERO,
        CAST(NULL AS VARCHAR) AS ENDERECO_CNEFE,
        CAST(NULL AS DOUBLE)  AS SIMILARITY,
        CAST(NULL AS INT)     AS DISTANCIA_NUM,
        CAST(NULL AS VARCHAR) AS FASE,
        CAST(NULL AS DOUBLE)  AS LATITUDE,
        CAST(NULL AS DOUBLE)  AS LONGITUDE,
        CAST(NULL AS VARCHAR) AS DSC_LOCALIDADE,
        CAST(NULL AS VARCHAR) AS COD_SETOR,
        CAST(NULL AS VARCHAR) AS %s,
        CAST(NULL AS VARCHAR) AS %s
      WHERE 1=0
    ", meio_filtro_mun_ra, meio_filtro_mun_ra2))
          
          # Mesmo esquema de colunas do resultado real da FASE A.5 (ver bloco
          # de consolidação mais abaixo): "address" minúsculo, sem DISTANCIA_NUM.
          triagem_A5 <- list(data.frame(
            NU_NOTIFIC      = character(),
            address         = character(),
            NU_NUMERO       = character(),
            ENDERECO_CNEFE  = character(),
            SIMILARITY      = numeric(),
            FASE            = character(),
            LATITUDE        = numeric(),
            LONGITUDE       = numeric(),
            DSC_LOCALIDADE  = character(),
            COD_SETOR       = character(),
            stringsAsFactors = FALSE
          ))

          triagem_A5[[1]][[meio_filtro_mun_ra]]  <- character()
          triagem_A5[[1]][[meio_filtro_mun_ra2]] <- character()
        } else {
          for (i in seq_len(total_notif_fA5)) {
            
            nu        <- notificacoes_fA5$NU_NOTIFIC[i]
            
            if (uf_selecionada == "DF") {
              muniresat <- notificacoes_fA5$Regiao_adm[i]
            } else {
              muniresat <- notificacoes_fA5$MUNIRESAT[i]
            }
            
            nu_numero <- notificacoes_fA5$NU_NUMERO[i]
            address   <- notificacoes_fA5$address[i]
            nm_logrado <- notificacoes_fA5$NM_LOGRADO[i]

            print(paste0(
              "Fase 5A - Processando registro ", i, " de ", total_notif_fA5,
              " | NU_NOTIFIC: ", nu,
              " | address: ", address
            ))
            flush.console()

            # Sem logradouro (nem o complemento usado como substituto -- ver
            # juntar_endereco()/etl_sinan()), `address` carrega só número e
            # bairro, texto pouco informativo pra comparar por similaridade.
            # jaro_winkler_similarity(rua_real, "S/N, BAIRRO") frequentemente
            # bate acima do limiar padrão de 0,5 contra ruas reais do mesmo
            # bairro, por coincidência de caracteres -- SEM o filtro por
            # 5-gramas que a FASE 4 tem, que naturalmente descarta buscas tão
            # curtas. Pular aqui evita um match espúrio e deixa o caso seguir
            # pro fallback por centroide. Reportado processando Recife/TB
            # (~29 mil linhas): 0 casos foram para o fallback.
            if (is.na(nm_logrado) || trimws(nm_logrado) == "") {
              res <- data.frame()
            } else {
              res <- dbGetQuery(con, sprintf("
    -- LIMIT em vez de janela sobre a tabela inteira: as duas ROW_NUMBER() de
    -- antes obrigavam o DuckDB a materializar e ordenar TODO o CNEFE da UF, uma
    -- vez por registro pendente, para no fim ficar com 15 linhas. Foi o que
    -- estourou a memória processando SP (failed to pin block, 3.7 GiB de 3.7
    -- GiB usados), onde a subfase G.5 nem tem filtro de município. Com
    -- ORDER BY ... LIMIT o DuckDB usa Top-N e guarda só as melhores candidatas.
    -- O limite de 2000 é folgado de propósito: serve para ainda sobrar material
    -- depois de descartar os endereços repetidos (prédios com muitas unidades),
    -- e o vencedor continua sendo o de maior similaridade.
    WITH candidatos AS (
      SELECT
        c.ENDERECO_CNEFE,
        c.LATITUDE,
        c.LONGITUDE,
        c.DSC_LOCALIDADE,
        c.COD_SETOR,
        %s,
        jaro_winkler_similarity(c.ENDERECO_CNEFE, '%s') AS SIMILARITY
      FROM %s AS c
      WHERE %s = '%s'
      ORDER BY SIMILARITY DESC, c.ENDERECO_CNEFE
      LIMIT 2000
    ),
    enderecos_unicos AS (
      SELECT DISTINCT ON (ENDERECO_CNEFE) *
      FROM candidatos
      ORDER BY ENDERECO_CNEFE, SIMILARITY DESC
    ),
    rank_final AS (
      SELECT
        '%s'  AS NU_NOTIFIC,
        '%s'  AS address,
        '%s'  AS NU_NUMERO,
        ENDERECO_CNEFE,
        SIMILARITY,
        'FASE A.5' AS FASE,
        LATITUDE, LONGITUDE, DSC_LOCALIDADE, COD_SETOR,
        %s,
        ROW_NUMBER() OVER (
          ORDER BY SIMILARITY DESC, ENDERECO_CNEFE
        ) AS rank
      FROM enderecos_unicos
    )
    SELECT *
    FROM rank_final
    WHERE rank <= 15
    ORDER BY rank
  ",
                                           col_mun_ra,
                                           gsub("'", "''", as.character(address)),
                                           tabela_cnefe, meio_filtro_mun_ra, muniresat,
                                           nu,
                                           gsub("'", "''", as.character(address)),
                                           nu_numero,
                                           col_mun_ra2
              ))
            }

            if (nrow(res) > 0 &&
                !is.na(res$SIMILARITY[1]) &&
                res$SIMILARITY[1] >= limiar_similaridade) {
              resultados_faseA51[[i]] <- res
            }

            tempA51_df <- do.call(rbind, Filter(Negate(is.null), resultados_faseA51))

            # Triagem guarda só o resultado do PRÓPRIO registro (mesmo
            # critério de resultados_faseA51[[i]] acima), não mais o
            # acumulado de 1..i -- ver nota de performance na FASE 4A/4B.
            # `[i] <-` (colchete simples) em vez de `[[i]] <-` para não
            # desalinhar índices quando o valor é NULL.
            triagem_A5[i] <- list(
              if (nrow(res) > 0 && !is.na(res$SIMILARITY[1]) && res$SIMILARITY[1] >= limiar_similaridade) res else NULL
            )

            # NULL quando nenhum registro (de 1 até i) teve match ainda -- ver
            # comentário equivalente na FASE A4.
            if (!is.null(tempA51_df)) {
              tempA51_df <- tempA51_df %>% filter(rank == 1)
            }
            
            if (!is.null(tempA51_df) && nrow(tempA51_df) > 0) {
              
              dbWriteTable(con, "tempA51_AUX", tempA51_df, temporary = TRUE, overwrite = TRUE)
              
              # INSERT INTO acumula resultados a cada iteração
              dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempA5 AS
      SELECT
        NU_NOTIFIC,
        ADDRESS,
        NU_NUMERO,
        ENDERECO_CNEFE,
        SIMILARITY,
        0 AS DISTANCIA_NUM,
        FASE,
        LATITUDE,
        LONGITUDE,
        DSC_LOCALIDADE,
        COD_SETOR,
        %s
      FROM tempA51_AUX
    ", col_mun_ra2))
              
              dbExecute(con, "DELETE FROM tempA WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA5)")
            } else {
              dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempA5 AS
      SELECT
        CAST(NULL AS VARCHAR) AS NU_NOTIFIC,
        CAST(NULL AS VARCHAR) AS ADDRESS,
        CAST(NULL AS VARCHAR) AS NU_NUMERO,
        CAST(NULL AS VARCHAR) AS ENDERECO_CNEFE,
        CAST(NULL AS DOUBLE)  AS SIMILARITY,
        CAST(NULL AS INT)     AS DISTANCIA_NUM,
        CAST(NULL AS VARCHAR) AS FASE,
        CAST(NULL AS DOUBLE)  AS LATITUDE,
        CAST(NULL AS DOUBLE)  AS LONGITUDE,
        CAST(NULL AS VARCHAR) AS DSC_LOCALIDADE,
        CAST(NULL AS VARCHAR) AS COD_SETOR,
        CAST(NULL AS VARCHAR) AS %s,
        CAST(NULL AS VARCHAR) AS %s
      WHERE 1=0
    ", meio_filtro_mun_ra, meio_filtro_mun_ra2))
              
              total_A51 <- 0
              print("FASE 5A: nenhum registro atingiu o limiar de similaridade.")
            }
            
            dbExecute(con, "DELETE FROM temp0 WHERE NU_NOTIFIC IN (SELECT DISTINCT NU_NOTIFIC FROM tempA5)")
            
          } 
        }
        
        # ==============================================================================
        # FASE 5 – Subfase B (geral): loop Jaro-Winkler (address ~ ENDERECO_CNEFE)
        #           SEM filtro de município (temp0 pendentes)
        # ==============================================================================
        
        notificacoes_f5G <- dbGetQuery(con, sprintf("
    SELECT NU_NOTIFIC, %s, NU_NUMERO, address, NM_LOGRADO FROM temp0
  ", mun_ra))
        
        total_notif_f5G   <- nrow(notificacoes_f5G)
        resultados_fase5G <- vector("list", total_notif_f5G)
        
        triagem_G5 <- list()
        length(triagem_G5) <- total_notif_f5G
        
        print(paste0("FASE 5G geral iniciada: ", total_notif_f5G, " registros pendentes."))
        
        sql_escape <- function(x) {
          if (is.na(x) || is.null(x)) return("NULL")
          paste0("'", gsub("'", "''", as.character(x)), "'")
        }
        
        sql_escape_raw <- function(x) {
          if (is.na(x) || is.null(x)) return("NULL")
          gsub("'", "''", as.character(x))
        }
        
        if(length(resultados_fase5G) == 0){
          dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempG5 AS
      SELECT
        CAST(NULL AS VARCHAR) AS NU_NOTIFIC,
        CAST(NULL AS VARCHAR) AS ADDRESS,
        CAST(NULL AS VARCHAR) AS NU_NUMERO,
        CAST(NULL AS VARCHAR) AS ENDERECO_CNEFE,
        CAST(NULL AS DOUBLE)  AS SIMILARITY,
        CAST(NULL AS INT)     AS DISTANCIA_NUM,
        CAST(NULL AS VARCHAR) AS FASE,
        CAST(NULL AS DOUBLE)  AS LATITUDE,
        CAST(NULL AS DOUBLE)  AS LONGITUDE,
        CAST(NULL AS VARCHAR) AS DSC_LOCALIDADE,
        CAST(NULL AS VARCHAR) AS COD_SETOR,
        CAST(NULL AS VARCHAR) AS %s,
        CAST(NULL AS VARCHAR) AS %s
      WHERE 1=0
    ", meio_filtro_mun_ra, meio_filtro_mun_ra2))
          
          # Mesmo esquema de colunas do resultado real da FASE G.5 (ver bloco
          # de consolidação mais abaixo): "address" minúsculo, sem DISTANCIA_NUM.
          triagem_G5 <- list(data.frame(
            NU_NOTIFIC      = character(),
            address         = character(),
            NU_NUMERO       = character(),
            ENDERECO_CNEFE  = character(),
            SIMILARITY      = numeric(),
            FASE            = character(),
            LATITUDE        = numeric(),
            LONGITUDE       = numeric(),
            DSC_LOCALIDADE  = character(),
            COD_SETOR       = character(),
            stringsAsFactors = FALSE
          ))

          triagem_G5[[1]][[meio_filtro_mun_ra]]  <- character()
          triagem_G5[[1]][[meio_filtro_mun_ra2]] <- character()
        } else {
          for (i in seq_len(total_notif_f5G)) {
            
            nu        <- notificacoes_f5G$NU_NOTIFIC[i]
            
            if (uf_selecionada == "DF") {
              muniresat <- notificacoes_f5G$Regiao_adm[i]
            } else {
              muniresat <- notificacoes_f5G$MUNIRESAT[i]
            }
            
            nu_numero <- notificacoes_f5G$NU_NUMERO[i]
            address   <- notificacoes_f5G$address[i]
            nm_logrado <- notificacoes_f5G$NM_LOGRADO[i]

            print(paste0(
              "Fase 5B geral - Processando registro ", i, " de ", total_notif_f5G,
              " | NU_NOTIFIC: ", nu,
              " | address: ", address
            ))
            flush.console()

            # Sem logradouro, `address` carrega só número e bairro -- e esta
            # subfase (G.5) nem filtra por município, comparando contra o
            # CNEFE da UF inteira. Mesmo risco de match espúrio por
            # coincidência de caracteres já documentado na FASE A.5 (ver
            # comentário lá); pular aqui é ainda mais importante, por não
            # ter nenhum filtro territorial reduzindo os candidatos.
            if (is.na(nm_logrado) || trimws(nm_logrado) == "") {
              res <- data.frame()
            } else {
              # ── Candidato vencedor (para georreferenciamento) ────────────────────────
              res <- dbGetQuery(con, sprintf("
    -- LIMIT em vez de janela sobre a tabela inteira: as duas ROW_NUMBER() de
    -- antes obrigavam o DuckDB a materializar e ordenar TODO o CNEFE da UF, uma
    -- vez por registro pendente, para no fim ficar com 15 linhas. Foi o que
    -- estourou a memória processando SP (failed to pin block, 3.7 GiB de 3.7
    -- GiB usados), onde a subfase G.5 nem tem filtro de município. Com
    -- ORDER BY ... LIMIT o DuckDB usa Top-N e guarda só as melhores candidatas.
    -- O limite de 2000 é folgado de propósito: serve para ainda sobrar material
    -- depois de descartar os endereços repetidos (prédios com muitas unidades),
    -- e o vencedor continua sendo o de maior similaridade.
    WITH candidatos AS (
      SELECT
        c.ENDERECO_CNEFE,
        c.LATITUDE,
        c.LONGITUDE,
        c.DSC_LOCALIDADE,
        c.COD_SETOR,
        %s,
        jaro_winkler_similarity(c.ENDERECO_CNEFE, '%s') AS SIMILARITY
      FROM %s AS c
      ORDER BY SIMILARITY DESC, c.ENDERECO_CNEFE
      LIMIT 2000
    ),
    enderecos_unicos AS (
      SELECT DISTINCT ON (ENDERECO_CNEFE) *
      FROM candidatos
      ORDER BY ENDERECO_CNEFE, SIMILARITY DESC
    ),
    rank_final AS (
      SELECT
        '%s'  AS NU_NOTIFIC,
        '%s'  AS address,
        '%s'  AS NU_NUMERO,
        ENDERECO_CNEFE,
        SIMILARITY,
        'FASE G.5' AS FASE,
        LATITUDE, LONGITUDE, DSC_LOCALIDADE, COD_SETOR,
        %s,
        ROW_NUMBER() OVER (
          ORDER BY SIMILARITY DESC, ENDERECO_CNEFE
        ) AS rank
      FROM enderecos_unicos
    )
    SELECT *
    FROM rank_final
    WHERE rank <= 15
    ORDER BY rank
  ",
                                           col_mun_ra,
                                           gsub("'", "''", as.character(address)),
                                           tabela_cnefe,
                                           nu,
                                           gsub("'", "''", as.character(address)),
                                           nu_numero,
                                           col_mun_ra2
              ))
            }

            if (nrow(res) > 0 &&
                !is.na(res$SIMILARITY[1]) &&
                res$SIMILARITY[1] >= limiar_similaridade) {
              resultados_fase5G[[i]] <- res
            }

            temp5G_df <- do.call(rbind, Filter(Negate(is.null), resultados_fase5G))

            # Triagem guarda só o resultado do PRÓPRIO registro -- ver nota
            # equivalente na FASE A5/4A.
            triagem_G5[i] <- list(
              if (nrow(res) > 0 && !is.na(res$SIMILARITY[1]) && res$SIMILARITY[1] >= limiar_similaridade) res else NULL
            )

            # NULL quando nenhum registro (de 1 até i) teve match ainda -- ver
            # comentário equivalente na FASE A4.
            if (!is.null(temp5G_df)) {
              temp5G_df <- temp5G_df %>% filter(rank == 1)
            }
            
            if (!is.null(temp5G_df) && nrow(temp5G_df) > 0) {
              
              dbWriteTable(con, "temp5G_AUX", temp5G_df, temporary = TRUE, overwrite = TRUE)
              
              # INSERT INTO acumula resultados a cada iteração
              dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempG5 AS
      SELECT
        NU_NOTIFIC,
        ADDRESS,
        NU_NUMERO,
        ENDERECO_CNEFE,
        SIMILARITY,
        0 AS DISTANCIA_NUM,
        FASE,
        LATITUDE,
        LONGITUDE,
        DSC_LOCALIDADE,
        COD_SETOR,
        %s
      FROM temp5G_AUX
    ", col_mun_ra2))
              
              total_G5 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempG5")$n
            } else {
              dbExecute(con, sprintf("
      CREATE OR REPLACE TEMP TABLE tempG5 AS
      SELECT
        CAST(NULL AS VARCHAR) AS NU_NOTIFIC,
        CAST(NULL AS VARCHAR) AS ADDRESS,
        CAST(NULL AS VARCHAR) AS NU_NUMERO,
        CAST(NULL AS VARCHAR) AS ENDERECO_CNEFE,
        CAST(NULL AS DOUBLE)  AS SIMILARITY,
        CAST(NULL AS INT)     AS DISTANCIA_NUM,
        CAST(NULL AS VARCHAR) AS FASE,
        CAST(NULL AS DOUBLE)  AS LATITUDE,
        CAST(NULL AS DOUBLE)  AS LONGITUDE,
        CAST(NULL AS VARCHAR) AS DSC_LOCALIDADE,
        CAST(NULL AS VARCHAR) AS COD_SETOR,
        CAST(NULL AS VARCHAR) AS %s,
        CAST(NULL AS VARCHAR) AS %s
      WHERE 1=0
    ", meio_filtro_mun_ra, meio_filtro_mun_ra2))
              
              total_G51 <- 0
              print("FASE 5G: nenhum registro atingiu o limiar de similaridade.")
            }
          } 
        }
        
        # --- Consolida Fase 5 (A + B) — FORA dos loops ───────────────────────────
        dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE temp5 AS
    SELECT *
    FROM tempA5
    UNION ALL
    SELECT *
    FROM tempG5
  ")
        
        total_fase5  <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp5")$n
        total_A5     <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempA5")$n
        # total_G5 nunca era recalculado aqui (só existia a variável de mesmo nome
        # atribuída dentro do loop da FASE 5G, que fica indefinida sempre que o
        # loop não roda ou a última iteração não encontra match) -- por isso o
        # print logo abaixo quebrava com "object 'total_G5' not found".
        total_G5     <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM tempG5")$n
        dbExecute(con, "DELETE FROM temp0 WHERE CAST(NU_NOTIFIC AS VARCHAR) IN (SELECT DISTINCT NU_NOTIFIC FROM temp5)")
        pendentes_f5 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM temp0")$n
        
        setProgress(
          value = 0.88,
          message = 'FASE 5 concluída',
          detail = paste(total_fase5, 'georreferenciados |', pendentes_f5, 'pendentes')
        )
        
        cat(paste0("FASE 5 concluída: ", total_fase5,
                   " georreferenciados | ", pendentes_f5, " pendentes",
                   " (A.5 = ", total_A5,
                   " | B.5 = ", total_G5, ")\n"))
        
        # ==============================================================
        # CONSOLIDAÇÃO FINAL
        # ==============================================================
        
        setProgress(
          value = 0.92,
          message = 'Consolidando resultados...',
          detail = 'Unindo todas as fases'
        )
        
        # Cada bloco abaixo monta triagem_XXX combinando os snapshots
        # acumulados por notificação; se NENHUMA notificação dessa fase teve
        # match (lista inteira de NULLs), do.call(rbind, ...) devolve NULL, e
        # NULL %>% mutate(...) quebra com "método não aplicável para 'mutate'
        # aplicado a um objeto de classe NULL". Por isso o mutate/select só
        # roda quando o rbind não é NULL -- se for, triagem_XXX permanece NULL
        # e o dplyr::bind_rows() mais abaixo já ignora entradas NULL de forma
        # segura. Reportado ao georreferenciar o BA: FASE 5A ficou com 0
        # matches (as 2 notificações pendentes só deram match na FASE 5G).
        triagem_A4 <- do.call(rbind, Filter(Negate(is.null), triagem_A4))
        if (!is.null(triagem_A4)) {
          triagem_A4 <- triagem_A4 %>%
            mutate(address = address3,
                   ENDERECO_CNEFE = ENDERECO_CNEFE3,
                   similarity = SIMILARITY) %>%
            select(-c(NU_NUMERO, address3, ENDERECO_CNEFE3, SIMILARITY))
        }

        # Igual ao bloco de triagem_A4 (mesma estrutura de colunas -- address3 +
        # ENDERECO_CNEFE3 -- já que B4 usa a mesma query de match por address3,
        # só sem o filtro de município). Antes tentava ler ADDRESS/DISTANCIA_NUM/
        # FASE/DSC_LOCALIDADE/COD_SETOR/CD_MUN, colunas que essa fase nunca
        # produz -- só não quebrava em testes anteriores porque nenhum deles
        # teve matches reais em B4 (ficava no branch "0 registros", com o
        # data.frame vazio de schema genérico que tem ADDRESS).
        triagem_B4 <- do.call(rbind, Filter(Negate(is.null), triagem_B4))
        if (!is.null(triagem_B4)) {
          triagem_B4 <- triagem_B4 %>%
            mutate(address = address3,
                   ENDERECO_CNEFE = ENDERECO_CNEFE3,
                   similarity = SIMILARITY) %>%
            select(-c(NU_NUMERO, address3, ENDERECO_CNEFE3, SIMILARITY))
        }

        triagem_A5 <- do.call(rbind, Filter(Negate(is.null), triagem_A5))
        if (!is.null(triagem_A5)) {
          triagem_A5 <- triagem_A5 %>%
            mutate(similarity = SIMILARITY) %>%
            select(-c(NU_NUMERO, FASE, DSC_LOCALIDADE,
                      COD_SETOR, CD_MUN, SIMILARITY))
        }

        # Mesma estrutura de colunas de triagem_A5 (a query de FASE G.5 já
        # produz "address" minúsculo, igual à A.5 -- não existe coluna ADDRESS
        # maiúscula nem DISTANCIA_NUM aqui). Antes tentava ler/dropar ADDRESS e
        # DISTANCIA_NUM, que só quebrava quando essa fase tinha match real (nos
        # testes até agora ela sempre ficou com "0 registros pendentes", então
        # o bug nunca foi exercitado -- é o mesmo padrão do bug já corrigido em
        # triagem_B4).
        triagem_G5 <- do.call(rbind, Filter(Negate(is.null), triagem_G5))
        if (!is.null(triagem_G5)) {
          triagem_G5 <- triagem_G5 %>%
            mutate(similarity = SIMILARITY) %>%
            select(-c(NU_NUMERO, FASE, DSC_LOCALIDADE,
                      COD_SETOR, CD_MUN, SIMILARITY))
        }
        
        dados_triagem <- dplyr::bind_rows(
          triagem_A4,
          triagem_B4,
          triagem_A5,
          triagem_G5
        )
        
        dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE TEMPGEO AS
    SELECT *,
    '1' as rank
    FROM temp1
    UNION ALL
    SELECT *,
    '1' as rank
     FROM temp2
    UNION ALL
    SELECT *,
    '1' as rank
     FROM temp3
    UNION ALL
    SELECT *,
    '1' as rank
     FROM temp4
    UNION ALL
    SELECT *,
    '1' as rank
    FROM temp5
  ")
        
        # Colunas que a consolidação abaixo (FINALGEO) grava no resultado --
        # nenhuma é um campo legítimo do SINAN ou de planilha própria; se o
        # arquivo enviado já tiver alguma, é quase certo que veio de um
        # processamento anterior deste painel (a saída baixada leva essas
        # mesmas colunas) e reprocessá-lo faz colidir com o que o pipeline
        # tenta gravar agora. Removidas aqui, antes do join, com aviso.
        # Sem isso: "rank" (sem alias na query) quebra a criação de FINALGEO
        # com "Ambiguous reference to column name rank" -- reportado
        # processando Recife/TB, ~29 mil linhas, depois de concluídas as 5
        # fases. As demais (LATITUDE, FASE, ENDERECO_CNEFE etc., todas com
        # alias explícito) NÃO travam: o DuckDB aceita duas colunas de saída
        # com o mesmo nome, e ao ler o resultado a segunda vira
        # "<coluna>_1" -- a coordenada real do match fica nessa coluna
        # renomeada, e o app segue lendo em silêncio o valor velho sob o
        # nome original (ex.: LATITUDE do arquivo do usuário, não a do
        # match). CD_MUN/NM_MUN não entram na lista -- ver o alias distinto
        # logo abaixo, que já resolve esse caso mantendo os dois lados.
        colunas_reservadas_finalgeo <- c(
          "ADDRESS_CORE", "ENDERECO_CNEFE", "similarity", "DISTANCIA_NUM",
          "FASE", "LATITUDE", "LONGITUDE", "DSC_LOCALIDADE", "COD_SETOR",
          "NM_MUN_CNEFE", "CD_MUN_CNEFE", "rank"
        )
        colisoes_finalgeo <- names(doenca)[toupper(names(doenca)) %in% toupper(colunas_reservadas_finalgeo)]
        if (length(colisoes_finalgeo) > 0) {
          showNotification(
            paste0(
              "O arquivo enviado já tinha as colunas ", paste(colisoes_finalgeo, collapse = ", "),
              " -- provavelmente de um processamento anterior deste painel. Removidas antes de reprocessar."
            ),
            type = "warning", duration = 15
          )
          doenca <- doenca %>% dplyr::select(-dplyr::any_of(colisoes_finalgeo))
        }

        dbWriteTable(con, "doenca_join", doenca, temporary = TRUE, overwrite = TRUE)

        # NM_MUN/CD_MUN do CNEFE ganham um alias distinto (NM_MUN_CNEFE/
        # CD_MUN_CNEFE) em vez de usar col_mun_ra cru -- D.* já pode conter
        # uma coluna CD_MUN do próprio usuário (ex: onda4BA, onda4RJ, que
        # declaram sua própria coluna de município como "CD_MUN"). Sem esse
        # alias, a query devolve DUAS colunas chamadas CD_MUN, e qualquer
        # select()/count() por nome nesse ponto em diante fica ambíguo,
        # arriscando descartar ou ler o valor errado (o do CNEFE em vez do
        # dado original do usuário, ou vice-versa). georreferenciar_UF() só
        # roda para UFs fora do DF, então col_mun_ra aqui é sempre
        # "C.NM_MUN, C.CD_MUN" -- por isso o alias fixo abaixo é seguro.
        dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE FINALGEO AS
    SELECT
      D.*,
      C.ADDRESS    AS ADDRESS_CORE,
      C.ENDERECO_CNEFE,
      C.SIMILARITY AS similarity,
      C.DISTANCIA_NUM,
      C.FASE,
      C.LATITUDE,
      C.LONGITUDE,
      C.DSC_LOCALIDADE,
      C.COD_SETOR,
      C.NM_MUN AS NM_MUN_CNEFE,
      C.CD_MUN AS CD_MUN_CNEFE,
      C.rank
    FROM doenca_join D
    LEFT JOIN TEMPGEO C ON C.NU_NOTIFIC = D.NU_NOTIFIC
  ")
        
        resultado <- dbGetQuery(con, "SELECT * FROM FINALGEO")
        resultado <- resultado %>% arrange(desc(similarity))
        
        resultado$rank <- as.character(resultado$rank)

        # -------------------------------------------------------
        # FALLBACK POR CENTROIDE DE POLÍGONO
        # -------------------------------------------------------
        # Quem saiu da última fase sem match recebe o ponto do polígono do
        # município informado ou, sem município válido, da UF -- ver
        # aplicar_fallback_centroide_UF() em georreferenciamento_funcoes.R.
        # Sem shapefile da UF em "Shapes UFs", esses casos seguem sem
        # coordenada, como antes, e o usuário é avisado.
        if (uf_selecionada != "DF") {
          setProgress(
            value = 0.93,
            message = 'Aplicando fallback por centroide...',
            detail = 'Casos sem match recebem o centroide do município ou da UF'
          )

          pontos_uf <- tryCatch(
            pontos_poligonos(carregar_shape_uf(uf_selecionada),
                             id = "CD_MUN", chave = uf_selecionada, nome = "NM_MUN"),
            error = function(e) {
              message("Fallback por centroide indisponível para ", uf_selecionada,
                      ": ", conditionMessage(e))
              NULL
            }
          )

          if (is.null(pontos_uf)) {
            showNotification(
              paste0("Sem malha de municípios para ", uf_selecionada,
                     ": os casos sem match ficaram sem coordenada."),
              type = "warning", duration = 10
            )
          } else {
            resultado <- aplicar_fallback_centroide_UF(resultado, pontos_uf)
          }
        }

        # Classificação final de cada caso -- ver classificar_georreferenciado()
        # e classificar_indicador() em georreferenciamento_funcoes.R. Quem
        # declarou só o endereço completo tem NM_LOGRADO/NM_COMPLEM NA (ver
        # padronizar_colunas_usuario): o endereço declarado faz o papel do
        # logradouro -- sem isso, todos esses casos saíam como endereço vazio.
        endereco_declarado <- input$escopo_arquivo == "OUTRO" &&
          !is.null(input$usu_address) && input$usu_address != ""
        resultado$GEORREFERENCIADO <- classificar_georreferenciado(
          resultado$FASE, resultado$LATITUDE,
          if (endereco_declarado) resultado$address else resultado$NM_LOGRADO,
          resultado$NM_COMPLEM
        )
        resultado$INDICADOR <- classificar_indicador(resultado)

        media_similarity <- dbGetQuery(con,
                                       "SELECT MEAN(similarity) AS media FROM FINALGEO"
        )$media
        
        duplicados <- dbGetQuery(con, "
    SELECT COUNT(*) - COUNT(DISTINCT NU_NOTIFIC) AS duplicados
    FROM FINALGEO
  ")$duplicados
        
        # Casos de fallback têm coordenada, mas não são georreferenciados por
        # endereço -- contados à parte.
        eh_centroide <- !is.na(resultado$FASE) & startsWith(resultado$FASE, "CENTROIDE_")
        georreferenciados_count <- sum(!is.na(resultado$LATITUDE) & !eh_centroide)
        centroide_count <- sum(eh_centroide)
        nao_georreferenciados_count <- sum(is.na(resultado$LATITUDE))

        setProgress(
          value = 0.95,
          message = 'Finalizando processamento...',
          detail = paste('Calculando métricas:', georreferenciados_count, 'georreferenciados')
        )

        cat(paste0(
          "Georreferenciados: ",     georreferenciados_count,
          " | Centroide (fallback): ", centroide_count,
          " | Não georreferenciados: ", nao_georreferenciados_count,
          " | Similaridade média: ",  round(media_similarity, 4),
          " | Duplicados: ",          duplicados, "\n"
        ))
        
        # Sem shutdown: a instância do CNEFE.duckdb é compartilhada por todas as
        # sessões do Shiny no mesmo processo R, e derrubá-la aqui interromperia
        # o processamento de quem estivesse rodando ao mesmo tempo. A conexão
        # desta execução é fechada (aqui e, se algo falhar antes, no on.exit).
        DBI::dbDisconnect(con)
        
        end   <- Sys.time()
        total <- end - begin
        
        # ==============================================================
        # MÉTRICAS FINAIS
        # ==============================================================
        
        setProgress(
          value = 0.98,
          message = 'Computando métricas finais...',
          detail = 'Últimos cálculos'
        )
        
        if(uf_selecionada == "DF"){
          contagem_casos_MUN_RA <- resultado %>%
            count(NM_SUBDIST) %>%
            mutate(proporcao = n / sum(n))
        } else{
          contagem_casos_MUN_RA <- resultado %>%
            count(NM_MUN_CNEFE) %>%
            mutate(proporcao = n / sum(n))
        }
        
        qualidade_enderecos <- resultado %>%
          count(QUALIDADE_ENDERECO) %>%
          mutate(proporcao = n / sum(n))
        
        qtd_tipo_endereco <- resultado %>%
          count(TIPO_ENDERECO) %>%
          mutate(proporcao = n / sum(n))
        
        setProgress(
          value = 1.0,
          message = 'Georreferenciamento concluído!'
        )
        
      }) # Fecha withProgress
      
      return(
        list(
          base_processada = resultado,
          base_triagem = dados_triagem,
          metricas = list(
            quantidade_de_casos     = quantidade_de_casos,
            contagem_casos_MUN_RA      = contagem_casos_MUN_RA,
            qualidade_enderecos     = qualidade_enderecos,
            qtd_tipo_endereco       = qtd_tipo_endereco,
            tempo_de_processamento  = total,
            # limite usado NESTE processamento (a triagem compara com ele)
            limiar_similaridade     = limiar_similaridade
          )
        )
      )

    } else {
      campos <- definir_campos(
        logradouro = 'NM_LOGRADO',
        cep = "NU_CEP",
        municipio = "MUNIRESAT",
        estado = "SG_UF",
        numero = "NU_NUMERO",
        localidade = "NM_BAIRRO"
      )
      
      withProgress(message = 'Iniciando georreferenciamento com o geocodebr.', value = 0, {
        
        total_registros <- nrow(doenca)
        
        ##########
        # ETAPA 1 - GEOCODIFICAÇÃO
        ##########
        
        setProgress(
          value = 0.1,
          message = "Preparando geocodificação...",
          detail = paste("Total de registros:", total_registros)
        )
        
        cat("Iniciando geocodificação...\n")
        
        res_geocode <- geocodebr::geocode(
          enderecos = doenca,
          campos_endereco = campos,
          resultado_completo = FALSE,
          resolver_empates = FALSE,
          resultado_sf = FALSE,
          verboso = FALSE,
          cache = TRUE,
          n_cores = 3
        )
        
        setProgress(
          value = 0.5,
          message = "Geocodificação concluída",
          detail = "Processando coordenadas..."
        )
        
        ### DEFINIÇÃO DE SETOR CENSITÁRIO
        # TRANSFORMA AS COORDENADAS PARA FORMATO APROPRIADO DO SHAPE FILE
        
        doenca_final <- res_geocode %>%
          rename(LATITUDE = lat, LONGITUDE = lon)

        doenca_sf <- doenca_final %>%
          filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
          mutate(LONGITUDE = as.numeric(LONGITUDE),
                 LATITUDE = as.numeric(LATITUDE)) %>%
          st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
        
        caminho_shape <- paste0(
          "SC_",
          uf_selecionada,
          ".rds"
        )
        
        # VERIFICA SE O ARQUIVO EXISTE
        if (!file.exists(caminho_shape)) {
          stop(paste("Shape file do setor censitário não encontrado para a UF:", uf_selecionada))
        }
        
        # LEITURA DO SHAPE FILE CONFORME UF SELECIONADA
        SC_coord_dados <- readRDS(caminho_shape)
        
        # VERIFICAR SE AS COORDENADAS ESTÃO NO MESMO SISTEMA
        if (st_crs(doenca_sf) != st_crs(SC_coord_dados)) {
          doenca_sf <- st_transform(doenca_sf, st_crs(SC_coord_dados))
        }
        
        # JOIN VIA SHAPE FILE, OBTENÇÃO DE DADOS DO SETOR CENSITÁRIO E INTERESEÇÃO DE PONTOS NA FRONTEIRA
        doenca_final <- doenca_sf %>% 
          st_join(SC_coord_dados[, c("CD_SETOR", "SITUACAO", "geometry")], join = st_intersects, left = TRUE) %>% 
          rename(ponto_sf_SC = geometry) %>% 
          st_drop_geometry()
        
        doenca_final <- doenca_final %>%
          select(
            # todas as colunas originais, exceto as georreferenciadas
            -c(TIPO_ENDERECO, QUALIDADE_ENDERECO, precisao, address, endereco_encontrado, desvio_metros, contagem_cnefe, CD_SETOR, SITUACAO),
            # colunas georreferenciadas no final, na ordem desejada
            TIPO_ENDERECO,
            QUALIDADE_ENDERECO,
            precisao,
            address,
            endereco_encontrado,
            desvio_metros,
            contagem_cnefe,
            CD_SETOR,
            SITUACAO
          )
        
        ##########
        # FINALIZAÇÃO
        ##########
        
        setProgress(
          value = 0.95,
          message = "Finalizando métricas...",
          detail = "Calculando indicadores"
        )
        
        end <- Sys.time()
        total <- end - begin
        
        ## MÉTRICAS
        
        resumo_precisao <- res_geocode %>%
          count(precisao)
        
        total_casos <- nrow(doenca)
        
        proporcao_cep <- res_geocode %>% 
          filter(precisao == 'cep') %>%
          nrow() / total_casos
        
        proporcao_localidade <- res_geocode %>% 
          filter(precisao == 'localidade') %>%
          nrow() / total_casos
        
        proporcao_logradouro <- res_geocode %>% 
          filter(precisao == 'logradouro') %>%
          nrow() / total_casos
        
        proporcao_municipio <- res_geocode %>% 
          filter(precisao == 'municipio') %>%
          nrow() / total_casos
        
        proporcao_numero_aproximado <- res_geocode %>% 
          filter(precisao == 'numero_aproximado') %>%
          nrow() / total_casos
        
        contagem_casos_MUN <- doenca %>% 
          count(MUNIRESAT) %>%
          mutate(proporcao = n / sum(n))
        
        qualidade_enderecos <- doenca %>% 
          count(QUALIDADE_ENDERECO) %>%
          mutate(proporcao = n / sum(n))
        
        qtd_tipo_endereco <- doenca %>% 
          count(TIPO_ENDERECO) %>%
          mutate(proporcao = n / sum(n))
        
        setProgress(
          value = 1,
          message = "Processamento concluído",
          detail = "Georreferenciamento finalizado"
        )
        
      })
      
      return(
        list(
          base_processada = doenca_final,
          metricas = list(
            resumo_precisao = resumo_precisao,
            tempo_de_processamento = total,
            total_casos = total_casos,
            proporcao_cep = proporcao_cep,
            proporcao_localidade = proporcao_localidade,
            proporcao_logradouro = proporcao_logradouro,
            proporcao_municipio = proporcao_municipio,
            proporcao_numero_aproximado = proporcao_numero_aproximado,
            tempo_de_processamento = total,
            contagem_casos_MUN = contagem_casos_MUN,
            qualidade_enderecos = qualidade_enderecos,
            qtd_tipo_endereco = qtd_tipo_endereco,
            quantidade_de_casos = quantidade_de_casos
          )
        )
      )
    }
  }
  
  # georreferenciar_DF() e georreferenciar_DF_batches() já carregadas
  # globalmente no topo do app.R (ver source() perto dos library()).
  
  # Cache em memória para evitar recarregar várias vezes
  carregar_shape_uf <- function(uf) {
    # Converter UF para maiúsculas
    uf <- toupper(uf)

    # Shapefiles de município/RA de cada UF ficam organizados nesta subpasta,
    # dentro da pasta do app (antes ficavam soltos na raiz).
    pasta_shapes <- "Shapes UFs"

    # Verificar quais shapefiles estão disponíveis
    arquivos_shp <- list.files(path = pasta_shapes, pattern = "\\.shp$")

    message(paste("Shapefiles disponíveis:", paste(arquivos_shp, collapse = ", ")))
    
    if (uf == "DF") {
      # Para DF, procurar por arquivos de Regiões Administrativas
      padroes_df <- "DF_subdistritos_CD2022\\.shp$"
      
      arquivo_encontrado <- NULL
      for (padrao in padroes_df) {
        matches <- grep(padrao, arquivos_shp, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) {
          arquivo_encontrado <- matches[1]
          break
        }
      }
      
      if (is.null(arquivo_encontrado)) {
        # Tentar qualquer arquivo que comece com DF
        matches <- grep("^DF.*\\.shp$", arquivos_shp, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) {
          arquivo_encontrado <- matches[1]
        } else {
          stop(paste(
            "Nenhum shapefile encontrado para o DF.",
            "\nArquivos disponíveis:", paste(arquivos_shp, collapse = ", ")
          ))
        }
      }
      
    } else {
      # Para outras UFs, procurar por municípios
      padroes_uf <- c(
        paste0("^", uf, "_Municipios_2024\\.shp$"),
        paste0("^", uf, ".*Municipio.*\\.shp$"),
        paste0("^", uf, ".*\\.shp$")
      )
      
      arquivo_encontrado <- NULL
      for (padrao in padroes_uf) {
        matches <- grep(padrao, arquivos_shp, ignore.case = TRUE, value = TRUE)
        if (length(matches) > 0) {
          arquivo_encontrado <- matches[1]
          break
        }
      }
      
      if (is.null(arquivo_encontrado)) {
        stop(paste(
          "Shapefile não encontrado para UF:", uf,
          "\nArquivos disponíveis:", paste(arquivos_shp, collapse = ", ")
        ))
      }
    }
    
    message(paste("Carregando shapefile:", arquivo_encontrado))

    # Carregar shapefile (agora dentro da subpasta "Shapes UFs")
    shape <- st_read(file.path(pasta_shapes, arquivo_encontrado), quiet = TRUE)

    # Alguns shapefiles baixados do IBGE vêm sem .prj (ou com .prj corrompido/
    # ausente no download), o que faz st_read() devolver um objeto sem CRS.
    # Sem isso, st_transform() mais adiante quebra com "cannot transform sfc
    # object with missing crs". As malhas territoriais do IBGE são sempre
    # publicadas em SIRGAS2000 (EPSG:4674), então assumimos esse CRS como
    # fallback nesse caso, em vez de derrubar o mapa.
    if (is.na(sf::st_crs(shape))) {
      warning(paste0(
        "Shapefile '", arquivo_encontrado, "' veio sem CRS definido ",
        "(.prj ausente/corrompido). Assumindo SIRGAS2000 (EPSG:4674), ",
        "padrão do IBGE."
      ))
      sf::st_crs(shape) <- 4674
    }

    if (nrow(shape) <= 1) {
      warning(paste(
        "O shapefile parece ter apenas", nrow(shape), "polígono(s).",
        "\nIsso pode indicar que está carregando apenas o contorno geral.",
        "\nVerifique se o shapefile contém as divisões internas."
      ))
    }
    
    return(shape)
  }
  
  # Função para ler arquivos grandes
  ler_arquivo_grande <- function(caminho, tipo = "csv", separador = NULL) {
    tryCatch({
      if (tipo %in% c("csv", "txt")) {
        # Verificar tamanho do arquivo
        file_info <- file.info(caminho)
        file_size_mb <- file_info$size / 1024^2
        
        # Detectar o separador automaticamente se não for especificado
        if (is.null(separador)) {
          separador <- detectar_separador(caminho)
        }
        
        # Estratégias diferentes baseadas no tamanho
        if (file_size_mb < 50) {
          # Arquivos pequenos: ler normalmente
          # check.names = FALSE: sem isso o read.csv troca espaços e acentos dos
          # nomes por pontos ("O3 logradouro final" vira "O3.logradouro.final"),
          # e a coluna declarada pelo usuário deixa de ser encontrada. O fread e
          # o read_excel, usados nos outros casos, preservam o nome original --
          # o mesmo arquivo só dava erro quando entrava por aqui (CSV < 50 MB).
          df <- read.csv(caminho, stringsAsFactors = FALSE, check.names = FALSE,
                         encoding = "UTF-8", sep = separador)
        } else if (file_size_mb < 200) {
          # Arquivos médios: usar data.table para eficiência
          if (requireNamespace("data.table", quietly = TRUE)) {
            df <- data.table::fread(caminho, showProgress = FALSE, 
                                    encoding = "UTF-8", sep = separador)
            df <- as.data.frame(df)
          } else {
            # Fallback para read.csv com otimizações
            df <- read.csv(caminho, stringsAsFactors = FALSE, check.names = FALSE,
                           nrows = -1, comment.char = "", 
                           encoding = "UTF-8", sep = separador)
          }
        } else {
          # Arquivos grandes: ler em chunks ou usar data.table
          if (requireNamespace("data.table", quietly = TRUE)) {
            df <- data.table::fread(caminho, showProgress = FALSE, 
                                    encoding = "UTF-8", sep = separador)
            df <- as.data.frame(df)
          } else {
            showNotification("Arquivo muito grande. Instale o pacote 'data.table' para melhor performance.", 
                             type = "warning")
            # Ler apenas as primeiras linhas para preview
            df <- read.csv(caminho, stringsAsFactors = FALSE, check.names = FALSE,
                           nrows = 1000, encoding = "UTF-8", sep = separador)
          }
        }
      } else if (tipo == "xlsx" | tipo == "xls") {
        if (requireNamespace("readxl", quietly = TRUE)) {
          df <- readxl::read_excel(caminho)
          df <- as.data.frame(df)
        } else {
          stop("Pacote 'readxl' necessário para ler arquivos Excel")
        }
      } else {
        stop(paste0(
          "Formato de arquivo não suportado: '", tipo, "'. ",
          "Formatos aceitos: csv, txt, xlsx, xls."
        ))
      }
      
      return(df)
      
    }, error = function(e) {
      stop(paste("Erro ao ler arquivo:", e$message))
    })
  }
  
  # Função auxiliar para detectar o separador automaticamente
  detectar_separador <- function(caminho, amostra_linhas = 5) {
    # Ler as primeiras linhas para análise
    linhas <- readLines(caminho, n = amostra_linhas, encoding = "UTF-8")
    
    if (length(linhas) == 0) {
      return(",") # Separador padrão
    }
    
    # Contar ocorrências de possíveis separadores. useBytes = TRUE em todo
    # gregexpr/grepl aqui: sem isso, um arquivo com bytes inválidos em
    # UTF-8 (comum em CSV do SINAN exportado em Latin-1/CP1252 com
    # acento) quebra com "unable to translate ... to a wide string" e
    # cascateia num erro tipo "substituto tem 0 linha, dados têm N" mais
    # adiante -- reproduzido em produção com um arquivo de DF. Como aqui
    # só procuramos caracteres ASCII (vírgula/ponto-e-vírgula/tab), casar
    # por byte é igualmente correto e imune a texto com encoding inválido
    # ou desconhecido no restante da linha.
    contagens <- list(
      virgula = sum(sapply(linhas, function(x) lengths(regmatches(x, gregexpr(",", x, useBytes = TRUE))))),
      ponto_virgula = sum(sapply(linhas, function(x) lengths(regmatches(x, gregexpr(";", x, useBytes = TRUE))))),
      tab = sum(sapply(linhas, function(x) lengths(regmatches(x, gregexpr("\t", x, useBytes = TRUE)))))
    )

    # Verificar se o arquivo tem cabeçalho e dados
    if (length(linhas) > 1) {
      # Usar a linha de dados (segunda linha) para decisão mais precisa
      linha_dados <- linhas[2]

      # Se houver tabulação, é provavelmente tab
      if (grepl("\t", linha_dados, useBytes = TRUE)) {
        return("\t")
      }

      # Contar separadores na linha de dados
      virgulas_dados <- lengths(regmatches(linha_dados, gregexpr(",", linha_dados, useBytes = TRUE)))
      ponto_virgulas_dados <- lengths(regmatches(linha_dados, gregexpr(";", linha_dados, useBytes = TRUE)))
      
      # Heurística: escolher o separador com mais ocorrências na linha de dados
      if (ponto_virgulas_dados > virgulas_dados && ponto_virgulas_dados > 0) {
        return(";")
      } else if (virgulas_dados > 0) {
        return(",")
      }
    }
    
    # Fallback: escolher o separador com maior contagem geral
    if (contagens$ponto_virgula > contagens$virgula && contagens$ponto_virgula > 0) {
      return(";")
    } else if (contagens$virgula > 0) {
      return(",")
    } else if (contagens$tab > 0) {
      return("\t")
    }
    
    # Se não detectar nada, usar vírgula como padrão
    return(",")
  }
  
  # FUNÇÕES PARA KERNEL DENSITY ESTIMATION
  kde2d_custom <- function(x, y, h, n = 100, lims = c(range(x), range(y))) {
    # Implementação customizada do kde2d
    nx <- length(x)
    if (length(y) != nx) 
      stop("data vectors must be the same length")
    
    if (missing(h)) 
      h <- c(bandwidth.nrd_custom(x), bandwidth.nrd_custom(y))
    
    gx <- seq(lims[1], lims[2], length = n)
    gy <- seq(lims[3], lims[4], length = n)
    
    # Função kernel gaussiano
    gaussian_kernel <- function(u) {
      (1 / sqrt(2 * pi)) * exp(-0.5 * u^2)
    }
    
    # Calcular densidade
    z <- matrix(0, n, n)
    
    for (i in 1:nx) {
      # Distâncias normalizadas
      dx <- (gx - x[i]) / h[1]
      dy <- (gy - y[i]) / h[2]
      
      # Kernel 2D
      kx <- gaussian_kernel(dx)
      ky <- gaussian_kernel(dy)
      
      # Adicionar contribuição deste ponto
      z <- z + outer(kx, ky)
    }
    
    # Normalizar
    z <- z / (nx * h[1] * h[2])
    
    return(list(x = gx, y = gy, z = z))
  }
  
  # FUNÇÃO PARA CÁLCULO DO PARÂMETRO DE SUAVIZAÇÃO
  bandwidth.nrd_custom <- function(x) {
    # Estimativa de bandwidth usando a regra de Silverman
    n <- length(x)
    if (n < 2) return(1.0)
    
    # Estimativa de desvio padrão
    sigma <- sd(x)
    
    # IQR (Interquartile Range)
    q <- quantile(x, c(0.25, 0.75))
    iqr <- q[2] - q[1]
    
    # Estimativa robusta de desvio (evita outliers)
    sigma_robust <- min(sigma, iqr / 1.34)
    
    # Bandwidth de Silverman
    h <- 0.9 * sigma_robust * n^(-1/5)
    
    return(max(h, 0.01))  # Mínimo para evitar zeros
  }
  
  # Função para calcular densidade 2D alternativa usando stat_density_2d do ggplot2
  calcular_densidade_ggplot <- function(pontos_sf, df_shape, bandwidth = 0.1) {
    # Extrair coordenadas
    coords <- st_coordinates(pontos_sf)
    
    # Converter para data frame
    df_coords <- data.frame(
      x = coords[, "X"],
      y = coords[, "Y"]
    )
    
    return(df_coords)
  }
  
  ######
  ###### FUNÇÕES ######################
  ######
  ##########################################
  ##########################################
  ##########################################
  ##########################################
  
  # Observar upload de arquivos
  observe({
    req(input$arquivo_doenca)
    req(input$uf)
    
    resultados$arquivos_carregados <- TRUE
  })
  
  # Informações sobre arquivos carregados
  output$info_arquivos <- renderUI({
    req(resultados$arquivos_carregados)
    
    info_doenca <- NULL
    
    if (!is.null(input$arquivo_doenca)) {
      size_mb <- round(input$arquivo_doenca$size / 1024^2, 2)
      info_doenca <- tags$div(
        class = "file-input-info",
        tags$strong("Doença: "), input$arquivo_doenca$name,
        tags$br(), "Tamanho: ", size_mb, "MB"
      )
    }
    
    if (!is.null(input$uf)) {
      escopo_texto <- ifelse(input$escopo == "uf", 
                             paste("UF:", input$uf),
                             paste("Município:", input$codigo_municipio, "- UF:", input$uf))
      
      info_cnefe <- tags$div(
        class = "file-input-info",
        tags$strong("CNEFE: "), "Carregado automaticamente",
        tags$br(), "Escopo: ", escopo_texto
      )
    } else {
      info_cnefe <- NULL
    }
    
    tagList(info_doenca, info_cnefe)
  })
  
  # Processamento principal - CORRIGIDO
  observeEvent(input$processar, {
    req(input$arquivo_doenca)
    req(input$uf)
    req(input$escopo_arquivo)
    
    # tryCatch deve envolver todo o bloco de processamento
    tryCatch({
      
      # PRIMEIRO withProgress para leitura
      withProgress(message = 'Lendo arquivos...', value = 0, {
        
        # A UI e ler_arquivo_grande() já suportavam csv/txt/xlsx/xls, mas o
        # tipo nunca era detectado aqui -- sempre caía no default tipo="csv",
        # quebrando (ou lendo incorretamente) qualquer upload de xlsx/xls.
        # input$arquivo_doenca$name preserva o nome original (com extensão);
        # $datapath é só um arquivo temporário sem extensão significativa.
        tipo_arquivo <- tolower(tools::file_ext(input$arquivo_doenca$name))
        doenca <- ler_arquivo_grande(input$arquivo_doenca$datapath, tipo = tipo_arquivo)
        
        incProgress(0.3)
        
        # ESCOLHER FUNÇÃO DE PROCESSAMENTO BASEADO NA UF
        uf_selecionada <- input$uf
        
        if (input$escopo_arquivo == "SINAN") {
          
          doenca <- etl_sinan(doenca, input$uf)
          
        } else {
          
          doenca <- padronizar_colunas_usuario(
            df       = doenca,
            cod_unico = input$usu_cod_unico,
            logr     = input$usu_logr,
            num      = input$usu_num,
            mun = input$usu_mun,
            ra = input$usu_ra,
            compl    = input$usu_compl,
            bairro   = input$usu_bairro,
            address  = input$usu_address,
            uf = input$uf
          )
        }
        
        # ESCOLHA DO ESCOPO: UF ou MUNICÍPIO
        if (input$escopo == "municipio") {
          # Validar se código do município foi informado
          req(input$codigo_municipio)
          if (input$codigo_municipio == "") {
            stop("Para escopo municipal, informe o código do município")
          }
          
        } else if (input$escopo == "uf") {
          showNotification(paste("Processando ", uf_selecionada, "..."), type = "message")
        } else {
          stop("Escopo inválido selecionado")
        }
        
        incProgress(0.7) # Avançar para 70% após leitura e validação
        
      }) # FIM do primeiro withProgress
      
      # SEGUNDO withProgress para processamento
      withProgress(message = 'Processando dados...', value = 0, {
        
        if (uf_selecionada == "DF") {
          # Pipeline determinístico de 18 fases, com batching automático
          showNotification("Usando pipeline otimizado para DF...", type = "message")

          batch_size_df <- 3000

          # Atualiza a barra de progresso com o lote e a fase do pipeline em
          # andamento — mesma informação que já aparecia no console, agora
          # refletida no card de progresso do Shiny.
          progress_cb_df <- function(info) {
            if (is.null(info$fase)) {
              setProgress(
                value   = (info$lote - 1) / info$n_lotes,
                message = sprintf("Processando lote %d de %d...", info$lote, info$n_lotes),
                detail  = sprintf("até %d registros neste lote", batch_size_df)
              )
            } else {
              setProgress(
                value  = (info$lote - 1) / info$n_lotes,
                detail = sprintf("Lote %d/%d — %s (%d casos)", info$lote, info$n_lotes, info$fase, info$n)
              )
            }
          }

          resultado <- georreferenciar_DF_batches(
            doenca,
            batch_size = batch_size_df,
            progress_callback = progress_cb_df
          )
        } else {
          # Usar função processar_doenca_5 para outros casos
          showNotification(paste("Usando método para", uf_selecionada, "..."), type = "message")

          # Chamar função com ou sem código do município
          if(input$escopo == 'municipio'){
            resultado <- georreferenciar_UF(doenca, input$codigo_municipio, input$uf, input$similarity_threshold)
          } else {
            resultado <- georreferenciar_UF(doenca, codigo_municipio = NULL, input$uf, input$similarity_threshold)
          }
        }

        incProgress(1) # Completar 100%

        # Armazenar resultados
        # O pipeline DF não gera candidatos de triagem manual (base_triagem) —
        # produz georref_falha (casos sem nenhum match). A aba de Triagem fica
        # travada para DF (ver guards em input$carregar_triagem / box_endereco_atual / tabela_triagem).
        resultados$base_processada <- resultado$base_processada
        resultados$base_triagem <- if (uf_selecionada == "DF") NULL else resultado$base_triagem
        resultados$metricas <- resultado$metricas

        # Distribuição de casos por fase de match (aba Métricas Detalhadas).
        # Calculada aqui, sobre a base final, e não dentro de cada pipeline:
        # assim DF e demais UFs seguem a mesma definição -- percentual sobre o
        # total de casos, somando 100% -- e a coluna de fase ainda é lida antes
        # que o bloco abaixo a descarte quando o usuário declara as próprias
        # colunas (FASE sai nesse caso, nas UFs não-DF). No DF isso também
        # inclui os rótulos atribuídos depois do SQL (CENTROIDE_DF,
        # INSTITUCIONAL_FIXO), que o contagem_por_fase do pipeline não enxerga.
        # O GeocodeBr não tem fases (usa `precisao`); nesse caso a tabela não é
        # montada.
        col_fase <- if (uf_selecionada == "DF") "FASE_MATCH" else "FASE"
        if (!is.null(resultados$base_processada) &&
            col_fase %in% names(resultados$base_processada)) {
          fases <- as.character(resultados$base_processada[[col_fase]])
          fases[is.na(fases) | fases == ""] <- "Não georreferenciado"
          distribuicao <- as.data.frame(table(fases), stringsAsFactors = FALSE)
          names(distribuicao) <- c("FASE", "N")
          distribuicao$PCT <- round(100 * distribuicao$N / sum(distribuicao$N), 2)
          # Mais frequentes primeiro; "Não georreferenciado" sempre por último
          distribuicao <- distribuicao[
            order(distribuicao$FASE == "Não georreferenciado", -distribuicao$N), ]
          resultados$metricas$distribuicao_fases <- distribuicao

          # Precisão posicional de cada caso (ver classificar_precisao() em
          # graficos_funcoes.R) e os dados da aba Gráficos -- calculados aqui
          # pelo mesmo motivo da distribuição acima: a fase, o município
          # informado e a RA saem da base no descarte logo abaixo. A coluna
          # PRECISAO_POSICIONAL fica no resultado: o mapa usa para separar
          # endereço de centroide, e quem baixa os dados pode filtrar por ela.
          eh_df <- uf_selecionada == "DF"
          base_g <- resultados$base_processada
          base_g$PRECISAO_POSICIONAL <- as.character(classificar_precisao(
            base_g[[col_fase]], base_g$DISTANCIA_NUM, base_g$LATITUDE, eh_df
          ))
          resultados$base_processada <- base_g

          # Nomes dos municípios da malha já carregada no fallback por centroide
          nomes_unidade <- NULL
          pontos_cache <- .cache_pontos_poligonos[[uf_selecionada]]
          if (!eh_df && !is.null(pontos_cache$unidades$NOME)) {
            nomes_unidade <- setNames(pontos_cache$unidades$NOME,
                                      substr(pontos_cache$unidades$ID, 1, 6))
          }
          resultados$metricas$graficos <- preparar_graficos(
            base_g, eh_df, limiar = input$similarity_threshold, nomes_unidade = nomes_unidade
          )
        }

        resultados$processado <- TRUE

        # ----------------------------------------------------------------
        # Quando o usuário declara colunas de endereço (dados não-SINAN),
        # o resultado final descarta as colunas "estilo SINAN" que são só
        # cópias/derivadas do que ele mesmo declarou (NM_LOGRADO, NU_NUMERO,
        # NM_COMPLEM, NM_BAIRRO, address2/address3, TIPO_ENDERECO,
        # QUALIDADE_ENDERECO, e as específicas de cada pipeline: MUNIRESAT/
        # FASE/NM_MUN/CD_MUN/rank para as demais UFs, ID_DISTRIT/Regiao_adm/
        # Regiao_sau1 para o DF). Mantém as colunas originais do usuário e as
        # que são resultado genuíno do match (ENDERECO_CNEFE, LATITUDE/LONGITUDE,
        # GEORREFERENCIADO, INDICADOR, COD_SETOR/CD_SETOR, DSC_LOCALIDADE, similarity,
        # DISTANCIA_NUM, NM_SUBDIST/FASE_MATCH/CD_SUBDIST no caso do DF).
        # any_of() ignora silenciosamente colunas que não existem nesse
        # pipeline/UF, então a mesma lista serve para DF e não-DF.
        # NU_NOTIFIC só sai da lista de descarte quando o usuário não
        # declarou cod_unico -- nesse caso é o único identificador
        # disponível; quando cod_unico é declarado, NU_NOTIFIC é só uma
        # cópia dele e a coluna original já continua no resultado.
        #
        # "address" (endereço submetido, construído a partir do que o
        # usuário declarou) só entra no descarte fora do DF, onde é
        # redundante com ADDRESS_CORE (o endereço que de fato "ganhou" o
        # match no CNEFE). O pipeline do DF (GEOCODING_DF_CORRIDO.sql) não
        # produz um equivalente a ADDRESS_CORE -- só ENDERECO_CNEFE -- então
        # mantemos "address" lá como a única referência ao endereço original
        # no resultado final.
        # ----------------------------------------------------------------
        if (input$escopo_arquivo == "OUTRO" && !is.null(resultados$base_processada)) {
          colunas_sinan_derivadas <- c(
            "NM_LOGRADO", "NU_NUMERO", "NM_COMPLEM", "NM_BAIRRO",
            "address2", "address3",
            "TIPO_ENDERECO", "QUALIDADE_ENDERECO",
            "MUNIRESAT", "FASE", "NM_MUN_CNEFE", "CD_MUN_CNEFE", "rank",
            "ID_DISTRIT", "Regiao_adm", "Regiao_sau1"
          )

          if (uf_selecionada != "DF") {
            colunas_sinan_derivadas <- c(colunas_sinan_derivadas, "address")
          }

          cod_unico_declarado <- !is.null(input$usu_cod_unico) && input$usu_cod_unico != ""
          if (cod_unico_declarado) {
            colunas_sinan_derivadas <- c(colunas_sinan_derivadas, "NU_NOTIFIC")
          }

          resultados$base_processada <- resultados$base_processada %>%
            dplyr::select(-dplyr::any_of(colunas_sinan_derivadas))
        }

        showNotification("Processamento concluído com sucesso!", type = "message")

      }) # FIM do segundo withProgress

      # Leva o usuário direto ao Resumo assim que o georreferenciamento
      # termina — a aba Início é a de abertura (apresentação/passo a passo),
      # e sem isso o resultado ficava numa aba que o usuário precisava
      # encontrar sozinho. Fica FORA do withProgress (barra de progresso já
      # fechada) e DENTRO do tryCatch, para não trocar de aba quando o
      # processamento falha — nesse caso o usuário permanece onde está e vê a
      # notificação de erro.
      updateTabsetPanel(session, "abas_principais", selected = "resumo")

    }, error = function(e) {
      # Este error handler pertence ao tryCatch externo
      showNotification(paste("Erro no processamento:", e$message), type = "error")
    })
    
  }) # FIM do observeEvent
  
  # Output para verificar se o processamento foi concluído
  output$processamento_concluido <- reactive({
    resultados$processado
  })
  outputOptions(output, "processamento_concluido", suspendWhenHidden = FALSE)
  
  ### MAPA DE COORDENADAS

  # Carrega o shapefile de contorno (municípios/RA) uma única vez por UF, em
  # vez de recarregar do disco toda vez que o mapa re-renderiza. Sem isso,
  # como renderLeaflet também depende de input$kde_bandwidth/kde_alpha/
  # mapa_tipo_visualizacao, qualquer ajuste nesses controles reexecutava o
  # bloco inteiro e relia o shapefile do zero. reactive() cacheia o
  # resultado e só recalcula quando input$uf muda de fato.
  shape_uf_reativo <- reactive({
    req(input$uf)
    tryCatch(
      sf::st_transform(carregar_shape_uf(input$uf), crs = 4326),
      error = function(e) {
        message(paste0(
          "Contorno de município/RA não disponível para ", input$uf,
          ": ", conditionMessage(e)
        ))
        NULL
      }
    )
  })

  output$mapa_coordenadas <- renderLeaflet({
    
    # ----------------------------
    # Requisitos básicos
    # ----------------------------
    req(resultados$base_processada)
    req(input$uf)
    
    # ----------------------------
    # Preparação dos dados
    # ----------------------------
    dados_mapa <- resultados$base_processada %>%
      dplyr::mutate(
        LONGITUDE = as.numeric(LONGITUDE),
        LATITUDE  = as.numeric(LATITUDE)
      ) %>%
      dplyr::filter(
        !is.na(LONGITUDE), !is.na(LATITUDE),
        dplyr::between(LONGITUDE, -180, 180),
        dplyr::between(LATITUDE, -90, 90)
      )

    # Endereço x centroide (fallback), a partir da PRECISAO_POSICIONAL gravada
    # no processamento. O GeocodeBr não tem essa coluna: todos os pontos
    # contam como endereço.
    dados_mapa$GRUPO_POS <- if ("PRECISAO_POSICIONAL" %in% names(dados_mapa)) {
      grupo_precisao(dados_mapa$PRECISAO_POSICIONAL)
    } else {
      rep("Endereço", nrow(dados_mapa))
    }

    # ----------------------------
    # Definição do campo de município
    # ----------------------------
    # Detecta pela presença real da coluna em dados_mapa, em vez de assumir
    # com base em input$uf/input$metodo. Antes, trocar a UF no seletor sem
    # clicar em "Processar Dados" de novo mantinha resultados$base_processada
    # com o formato da UF processada anteriormente (ex: DF, com NM_SUBDIST),
    # enquanto input$uf já refletia a nova seleção -- o código assumia
    # NM_MUN (ou NM_SUBDIST/NM_BAIRRO) sem essa coluna realmente existir,
    # quebrando addCircleMarkers com "objeto 'NM_MUN' não encontrado".
    # NM_MUN_CNEFE (não NM_MUN) porque a query FINALGEO dá esse alias ao lado
    # do CNEFE, evitando colisão com um eventual CD_MUN/NM_MUN próprio do
    # usuário nos dados declarados.
    campo_municipio <- if ("NM_BAIRRO" %in% names(dados_mapa)) {
      "NM_BAIRRO"
    } else if ("NM_SUBDIST" %in% names(dados_mapa)) {
      "NM_SUBDIST"
    } else if ("NM_MUN_CNEFE" %in% names(dados_mapa)) {
      "NM_MUN_CNEFE"
    } else {
      NA_character_
    }

    # ----------------------------
    # Definição do campo/rótulo de identificação (popup do mapa)
    # ----------------------------
    # Mesmo raciocínio do campo_municipio: detecta pela presença real da
    # coluna, em vez de fixar NU_NOTIFIC. Quando o usuário declara colunas de
    # endereço (dados não-SINAN) E também declara cod_unico, o resultado
    # final não tem mais NU_NOTIFIC (é descartado por ser cópia da coluna
    # que o próprio usuário já declarou) -- popup_label quebrava com "objeto
    # 'NU_NOTIFIC' não encontrado". O rótulo também muda: "Notificação" só
    # faz sentido pra dado que é (ou se comporta como) SINAN; quando é a
    # coluna de ID do próprio usuário, mostra só "ID" para não misturar
    # terminologia SINAN com um dado que não é SINAN.
    campo_identificador <- NA_character_
    rotulo_identificador <- "ID"
    if (!is.null(input$usu_cod_unico) && input$usu_cod_unico != "" &&
        input$usu_cod_unico %in% names(dados_mapa)) {
      campo_identificador <- input$usu_cod_unico
    } else if ("NU_NOTIFIC" %in% names(dados_mapa)) {
      campo_identificador <- "NU_NOTIFIC"
      rotulo_identificador <- "Notificação"
    }

    # Monta o HTML do popup a partir do que realmente está disponível
    # (município e/ou identificador), em vez de duplicar essa lógica nas
    # duas visualizações (Pontos e KDE-com-pontos) -- as duas usam a mesma
    # coluna POPUP_HTML (ver logo abaixo), evitando que uma seja corrigida
    # e a outra não da próxima vez que algo aqui precisar mudar.
    #
    # Indexa a coluna por nome (dados_mapa[[campo]]) em vez de colar o nome
    # dela dentro de um texto e virar fórmula com as.formula() -- a versão
    # anterior quebrava com "Error in str2lang: unexpected symbol" sempre
    # que o nome tinha espaço ou qualquer caractere que não formasse um
    # identificador R válido (ex.: a coluna de ID declarada pelo usuário em
    # dados não-SINAN podia se chamar "ID participante"). Indexação por
    # nome funciona com qualquer texto de nome de coluna, sem essa
    # restrição -- sem 'as.formula', sem risco de erro de parse.
    construir_popup_mapa <- function() {
      linhas <- list()
      if (!is.na(campo_municipio)) {
        linhas[[length(linhas) + 1]] <- paste0("<b>Município:</b> ", dados_mapa[[campo_municipio]])
      }
      if (!is.na(campo_identificador)) {
        linhas[[length(linhas) + 1]] <- paste0("<b>", rotulo_identificador, ":</b> ", dados_mapa[[campo_identificador]])
      }
      if ("PRECISAO_POSICIONAL" %in% names(dados_mapa)) {
        linhas[[length(linhas) + 1]] <- paste0("<b>Posição:</b> ", dados_mapa$PRECISAO_POSICIONAL)
      }
      if (length(linhas) == 0) {
        return(rep("", nrow(dados_mapa)))
      }
      Reduce(function(a, b) paste(a, b, sep = "<br>"), linhas)
    }
    # Calculado uma vez sobre dados_mapa inteiro; adicionar_pontos() só
    # subdivide por GRUPO_POS, então a mesma coluna serve às duas chamadas
    # (Pontos e KDE-com-pontos).
    dados_mapa$POPUP_HTML <- construir_popup_mapa()

    # ----------------------------
    # Segurança: sem dados
    # ----------------------------
    if (nrow(dados_mapa) == 0) {
      showNotification(
        "Nenhuma coordenada válida para a UF selecionada",
        type = "warning"
      )
      
      return(
        leaflet() %>%
          addProviderTiles(
            providers$Esri.WorldGrayCanvas,
            options = providerTileOptions(maxNativeZoom = 16, maxZoom = 20)
          ) %>%
          setView(lng = -47.9, lat = -15.8, zoom = 4)
      )
    }
    
    # ----------------------------
    # Base do mapa
    # ----------------------------
    # Basemap do Esri (World Light Gray Base): fundo cinza claro e discreto,
    # que mantem os pontos e o contorno das RAs/municipios legiveis -- mesmo
    # papel que o CartoDB.Positron cumpria antes de deixar de ser gratuito.
    #
    # As duas opcoes preservam o alcance de zoom que se tinha com o CartoDB
    # (maxZoom 20). Essa camada do Esri so publica tiles ate o zoom 16 e
    # declara maxZoom = 16, o que travaria o mapa nesse nivel -- 4 niveis a
    # menos de aproximacao, justamente onde se confere um endereco especifico.
    # maxZoom = 20 devolve o alcance e maxNativeZoom = 16 diz ao Leaflet para
    # reaproveitar (escalando) o tile do zoom 16 acima desse ponto, em vez de
    # pedir tiles que nao existem.
    mapa <- leaflet(dados_mapa) %>%
      addProviderTiles(
        providers$Esri.WorldGrayCanvas,
        options = providerTileOptions(maxNativeZoom = 16, maxZoom = 20)
      )

    # ----------------------------
    # Contorno dos municípios/RA da UF (se houver shapefile disponível em
    # "Shapes UFs/" -- nem toda UF tem um ainda, então isso é best-effort e
    # não deve derrubar o mapa quando faltar). Vem do reactive() cacheado
    # acima, não recarrega do disco a cada re-render.
    # ----------------------------
    shape_uf <- shape_uf_reativo()

    # ----------------------------
    # VISUALIZAÇÃO: KDE
    # ----------------------------
    # Pontos coloridos pela leitura única do painel: azul = endereço,
    # laranja = centroide (fallback). Anel branco fino em volta de cada ponto
    # para separar os que se sobrepõem. Usado pelas duas visualizações.
    adicionar_pontos <- function(mapa, dados, raio, cluster) {
      for (grp in c("Endereço", "Centroide")) {
        d <- dados[dados$GRUPO_POS == grp, , drop = FALSE]
        if (nrow(d) == 0) next
        mapa <- mapa %>%
          addCircleMarkers(
            data = d,
            lng = ~LONGITUDE,
            lat = ~LATITUDE,
            radius = raio,
            stroke = TRUE,
            color = "#ffffff",
            weight = 1,
            fillColor = CORES_GRUPO[[grp]],
            fillOpacity = 0.85,
            popup = d$POPUP_HTML,
            clusterOptions = if (cluster) markerClusterOptions() else NULL
          )
      }
      mapa
    }

    # Legenda só com os grupos que aparecem no mapa.
    adicionar_legenda <- function(mapa, dados) {
      grupos <- intersect(c("Endereço", "Centroide"), unique(dados$GRUPO_POS))
      if (length(grupos) == 0) return(mapa)
      mapa %>%
        addLegend(
          position = "bottomright",
          colors = unname(CORES_GRUPO[grupos]),
          labels = c("Endereço" = "Posição por endereço",
                     "Centroide" = "Centroide (fallback)")[grupos],
          title = "Posição",
          opacity = 1
        )
    }

    if (input$mapa_tipo_visualizacao == "densidade") {

      # Casos de centroide ficam fora da densidade por padrão: todos os de uma
      # unidade caem no mesmo ponto, e o acúmulo vira uma mancha artificial.
      dados_kde <- if (isTRUE(input$kde_incluir_centroides)) {
        dados_mapa
      } else {
        dados_mapa[dados_mapa$GRUPO_POS == "Endereço", , drop = FALSE]
      }
      n_fora <- nrow(dados_mapa) - nrow(dados_kde)

      if (nrow(dados_kde) > 0) {
        mapa <- mapa %>%
          addHeatmap(
            data = dados_kde,
            lng = ~LONGITUDE,
            lat = ~LATITUDE,
            blur = input$kde_bandwidth * 100,
            radius = input$kde_bandwidth * 80,
            max = 1,
            minOpacity = input$kde_alpha
          )

        # Pontos opcionais sobre o KDE (os mesmos que entram na densidade)
        if (isTRUE(input$kde_mostrar_pontos)) {
          mapa <- adicionar_pontos(mapa, dados_kde, raio = 3, cluster = FALSE) %>%
            adicionar_legenda(dados_kde)
        }
      } else {
        showNotification(
          "Nenhum caso com posição de endereço para a densidade. Marque a opção de incluir os casos de centroide.",
          type = "warning"
        )
      }

      # Nota no próprio mapa -- vai junto na imagem exportada.
      nota_kde <- paste0(
        "<b>Densidade:</b> ", fmt_n(nrow(dados_kde)), " casos",
        if (n_fora > 0) paste0("<br>", fmt_n(n_fora), " casos de centroide fora da densidade")
        else if (any(dados_mapa$GRUPO_POS == "Centroide")) "<br>inclui casos de centroide"
        else ""
      )
      mapa <- mapa %>%
        addControl(html = nota_kde, position = "bottomright")

    } else {

      # ----------------------------
      # VISUALIZAÇÃO: PONTOS
      # ----------------------------
      mapa <- adicionar_pontos(mapa, dados_mapa, raio = 5,
                               cluster = nrow(dados_mapa) > 1000) %>%
        adicionar_legenda(dados_mapa)
    }

    # Contorno por cima dos pontos/densidade (sem preenchimento, só a borda,
    # pra não esconder o que já foi desenhado).
    if (!is.null(shape_uf)) {
      mapa <- mapa %>%
        addPolygons(
          data = shape_uf,
          fill = FALSE,
          color = "#555555",
          weight = 1,
          opacity = 0.6
        )
    }

    # ----------------------------
    # Ajustes finais
    # ----------------------------
    # fitBounds() com uma caixa de tamanho zero -- todos os pontos na mesma
    # coordenada, caso de um único registro no arquivo, ou de vários caindo
    # no mesmo centroide de fallback -- não tem largura/altura para calcular
    # o zoom a partir, e o Leaflet estoura pro maxZoom do mapa (20). Nesse
    # zoom o provedor Esri (nativo só até 16) mostra o tile do zoom 16
    # ampliado 16x, uma mancha cinza borrada em vez de um mapa vazio de
    # verdade. Reportado processando AL, com um único caso no arquivo.
    # setView() com um zoom fixo (13, dentro do nativo do provedor) evita
    # isso quando a "caixa" não tem tamanho real.
    caixa_degenerada <- diff(range(dados_mapa$LONGITUDE)) == 0 &&
      diff(range(dados_mapa$LATITUDE)) == 0

    mapa <- if (caixa_degenerada) {
      mapa %>% setView(lng = dados_mapa$LONGITUDE[1], lat = dados_mapa$LATITUDE[1], zoom = 13)
    } else {
      mapa %>% fitBounds(
        lng1 = min(dados_mapa$LONGITUDE),
        lat1 = min(dados_mapa$LATITUDE),
        lng2 = max(dados_mapa$LONGITUDE),
        lat2 = max(dados_mapa$LATITUDE)
      )
    }

    mapa %>%
      addScaleBar(position = "bottomleft") %>%
      # Exportação em PNG (botão "Baixar mapa (PNG)", ver observeEvent abaixo).
      # O botão do próprio plugin fica escondido; a imagem sai no tamanho da
      # tela, com legenda, nota e escala, sem os botões de zoom. A imagem é
      # montada no navegador -- os tiles do Esri liberam CORS, o que permite.
      addEasyprint(options = easyprintOptions(
        hidden = TRUE,
        exportOnly = TRUE,
        sizeModes = list("CurrentSize"),
        hideControlContainer = FALSE,
        hideClasses = list("leaflet-control-zoom"),
        tileWait = 1500
      ))
  })

  observeEvent(input$exportar_mapa, {
    req(resultados$processado)
    modo <- if (identical(input$mapa_tipo_visualizacao, "densidade")) "densidade" else "pontos"
    leafletProxy("mapa_coordenadas") %>%
      easyprintMap(
        sizeModes = "CurrentSize",
        filename = paste0("mapa_", modo, "_", input$uf, "_", format(Sys.Date(), "%Y-%m-%d"))
      )
  })
  
  
  ############################################
  # 0. OBJETOS REATIVOS AUXILIARES
  ############################################
  
  resultados$dados_triagem   <- NULL
  resultados$selecoes_triagem <- NULL
  resultados$dados_finais   <- NULL
  resultados$ordem_triagem     <- NULL
  resultados$idx_triagem_atual <- 1
  
  
  ############################################
  # 1. CARREGAR DADOS PARA TRIAGEM
  ############################################
  
  observeEvent(input$carregar_triagem, {

    # BLOQUEIO
    if (input$uf == "DF") {

      showNotification(
        "Triagem indisponível para UF DF.",
        type = "warning",
        duration = 6
      )

      return(NULL)
    }

    if (input$metodo != "cnefe") {

      showNotification(
        "Triagem de endereços disponível apenas para o match probabilístico. Método usado: GecodeBr.",
        type = "warning",
        duration = 6
      )

      return(NULL)
    }

    req(resultados$processado)
    req(resultados$base_processada)
    req(resultados$base_triagem)
    
    limiar <- input$triagem_threshold
    
    if (input$triagem_scope == "baixa_similaridade") {
      
      ids_triagem <- resultados$base_processada %>%
        filter(is.na(similarity) | similarity < limiar) %>%
        pull(NU_NOTIFIC) %>%
        unique()
      
      dados_triagem <- resultados$base_triagem %>%
        filter(NU_NOTIFIC %in% ids_triagem)
      
    } else {
      dados_triagem <- resultados$base_triagem
    }
    
    # Base exibida (NUNCA será alterada depois)
    resultados$dados_triagem <- dados_triagem %>%
      arrange(NU_NOTIFIC, rank)
    
    # Inicializa seleção padrão (rank == 1)
    resultados$selecoes_triagem <- resultados$dados_triagem %>%
      filter(rank == 1) %>%
      select(NU_NOTIFIC, rank)
    
    showNotification(
      paste(
        "Triagem carregada com",
        length(unique(dados_triagem$NU_NOTIFIC)),
        "endereços."
      ),
      type = "message"
    )
    
    # Ordem fixa de navegação
    resultados$ordem_triagem <- resultados$dados_triagem %>%
      distinct(NU_NOTIFIC) %>%
      pull(NU_NOTIFIC)
    
    resultados$idx_triagem_atual <- 1
    
  })
  
  output$box_endereco_atual <- renderUI({

    if (input$uf == "DF") {
      return(
        wellPanel(
          h4("Triagem indisponível"),
          p("Triagem indisponível para UF DF.")
        )
      )
    }

    if (input$metodo != "cnefe") {
      return(
        wellPanel(
          h4("Triagem indisponível"),
          p("Triagem de endereços disponível apenas para o match probabilístico."),
          p(strong("Método utilizado: "), "GecodeBr")
        )
      )
    }

    req(resultados$dados_triagem,
        resultados$ordem_triagem,
        resultados$idx_triagem_atual)
    
    idx <- resultados$idx_triagem_atual
    req(idx <= length(resultados$ordem_triagem))
    
    nu <- resultados$ordem_triagem[idx]
    
    dado <- resultados$dados_triagem %>%
      filter(NU_NOTIFIC == nu) %>%
      slice(1)
    
    wellPanel(
      h4("Endereço em triagem"),
      strong("NU_NOTIFIC: "), dado$NU_NOTIFIC, br(),
      strong("Address informado:"), br(),
      dado$address
    )
  })
  
  ############################################
  # 2. TABELA DE TRIAGEM
  ############################################
  
  output$tabela_triagem <- renderDT({

    # BLOQUEIO VISUAL
    if (input$uf == "DF") {
      return(
        datatable(
          data.frame(
            Aviso = "Triagem indisponível para UF DF."
          ),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }

    if (input$metodo != "cnefe") {

      return(
        datatable(
          data.frame(
            Aviso = "Triagem disponível apenas para o método CNEFE (match probabilístico)."
          ),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }

    req(resultados$dados_triagem,
        resultados$ordem_triagem,
        resultados$idx_triagem_atual)
    
    idx <- resultados$idx_triagem_atual
    req(idx <= length(resultados$ordem_triagem))
    
    nu <- resultados$ordem_triagem[idx]
    
    dados <- resultados$dados_triagem %>%
      filter(NU_NOTIFIC == nu) %>%
      arrange(rank) %>%
      slice_head(n = 15)
    
    if (input$uf == "DF") {
      
      datatable(
        dados %>%
          select(
            ENDERECO_CNEFE,
            similarity,
            LATITUDE,
            LONGITUDE,
            NM_SUBDIST,
            rank
          ),
        selection = "single",
        options = list(
          paging = FALSE,
          ordering = FALSE
        ),
        class = "stripe hover cell-border"
      )
      
    } else {
      
      datatable(
        dados %>%
          select(
            ENDERECO_CNEFE,
            similarity,
            LATITUDE,
            LONGITUDE,
            NM_MUN,
            rank
          ),
        selection = "single",
        options = list(
          paging = FALSE,
          ordering = FALSE
        ),
        class = "stripe hover cell-border"
      )
    }
  })
  
  
  ############################################
  # 3. CAPTURAR CLIQUE (SEM RE-RENDER)
  ############################################
  
  observeEvent(input$tabela_triagem_rows_selected, {
    
    req(resultados$dados_triagem,
        resultados$ordem_triagem,
        resultados$idx_triagem_atual)
    
    idx <- resultados$idx_triagem_atual
    nu  <- resultados$ordem_triagem[idx]
    
    linha <- resultados$dados_triagem %>%
      filter(NU_NOTIFIC == nu) %>%
      arrange(rank) %>%
      slice(input$tabela_triagem_rows_selected)
    
    resultados$selecoes_triagem <- resultados$selecoes_triagem %>%
      filter(NU_NOTIFIC != nu)
    
    resultados$selecoes_triagem <- bind_rows(
      resultados$selecoes_triagem,
      linha %>% select(NU_NOTIFIC, rank)
    )
  })
  
  observeEvent(input$triagem_proximo, {
    req(resultados$ordem_triagem)
    
    if (resultados$idx_triagem_atual < length(resultados$ordem_triagem)) {
      resultados$idx_triagem_atual <- resultados$idx_triagem_atual + 1
    } else {
      showNotification("Último endereço da triagem.", type = "message")
    }
  })
  
  observeEvent(input$triagem_anterior, {
    if (resultados$idx_triagem_atual > 1) {
      resultados$idx_triagem_atual <- resultados$idx_triagem_atual - 1
    }
  })
  
  ############################################
  # 4. FINALIZAR TRIAGEM
  ############################################
  
  observeEvent(input$finalizar_triagem, {
    
    req(resultados$selecoes_triagem)
    req(resultados$base_processada)
    req(resultados$dados_triagem)
    
    selecionados <- resultados$dados_triagem %>%
      inner_join(resultados$selecoes_triagem,
                 by = c("NU_NOTIFIC", "rank"))
    
    resultados$dados_triagem$rank <- as.numeric(resultados$dados_triagem$rank)
    
    resultados$base_processada$rank <- as.numeric(resultados$base_processada$rank)
    
    if (input$uf == "DF") {
      
      selecionados <- selecionados %>%
        select(
          NU_NOTIFIC,
          ENDERECO_CNEFE,
          similarity,
          LATITUDE,
          LONGITUDE,
          NM_SUBDIST,
          rank
        )

      resultados$dados_finais <- resultados$base_processada %>%
        left_join(selecionados, by = "NU_NOTIFIC", suffix = c(".old", "")) %>%
        mutate(
          ENDERECO_CNEFE = coalesce(ENDERECO_CNEFE, ENDERECO_CNEFE.old),
          similarity     = coalesce(similarity, similarity.old),
          LATITUDE       = coalesce(LATITUDE, LATITUDE.old),
          LONGITUDE      = coalesce(LONGITUDE, LONGITUDE.old),
          NM_SUBDIST     = coalesce(NM_SUBDIST, NM_SUBDIST.old),
          rank           = coalesce(rank, rank.old)
        ) %>%
        select(-ends_with(".old"))
      
    } else {
      
      selecionados <- selecionados %>%
        select(
          NU_NOTIFIC,
          ENDERECO_CNEFE,
          similarity,
          LATITUDE,
          LONGITUDE,
          NM_MUN,
          rank
        )

      resultados$dados_finais <- resultados$base_processada %>%
        left_join(selecionados, by = "NU_NOTIFIC", suffix = c(".old", "")) %>%
        mutate(
          ENDERECO_CNEFE = coalesce(ENDERECO_CNEFE, ENDERECO_CNEFE.old),
          similarity     = coalesce(similarity, similarity.old),
          LATITUDE       = coalesce(LATITUDE, LATITUDE.old),
          LONGITUDE      = coalesce(LONGITUDE, LONGITUDE.old),
          NM_MUN         = coalesce(NM_MUN, NM_MUN.old),
          rank           = coalesce(rank, rank.old)
        ) %>%
        select(-ends_with(".old"))

      # Candidato escolhido abaixo do limite do processamento (possível nas
      # fases 5, que guardam as 15 melhores candidatas mesmo abaixo dele)
      # deixa de contar como georreferenciado. Só rebaixa: a triagem não
      # atualiza FASE nem PRECISAO_POSICIONAL, e o endereço vazio não é
      # verificável aqui (as colunas de endereço podem já ter sido descartadas).
      limiar <- resultados$metricas$limiar_similaridade
      if (!is.null(limiar) && "GEORREFERENCIADO" %in% names(resultados$dados_finais)) {
        finais   <- resultados$dados_finais
        revistos <- finais$NU_NOTIFIC %in% selecionados$NU_NOTIFIC
        sim      <- suppressWarnings(as.numeric(finais$similarity))
        finais$GEORREFERENCIADO[revistos & (is.na(sim) | sim < limiar)] <- 0L
        resultados$dados_finais <- finais
      }
    }
    
    showNotification(
      "Triagem finalizada com sucesso. Resultado pronto para download.",
      type = "message"
    )
  })
  
  
  ############################################
  # 5. RESETAR TRIAGEM
  ############################################
  
  observeEvent(input$reset_triagem, {
    
    req(resultados$dados_triagem)
    
    resultados$selecoes_triagem <- resultados$dados_triagem %>%
      filter(rank == 1) %>%
      select(NU_NOTIFIC, rank)
    
    showNotification(
      "Seleções resetadas para a opção de maior similaridade.",
      type = "message"
    )
  })
  
  
  ############################################
  # 6. DOWNLOAD
  ############################################
  
  output$download_triagem <- downloadHandler(
    filename = function() {
      paste0("resultado_triagem_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(resultados$dados_finais)
      readr::write_csv2(resultados$dados_finais, file)
    }
  )
  
  # Resumo do processamento (SIMPLIFICADO)
  output$resumo_processamento <- renderPrint({
    if(input$metodo == "cnefe"){
      req(resultados$processado)
      
      cat("=== RESUMO DO PROCESSAMENTO ===\n\n")
      cat("Método utilizado: Match Probabilístico\n")
      
      escopo_texto <- ifelse(input$escopo == "uf", 
                             paste("UF:", input$uf),
                             paste("Município:", input$codigo_municipio, "- UF:", input$uf))
      cat("Escopo processado:", escopo_texto, "\n")
      
      cat("Tempo de processamento:", format(resultados$metricas$tempo_de_processamento, digits = 2), "\n")
      cat("Total de registros processados:", nrow(resultados$base_processada), "\n")

      if (input$uf == "DF") {
        # Pipeline DF: fases 12, 13, 15 e 18 são as únicas que usam
        # similaridade (Jaccard de tokens nas 12/13/15, Jaro na 18) —
        # as demais são match exato de bigramas/IDs/distância numérica.
        fases_similaridade <- c("RUA_AVENIDA", "NOMINAL_BIGRAMA", "RUA_IGNORADO", "JARO_TOKENS")
        n_similaridade <- resultados$metricas$contagem_por_fase %>%
          filter(FASE_MATCH %in% fases_similaridade) %>%
          pull(N) %>%
          sum()
        cat("Casos geocodificados por similaridade:", n_similaridade, "\n")
      } else {
        cat("Casos geocodificados por similaridade:", sum(!is.na(resultados$base_processada$similarity) &
                                                            resultados$base_processada$similarity != 1), "\n")
      }
    } else{
      cat("=== RESUMO DO PROCESSAMENTO ===\n\n")
      cat("Método utilizado: Match Probabilístico\n")
      
      escopo_texto <- ifelse(input$escopo == "uf", 
                             paste("UF:", input$uf),
                             paste("Município:", input$codigo_municipio, "- UF:", input$uf))
      cat("Escopo processado:", escopo_texto, "\n")
      
      cat("Tempo de processamento:", 
          format(resultados$metricas$tempo_de_processamento, digits = 2), "\n")
      cat("Total de registros processados:", 
          nrow(resultados$base_processada), "\n\n")
    }
  })
  
  # Métricas detalhadas para CNEFE - TODOS OS BAIRROS
  output$metricas_RA <- renderTable({
    uf_selecionada <- input$uf
    if(uf_selecionada == "DF"){
      req(resultados$processado)

      resultados$metricas$contagem_por_ra %>%
        select(RA = NM_SUBDIST, Quantidade = N, Percentual = PCT)
    }else{
      req(resultados$processado)

      # Mostrar todos as RAs
      resultados$metricas$contagem_casos_MUN_RA %>%
        select(RA = NM_MUN_CNEFE, Quantidade = n, Percentual = proporcao)
    }
  }, digits = 3)

  # DF: cobertura do georreferenciamento pela coluna GEORREFERENCIADO
  # (endereço / centroide ou endereço vazio) -- ver resumo_georreferenciado()
  output$metricas_cobertura <- renderTable({
    req(resultados$processado)
    req(input$uf == "DF")

    resultados$metricas$cobertura %>%
      select(Status = DESCRICAO, Quantidade = N, Percentual = PCT)
  }, digits = 2)

  # Distribuição de casos por fase de match -- DF e demais UFs, calculada no
  # observer de processamento (resultados$metricas$distribuicao_fases)
  output$metricas_fases <- renderTable({
    req(resultados$processado)
    req(resultados$metricas$distribuicao_fases)

    resultados$metricas$distribuicao_fases %>%
      select(Fase = FASE, Quantidade = N, `Percentual (%)` = PCT)
  }, digits = 2)

  # Métricas detalhadas para CNEFE - TODOS OS TIPOS DE QUALIDADE
  output$metricas_qualidade <- renderTable({
    req(resultados$processado)
    if (input$uf == "DF") return(NULL)  # conceito não se aplica ao pipeline DF

    # Mostrar todos os tipos de qualidade de endereço
    resultados$metricas$qualidade_enderecos %>%
      select(`Qualidade` = QUALIDADE_ENDERECO, Quantidade = n, Percentual = proporcao)
  })

  # Métricas detalhadas para CNEFE - TODOS OS TIPOS DE QUALIDADE
  output$metricas_tipo_endereco <- renderTable({
    req(resultados$processado)
    if (input$uf == "DF") return(NULL)  # conceito não se aplica ao pipeline DF

    # Mostrar todos os tipos de endereço
    resultados$metricas$qtd_tipo_endereco %>%
      select(`Tipo Endereço` = TIPO_ENDERECO, Quantidade = n, Percentual = proporcao)
  })

  # ------------------------------------------------------------------
  # ABA GRÁFICOS
  # Os dados vêm de preparar_graficos(), chamada no observer de
  # processamento; os gráficos, de graficos_funcoes.R. O GeocodeBr não passa
  # por ali (não tem fases) e fica só com uma nota.
  # ------------------------------------------------------------------
  dados_graficos <- reactive(resultados$metricas$graficos)

  output$graficos_conteudo <- renderUI({
    req(resultados$processado)
    g <- dados_graficos()
    if (is.null(g)) {
      return(p(class = "grafico-descricao",
               "Os gráficos desta aba descrevem o match com o CNEFE. O GeocodeBr classifica a ",
               "precisão de outra forma, na coluna precisao dos dados processados."))
    }
    k <- g$kpi

    tile <- function(rotulo, valor, detalhe, cor = NULL) {
      div(class = "kpi-tile",
          div(class = "kpi-rotulo",
              if (!is.null(cor)) span(class = "kpi-marca", style = paste0("background-color: ", cor, ";")),
              rotulo),
          div(class = "kpi-valor", valor),
          div(class = "kpi-detalhe", detalhe))
    }
    secao <- function(titulo, descricao, grafico, altura, tabela, extra = NULL) {
      div(class = "grafico-secao",
          h4(titulo),
          p(class = "grafico-descricao", descricao),
          extra,
          plotOutput(grafico, height = paste0(round(altura), "px")),
          tags$details(tags$summary("Ver dados"), tableOutput(tabela)))
    }

    tagList(
      div(class = "kpi-row",
          tile("Casos processados", fmt_n(k$n), "registros na base"),
          tile("Posição por endereço", fmt_pct(100 * k$n_endereco / k$n),
               paste(fmt_n(k$n_endereco), "casos"), COR_ENDERECO),
          tile("Centroide (fallback)", fmt_pct(100 * k$n_centroide / k$n),
               paste(fmt_n(k$n_centroide), "casos, sem setor censitário"), COR_CENTROIDE),
          if (k$n_sem > 0) tile("Sem coordenada", fmt_pct(100 * k$n_sem / k$n),
                                paste(fmt_n(k$n_sem), "casos"), COR_SEM_COORD),
          tile("Setores censitários", fmt_n(k$n_setores),
               paste0("com casos; ", fmt_pct(100 * k$n_com_setor / k$n), " dos casos têm setor"))
      ),

      secao(
        "Precisão posicional",
        if (g$eh_df) {
          paste("Onde cada caso foi posicionado, a partir da fase de match que o resolveu.",
                "Endereço aproximado: fases cujo rótulo termina em APROX (quadra ou conjunto",
                "aproximados). Local fixo: complexo penitenciário. Centroide: nenhuma fase casou,",
                "e o caso recebeu o centro do polígono da RA ou do DF, sem setor censitário.")
        } else {
          paste("Onde cada caso foi posicionado, a partir da fase de match que o resolveu.",
                "Número aproximado: o número informado não existe no CNEFE e o caso foi para o",
                "mais próximo. Início do logradouro: o caso não tinha número. Centroide: nenhuma",
                "fase casou, e o caso recebeu o centro do polígono do município ou da UF, sem",
                "setor censitário.")
        },
        "graf_precisao", 70 + 36 * nrow(g$precisao), "tab_precisao"
      ),

      if (nrow(g$cascata) > 0) secao(
        "Cobertura ao longo das fases",
        paste("Quanto da base já tinha coordenada depois de cada etapa, na ordem em que o",
              "pipeline executa. Onde as barras param de crescer, as etapas seguintes pouco",
              "acrescentam.",
              if (g$eh_df) "QUADRA_APROX é gravado pelas fases 5, 7 e 14 e aparece na posição da fase 5." else ""),
        "graf_cascata", 90 + 24 * nrow(g$cascata), "tab_cascata"
      ),

      secao(
        if (g$eh_df) "Georreferenciamento por Região Administrativa informada"
        else "Georreferenciamento por município informado",
        paste("Composição dos casos de cada", if (g$eh_df) "RA" else "município",
              "informado na notificação. Onde a parte de centroide é alta, o endereço",
              "informado costuma estar incompleto ou fora do padrão do CNEFE."),
        "graf_unidade", 90 + 30 * length(unique(g$por_unidade$UNIDADE)), "tab_unidade"
      ),

      if (!is.null(g$distancia)) secao(
        "Distância do número nos matches aproximados",
        paste("Nas fases 2.1, 3.2 e 4.2 o logradouro casou, mas o número informado não existe",
              "no CNEFE, e o caso foi para o número mais próximo. Quanto maior a diferença,",
              "maior o deslocamento ao longo da rua."),
        "graf_distancia", 320, "tab_distancia"
      ),

      if (!is.null(g$divergencia) && g$divergencia$n_avaliados > 0) {
        dv <- g$divergencia
        bloco <- div(class = "kpi-row",
                     tile("Município divergente", fmt_pct(100 * dv$n_divergentes / dv$n_avaliados),
                          paste(fmt_n(dv$n_divergentes), "de", fmt_n(dv$n_avaliados),
                                "casos com match e município informado válido")))
        descricao <- paste(
          "Casos em que o endereço encontrado no CNEFE fica em outro município que não o",
          "informado. O pipeline aceita isso quando o endereço só existe em um município",
          "(fase A.1.2) e nas fases sem filtro de município (B e G). Uma taxa alta sugere",
          "erro no campo de município da notificação.")
        if (is.null(dv$por_fase)) {
          div(class = "grafico-secao", h4("Município divergente"),
              p(class = "grafico-descricao", descricao), bloco)
        } else {
          secao("Município divergente", descricao, "graf_divergencia",
                80 + 30 * nrow(dv$por_fase), "tab_divergencia", extra = bloco)
        }
      },

      if (!is.null(g$setores)) secao(
        "Casos por setor censitário",
        paste("Quantos setores receberam 1, 2, 3 a 5… casos. Casos de centroide ficam de fora,",
              "porque não têm setor."),
        "graf_setores", 320, "tab_setores"
      ),

      if (!is.null(g$similaridade)) secao(
        "Similaridade nas fases probabilísticas",
        paste("Similaridade Jaro-Winkler dos casos resolvidos pelas fases 4 e 5, as únicas em que",
              "ela decide o match. A linha marca o limite usado neste processamento. Muitos casos",
              "logo acima dela são matches no limite da aceitação, bons candidatos à revisão na",
              "aba Triagem."),
        "graf_similaridade", 320, "tab_similaridade"
      )
    )
  })

  output$graf_precisao     <- renderPlot({ req(dados_graficos()); grafico_precisao(dados_graficos()) }, res = 96)
  output$graf_cascata      <- renderPlot({ req(nrow(dados_graficos()$cascata) > 0); grafico_cascata(dados_graficos()) }, res = 96)
  output$graf_unidade      <- renderPlot({ req(dados_graficos()); grafico_por_unidade(dados_graficos()) }, res = 96)
  output$graf_distancia    <- renderPlot({ req(dados_graficos()$distancia); grafico_distancia(dados_graficos()) }, res = 96)
  output$graf_divergencia  <- renderPlot({ req(dados_graficos()$divergencia$por_fase); grafico_divergencia(dados_graficos()) }, res = 96)
  output$graf_setores      <- renderPlot({ req(dados_graficos()$setores); grafico_setores(dados_graficos()) }, res = 96)
  output$graf_similaridade <- renderPlot({ req(dados_graficos()$similaridade); grafico_similaridade(dados_graficos()) }, res = 96)

  # Tabelas "Ver dados": os mesmos números dos gráficos, legíveis sem a cor.
  output$tab_precisao <- renderTable({
    d <- req(dados_graficos())$precisao
    data.frame(`Precisão` = as.character(d$PRECISAO), Casos = fmt_n(d$N), `%` = fmt_pct(d$PCT),
               check.names = FALSE)
  })
  output$tab_cascata <- renderTable({
    d <- req(dados_graficos())$cascata
    data.frame(Etapa = d$ROTULO, Casos = fmt_n(d$N), `Acrescenta` = fmt_pct(d$INCREMENTO_PCT),
               `Acumulado` = fmt_pct(d$ACUM_PCT), check.names = FALSE)
  })
  output$tab_unidade <- renderTable({
    d <- req(dados_graficos())$por_unidade
    w <- as.data.frame.matrix(stats::xtabs(N ~ UNIDADE + GRUPO, data = d))
    w <- w[order(-rowSums(w)), , drop = FALSE]
    data.frame(Unidade = rownames(w), Total = fmt_n(rowSums(w)),
               lapply(w, fmt_n), check.names = FALSE)
  })
  output$tab_distancia <- renderTable({
    d <- req(dados_graficos()$distancia)$tabela
    data.frame(`Diferença de número` = d$FAIXA, Casos = fmt_n(d$N), check.names = FALSE)
  })
  output$tab_divergencia <- renderTable({
    d <- req(dados_graficos()$divergencia$por_fase)
    data.frame(Fase = d$FASE, Casos = fmt_n(d$N))
  })
  output$tab_setores <- renderTable({
    d <- req(dados_graficos()$setores)$tabela
    data.frame(`Casos no setor` = d$CASOS, Setores = fmt_n(d$N_SETORES), check.names = FALSE)
  })
  output$tab_similaridade <- renderTable({
    s <- req(dados_graficos()$similaridade)$valores
    t <- table(cut(s, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE))
    t <- t[t > 0]
    data.frame(Similaridade = gsub("\\.", ",", names(t)), Casos = fmt_n(as.integer(t)))
  })

  # Download dos dados processados
  output$download_dados <- downloadHandler(
    filename = function() {
      paste0("dados_processados_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv2(
        resultados$base_processada,
        file
      )
    }
  )
  
  # Download das métricas
  output$download_metricas <- downloadHandler(
    filename = function() {
      paste0("metricas_processamento_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metricas_df <- data.frame(
        Metrica = names(resultados$metricas),
        Valor   = as.character(unlist(resultados$metricas)),
        stringsAsFactors = FALSE
      )
      
      write_csv2(
        metricas_df,
        file
      )
    }
  )
  
  # Limpar tudo
  observeEvent(input$limpar, {
    resultados$base_processada <- NULL
    resultados$metricas <- NULL
    resultados$processado = FALSE
    resultados$arquivos_carregados = FALSE
    resultados$dados_triagem <- NULL
    resultados$selecoes_triagem <- list()
    resultados$dados_finais <- NULL
    
    # Limpar inputs
    updateTextInput(session, "codigo_municipio", value = "")
    showNotification("Todos os dados foram limpos.", type = "message")
  })
}

# Executar aplicação
shinyApp(ui = ui, server = server)