# Carregando as bibliotecas necessárias
library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(sf)
library(DT)
library(plotly)
library(leafgl)
library(pdftools)
library(openai)
library(shinycssloaders)
library(wordcloud2)
library(ggplot2)
library(tidytext)
library(stopwords)
library(shinyjs)
library(spdep)
library(spatialreg)

# Limitar o tamanho máximo do arquivo de upload para 10 MB
options(shiny.maxRequestSize = 10 * 1024^2)  # 10 MB

# Acessar a chave de API da OpenAI
openai_api_key <- Sys.getenv("OPENAI_API_KEY")

nome_mapeamento <- c(
  "Valor.per.capita.adicionado.bruto.da.Agropecuária..a.preços.correntes..R..1.000." =
    "Valor per capita adicionado bruto da Agropecuária, a preços correntes (R$ 1.000)",
  "Impostos..líquidos.de.subsídios..per.capita.sobre.produtos..a.preços.correntes..R..1.000." =
  "Impostos, líquidos de subsídios, per capita sobre produtos, a preços correntes (R$ 1.000)",
  "Total.de.domicílios.particulares.permantes.ocupados.que.possui.ligação.à.rede.geral.de.distribuição.de.água..mas.utiliza.principalmente.outra.forma.per.capita"=
  "Total de domicílios particulares permantes ocupados que possui ligação à rede geral de distribuição de água, mas utiliza principalmente outra forma per capita",
  "Valor.per.capita.adicionado.bruto.da.Administração..defesa..educação.e.saúde.públicas.e.seguridade.social..a.preços.correntes..R..1.000." =
  "Valor per capita adicionado bruto da Administração, defesa, educação e saúde públicas e seguridade social, a preços correntes (R$ 1.000)")

# Definindo a Interface do Usuário (UI)
ui <- tagList(
  # 1) Primeiro, injetamos o CSS no head
  tags$head(
    tags$style(HTML("
      /* Permitir que o texto dentro do selectInput quebre linha quando for muito longo */
      .selectize-input, .selectize-dropdown {
        white-space: normal !important;
      }
    "))
  ), 
  
  page_navbar(
  title = "LongevMap",
  bg = "#2D89C8",
  inverse = TRUE,
  
  #------------#------------#------------#------------------------------
  #-------------- PAINEL 1 - Mapa Interativo LADO INTERFACE -------------
  #------------#------------#------------#------------------------------
  
  nav_panel(
    title = "Mapa Interativo do Brasil",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("data_type", "Selecione a Base de Dados:",
                    choices = c("Escolha uma opção...", "Dados Brutos", "Dados Per Capita"),
                    selected = "Escolha uma opção..."),
        
        uiOutput("variavel_mapa_ui"),  # A lista de variáveis será gerada dinamicamente
        
        selectInput("estado_mapa", "Selecione o Estado:",
                    choices = c("Todos os estados", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES",
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ",
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = "SP"),
        
        checkboxInput("show_legend", "Exibir Legenda", value = TRUE)
      ),
      
      mainPanel(
        width = 9,  # Aumentando a largura do mainPanel para ocupar o restante do espaço
        leafletOutput("mapa_interativo", height = "100vh")
      )
    )
  ),
  
  #------------#------------#------------#------------------------------
  #-------------- PAINEL 2 - DATA EXPLORER LADO INTERFACE ---------------
  #------------#------------#------------#------------------------------
  
  nav_panel(
    title = "Tabela de Dados",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("data_type_DE",
                    "Selecione a Base de Dados para a tabela ao lado:",
                    choices = c("Escolha uma opção...", "Dados_Brutos", "Dados_Per_Capita"),
                    selected = "Escolha uma opção..."),
        
        uiOutput("variavel_mapa_ui_DE"),
        
        selectInput("estado_mapa_DE", "Selecione o Estado:",
                    choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES",
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ",
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = NULL,
                    multiple = TRUE),
        conditionalPanel(condition = "input.estado_mapa_DE != ''",
                         selectizeInput("cidades_DE",
                                        label = "Selecione o município",
                                        choices = NULL,
                                        multiple = TRUE))
      ),
      mainPanel(
        width = 9,
        
        # Primeiro card: DataTable_DE_UI
        bslib::card(
          full_screen = TRUE,
          uiOutput("DataTable_DE_UI")
        ),
        
        # Segundo card: plot_DE
        bslib::card(
          full_screen = TRUE,
          plotly::plotlyOutput("plot_DE")
        )
      )
    )
  ),
  
  #------------#------------#------------#------------------------------
  #------------ PAINEL 3 - RAIO X MUNICIPAL  LADO INTERFACE -------------
  #------------#------------#------------#------------------------------
  
  nav_panel(
    title = "Raio-X Municipal",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("data_type_RM",
                    "Selecione a Base de Dados para a tabela ao lado:",
                    choices = c("Escolha uma opção...", "Dados Brutos", "Dados Per Capita"),
                    selected = "Escolha uma opção..."),
        
        uiOutput("variavel_mapa_ui_RM"),
        
        selectInput("estado_mapa_RM", "Selecione o Estado:",
                    choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES",
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ",
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = NULL,
                    multiple = TRUE),
        conditionalPanel(condition = "input.estado_mapa_RM != ''",
                         selectizeInput("cidades_RM",
                                        label = "Selecione o município",
                                        choices = NULL,
                                        multiple = TRUE))
      ),
      
      mainPanel(
        width = 9,
        
        # Card para o gráfico
        bslib::card(
          full_screen = TRUE,
          plotly::plotlyOutput("scatterpolar_rm")
        ),
        
        # Card para a tabela de informações de valores originais, mínimo e máximo
        bslib::card(
          full_screen = TRUE,
          DT::dataTableOutput("informacao_tabela_RM")
        )
      )
    )
  ),
  
  #------------#------------#------------#------------------------------
  #------------ PAINEL 4 - Resíduos Modelos Regressão LADO INTERFACE -------------
  #------------#------------#------------#------------------------------
  
  nav_panel(
    title = "Resíduos",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("var_dependente", "Selecione a Variável Dependente:",
                    choices = c("Escolha uma opção...",
                                "Total de Idosos per capita (TIpc)",
                                "Mediana de Idade dos Idosos (MII)",
                                "Expectativa de Vida ao Nascer (EdVN)"),
                    selected = "Escolha uma opção..."),
        
        selectInput("modelo", "Selecione o Modelo:",
                    choices = c("Regressão Linear", "Regressão SAR", "GWR Básico", "GWR Multiscale")),
        
        selectInput("estado_mapa_modelos", "Selecione o Estado:",
                    choices = c("Todos os estados", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES",
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ",
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = "SP"),
        
        checkboxInput("exibir_legenda_modelos", "Exibir Legenda", value = TRUE)
        
      ),
      
      mainPanel(
        width = 9,
        
        # Card para o título do mapa e o mapa interativo
        bslib::card(
          full_screen = TRUE,
          # Título dinâmico do mapa
          uiOutput("titulo_mapa_residuos"),
          leafletOutput("mapa_residuos", height = "80vh")
        ),
        
        # Card para o título do sumário e o sumário do modelo
        bslib::card(
          full_screen = TRUE,
          # Título dinâmico para o sumário do modelo
          uiOutput("titulo_summary_modelos"),
          verbatimTextOutput("summary_output_modelos")
        )
      )
      
      
    )
  ),
  
  #------------#------------#------------#------------------------------------
  #------------ NOVO PAINEL 5 - Modelos Locais  LADO INTERFACE ---------------
  #------------#------------#------------#------------------------------------
  
  nav_panel(
    title = "Modelos Locais",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("var_dependente_locais", "Selecione a Variável Dependente:",
                    choices = c("Escolha uma opção...",
                                "Total de Idosos per capita (TIpc)",
                                "Mediana de Idade dos Idosos (MII)",
                                "Expectativa de Vida ao Nascer (EdVN)"),
                    selected = "Escolha uma opção..."),
        selectInput("tipo_modelo_locais", "Selecione o tipo de Modelo Local:",
                    choices = c("GWR Multiscale", "GWR Básico")),
        uiOutput("coeficiente_locais_ui"),
        selectInput("estado_locais", "Selecione o Estado:",
                    choices = c("Todos os estados", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES",
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ",
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = "SP"),
        # NOVO: Seletor de município – independente do estado selecionado:
        uiOutput("municipio_locais_ui"),
        checkboxInput("exibir_legenda_locais", "Exibir Legenda", value = TRUE),
        # NOVOS CHECKBOXES:
        checkboxInput("exibir_significancia_locais", 
                      "Destacar municípios COM significância estatística (|t| > 1.96) nos coeficientes", 
                      value = FALSE),
        checkboxInput("exibir_nsignificancia_locais", 
                      "Destacar municípios SEM significância estatística (|t| ≤ 1.96) nos coeficientes", 
                      value = FALSE)
      ),
      mainPanel(
        width = 9,
        bslib::card(
          full_screen = TRUE,
          uiOutput("titulo_mapa_locais"),
          leafletOutput("mapa_locais", height = "80vh")
        ),
        # Espaço entre os cards:
        br(), br(),
        # Card da tabela:
        bslib::card(
          full_screen = TRUE,
          uiOutput("titulo_detalhes_municipio"),
          DT::dataTableOutput("detalhes_municipio")
        )
      )
    )
  ),
  
  
  #------------#------------#------------#------------------------------
  #------------ PAINEL 6 - Análise de Documento PDF LADO INTERFACE -------
  #------------#------------#------------#------------------------------
  
  nav_panel(
    title = "Análise de Documentos dos CMIs",
    fluidPage(
      useShinyjs(),  # Inicializar shinyjs no UI
      # File input na tela principal
      fileInput("pdfFile", h3("Faça upload do arquivo PDF"), accept = ".pdf"),
      # Resumo para Pessoas Idosas com botão de download
      h3("Resumo o documento encaminhado do ponto de vista de uma pessoa idosa"),
      shinycssloaders::withSpinner(verbatimTextOutput("elderly_summary")),
      
      # Botão de Download (inicialmente desabilitado)
      shinyjs::disabled(downloadButton("download_elderly_summary", "Download Resumo para Pessoas Idosas")),
      br(), br(),
      
      # Botão para gerar nuvem de palavras e gráfico (inicialmente desabilitado)
      shinyjs::disabled(actionButton("generate_plots", "Gerar Nuvem de Palavras e Gráfico", class = "btn-primary")),
      br(), br(),
      
      # Nuvem de Palavras
      h3("Nuvem de Palavras"),
      shinycssloaders::withSpinner(wordcloud2::wordcloud2Output("wordcloud")),
      br(), br(),
      
      # Gráfico de Recorrência por Palavras
      h3("Gráfico de Recorrência por Palavras"),
      shinycssloaders::withSpinner(plotOutput("word_freq_plot")),
      br(), br()
    )
  ),
  
  #------------#------------#------------#------------------------------
  #------------ PAINEL 7 - Municípios Aptos ----------------------------
  #------------#------------#------------#------------------------------
  
  
  nav_panel(
    title = "Municípios aptos",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput(
          "estado_mapa_aptos",
          "Selecione o Estado:",
          choices = c(
            "Escolha uma UF..." = "",
            "Todos os estados" = "all",
            "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES",
            "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
            "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"
          ),
          selected = "",    # Nenhum selecionado
          selectize = TRUE
        ),
        checkboxInput("show_legend_aptos", "Exibir Legenda", value = TRUE)
      ),
      mainPanel(
        width = 9,
        leafletOutput("mapa_aptos", height = "100vh")
      )
    )
  ),
  
  
  # Espaçador e menu de links
  nav_spacer(),
  
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  )
 )
)

#------------#------------#------------#------------------------------
#--------------------- INÍCIO DO LADO SERVIDOR -----------------------
#------------#------------#------------#------------------------------

server <- function(input, output, session) {
  
  # Inicializar shinyjs
  shinyjs::useShinyjs()
  
  #------------#------------#------------#------------------------------
  #------------- CARREGANDO AS BASES DE DADOS --------------------------
  #------------#------------#------------#------------------------------
  
  # Carregar as bases de dados
  dados_brutos <- readRDS("data/Mapa_Final3.rds")
  dados_per_capita <- readRDS("data/Mapa_Final_PC.rds")
  
  # ---- Carregar a base dos Municípios Aptos ----
  municipios_aptos <- readRDS("data/Municipios_Aptos_Mapa.rds")
  estados_aptos <- readRDS("data/Estados_Aptos.rds")
  
  # Carregar os modelos salvos
  model_linear_TIpc <- readRDS("models/model_linear_TIpc.rds")
  model_SAR_TIpc <- readRDS("models/model_SAR_TIpc.rds")
  model_GWR_TIpc <- readRDS("models/model_GWR_TIpc.rds")
  model_GWR_Basic_TIpc <- readRDS("models/model_GWR_Basic_TIpc.rds")
  
  
  model_linear_MII <- readRDS("models/model_linear_MII.rds")
  model_SAR_MII <- readRDS("models/model_SAR_MII.rds")
  model_GWR_MII <- readRDS("models/model_GWR_MII.rds")
  model_GWR_Basic_MII <- readRDS("models/model_GWR_Basic_MII.rds")
  
  # Carregar os modelos da Expectativa de Vida ao Nascer (EdVN)
  model_linear_EdVN <- readRDS("models/model_linear_EDV.rds")
  model_SAR_EdVN <- readRDS("models/model_SAR_EDV.rds")
  model_GWR_EdVN <- readRDS("models/model_GWR_MEDV.rds")
  model_GWR_Basic_EDVN<- readRDS("models/model_GWR_Basic_EDV.rds")
  
  # Carregar o objeto sf para o mapa do modelo GWR Multiscale
  sf_objeto_GWR <- readRDS("data/sf_objeto_Dados_Painel_4.rds")
  
  # Organizar em uma lista para facilitar o acesso
  models <- list(
    "TIpc" = list(
      "Regressão Linear" = model_linear_TIpc,
      "Regressão SAR" = model_SAR_TIpc,
      "GWR Básico" = model_GWR_Basic_TIpc,
      "GWR Multiscale" = model_GWR_TIpc
    ),
    "MII" = list(
      "Regressão Linear" = model_linear_MII,
      "Regressão SAR" = model_SAR_MII,
      "GWR Básico" = model_GWR_Basic_MII,
      "GWR Multiscale" = model_GWR_MII
    ),
    "EdVN" = list(
      "Regressão Linear" = model_linear_EdVN,
      "Regressão SAR" = model_SAR_EdVN,
      "GWR Básico" = model_GWR_Basic_EDVN,
      "GWR Multiscale" = model_GWR_EdVN
    )
  )
  
  # Lista das bases para visualizar os coeficientes locais:
  sf_locais <- list(
    "TIpc" = list(
      "GWR Multiscale" = readRDS("data/sf_Base_Final_Coeficientes_GWR_Multi_TIpc.rds"),
      "GWR Básico"     = readRDS("data/sf_Base_Final_Coeficientes_GWR_Basic_TIpc.rds")
    ),
    "MII" = list(
      "GWR Multiscale" = readRDS("data/sf_Base_Final_Coeficientes_GWR_Multi_MII.rds"),
      "GWR Básico"     = readRDS("data/sf_Base_Final_Coeficientes_GWR_Basic_MII.rds")
    ),
    "EdVN" = list(
      "GWR Multiscale" = readRDS("data/sf_Base_Final_Coeficientes_GWR_Multi_EdVN.rds"),
      "GWR Básico"     = readRDS("data/sf_Base_Final_Coeficientes_GWR_Basic_EdVN.rds")
    )
  )
  
  observeEvent(input$exibir_significancia_locais, {
    if (isTRUE(input$exibir_significancia_locais)) {
      updateCheckboxInput(session, "exibir_nsignificancia_locais", value = FALSE)
    }
  })
  
  observeEvent(input$exibir_nsignificancia_locais, {
    if (isTRUE(input$exibir_nsignificancia_locais)) {
      updateCheckboxInput(session, "exibir_significancia_locais", value = FALSE)
    }
  })
  
  
  #------------#------------#------------#------------------------------
  #-------------- PAINEL 1 - Mapa Interativo LADO SERVIDOR --------------
  #------------#------------#------------#------------------------------
  
  # Observador para atualizar as variáveis de acordo com a base selecionada
  observe({
    req(input$data_type != "Escolha uma opção...")  # Garante que uma base válida foi selecionada
    
    dados <- if (input$data_type == "Dados Brutos") dados_brutos else dados_per_capita
    variaveis <- names(dados)[sapply(dados, is.numeric)]
    variaveis <- variaveis[!variaveis %in% c("Código UF", "Código Região")]
    variaveis <- sort(variaveis)
    updateSelectInput(session, "variavel_mapa", choices = variaveis, selected = variaveis[1])
  })
  
  # Interface UI para o seletor de variáveis
  output$variavel_mapa_ui <- renderUI({
    selectInput("variavel_mapa", "Selecione a Variável para o Mapa:", choices = NULL)
  })
  
  # Renderiza o mapa base apenas uma vez
  output$mapa_interativo <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -55, lat = -14, zoom = 4)  # Centraliza o mapa no Brasil
  })
  
  # Observa mudanças nos inputs e atualiza o mapa com leafletProxy
  observeEvent({
    input$data_type
    input$variavel_mapa
    input$estado_mapa
    input$show_legend
  }, {
    if (input$data_type == "Escolha uma opção...") {
      leafletProxy("mapa_interativo") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = "<div style='font-size: 16px; text-align: center;'><strong>Por favor, selecione uma base de dados para visualizar o mapa.</strong></div>",
          position = "topright"
        )
    } else {
      req(input$variavel_mapa)
      
      # Seleciona os dados de acordo com a base escolhida
      dados <- if (input$data_type == "Dados Brutos") dados_brutos else dados_per_capita
      
      # Filtra os dados se um estado específico foi selecionado
      if (input$estado_mapa != "Todos os estados") {
        dados <- dados %>% filter(Sigla_UF == input$estado_mapa)
      }
      
      valores <- dados[[input$variavel_mapa]]
      pal <- colorNumeric(palette = "YlOrRd", domain = valores, na.color = "transparent")
      
      leafletProxy("mapa_interativo", data = dados) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor = ~pal(valores),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          popup = ~paste(
            "<strong>Município:</strong>", `Nome Município`, "<br>",
            "<strong>Código Município:</strong>", `Código Município`, "<br>",
            "<strong>Estado:</strong>", Sigla_UF, "<br>",
            "<strong>", input$variavel_mapa, ":</strong>", valores
          )
        ) %>%
        {if (input$show_legend) addLegend(., pal = pal, values = valores,
                                          title = input$variavel_mapa, position = "bottomright") else .}
      
      # Reposicionar o mapa para o estado selecionado
      if (input$estado_mapa != "Todos os estados") {
        # Filtra os dados para o estado selecionado
        estado_selecionado <- dados %>% filter(Sigla_UF == input$estado_mapa)
        
        # Verifica se existem dados após o filtro
        if (nrow(estado_selecionado) > 0) {
          # Transforma para WGS84
          estado_selecionado <- st_transform(estado_selecionado, crs = 4326)
          
          # Calcula o bounding box
          bbox <- st_bbox(estado_selecionado)
          
          # Imprime o bbox para depuração
          print(bbox)
          
          # Reposiciona o mapa usando fitBounds
          leafletProxy("mapa_interativo") %>%
            fitBounds(lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
                      lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"]))
        } else {
          # Se não houver dados, centraliza o mapa no Brasil
          leafletProxy("mapa_interativo") %>%
            setView(lng = -55, lat = -14, zoom = 4)
        }
      } else {
        # Se "Todos os estados" for selecionado, ajustar o mapa para mostrar o Brasil inteiro
        leafletProxy("mapa_interativo") %>%
          setView(lng = -55, lat = -14, zoom = 4)
      }
    }
  })
  
  #------------#------------#------------#--------------------------------
  #-------------- PAINEL 2 - DATA EXPLORER LADO SERVIDOR -----------------
  #------------#------------#------------#--------------------------------
  
  # Reactive para gerar a base de dados que será utilizada tanto para a Interface
  df_DE <- reactive({
    # Retorne NULL se a escolha for o placeholder
    if (input$data_type_DE == "Escolha uma opção...") return(NULL)
    
    # Filtrando a tabela a partir dos dados selecionados na interface
    if (input$data_type_DE == "Dados_Brutos") {
      st_drop_geometry(dados_brutos)
    } else {
      st_drop_geometry(dados_per_capita)
    }
  })
  
  # Verificação ao selecionar o estado sem escolher a base
  observeEvent(input$estado_mapa_DE, {
    if (input$data_type_DE == "Escolha uma opção...") {
      showNotification("Por favor, selecione uma base de dados antes de escolher o estado.", type = "warning")
      
      # Desmarcando a seleção do estado
      updateSelectInput(session, "estado_mapa_DE", selected = character(0))
    }
  })
  
  # Observador para atualizar as variáveis de acordo com a base selecionada no segundo painel
  observe({
    req(input$data_type_DE != "Escolha uma opção...")  # Verifica se a base foi selecionada
    
    variaveis_DE <- names(df_DE())[sapply(df_DE(), is.numeric)]
    variaveis_DE <- variaveis_DE[!variaveis_DE %in% c("Código UF", "Código Região")]
    variaveis_DE <- sort(variaveis_DE)
    updateSelectInput(session, "variavel_mapa_DE", choices = variaveis_DE, selected = variaveis_DE[1])
  })
  
  # Interface UI para o seletor de variáveis
  output$variavel_mapa_ui_DE <- renderUI({
    selectInput("variavel_mapa_DE", "Selecione a Variável para ser apresentada na tabela ao lado:",
                choices = NULL,
                multiple = TRUE)
  })
  
  # Observer para apresentar apenas as cidades do estado previamente selecionado dentro do PAINEL 2
  observe({
    req(input$data_type_DE != "Escolha uma opção...")  # Verifica se a base foi selecionada
    
    Cidades_DE <- if (is.null(input$estado_mapa_DE)) character(0) else {
      filter(df_DE(), df_DE()$Sigla_UF %in% input$estado_mapa_DE) %>%
        `$`('Nome Município') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cidades_DE[input$cidades_DE %in% Cidades_DE])
    updateSelectizeInput(session,
                         "cidades_DE",
                         choices = Cidades_DE,
                         selected = stillSelected,
                         server = TRUE)
  })
  
  # Reactive para gerar a base de dados que será utilizada tanto para a tabela quanto para o gráfico
  df_Final <- reactive({
    # Filtrando a tabela a partir dos dados selecionados na iterface
    
    df_DE() %>%
      filter(
        is.null(input$estado_mapa_DE) | df_DE()$Sigla_UF %in% input$estado_mapa_DE,
        is.null(input$cidades_DE) | df_DE()$`Nome Município` %in% input$cidades_DE) %>%
      select("Código Município",
             "Nome Município",
             "Nome UF",
             input$variavel_mapa_DE) %>% # Adicionamos dinamicamente todas as variaveis selecionadas pelo usuário
      rename_with(~ substr(., 0, 20))     # Diminuindo o tamanho do nome da variável para melhorar o layout da tabela
    
  })
  
  # Renderizando a tabela final
  output$DataTable_DE <- DT::renderDataTable({
    req(input$data_type_DE != "Escolha uma opção...")
    validate(
      need(!is.null(df_Final()), "Nenhuma tabela a ser exibida.")
    )
    
    # Identificar quais colunas são numéricas
    numeric_cols <- which(sapply(df_Final(), is.numeric))
    
    # Construir datatable e formatar arredondando em 3 casas
    DT::datatable(
      df_Final(),
      escape = FALSE,
      options = list(scrollX = TRUE)
    ) %>%
      DT::formatRound(columns = numeric_cols, digits = 5)
  })
  
  # Alterando para renderUI para exibir uma mensagem amigável
  output$DataTable_DE_UI <- renderUI({
    # Verifica se a opção padrão está selecionada
    if (input$data_type_DE == "Escolha uma opção...") {
      # Exibe uma mensagem amigável
      HTML("<p>Selecione uma base de dados para visualizar a tabela.</p>")
    } else {
      # Renderiza a tabela quando uma opção é selecionada
      DT::dataTableOutput("DataTable_DE")
    }
  })
  
  # Limitar a seleção a 15 variáveis e exibir uma mensagem de alerta se exceder
  observeEvent(input$variavel_mapa_DE, {
    if (length(input$variavel_mapa_DE) > 15) {
      # Manter apenas as 15 primeiras variáveis selecionadas
      updateSelectInput(session, "variavel_mapa_DE", selected = input$variavel_mapa_DE[1:15])
      
      # Exibir uma notificação de alerta
      showNotification("Você pode selecionar no máximo 15 variáveis para exibir na tabela.", type = "error")
    }
  })
  
  # Renderizando o gráfico que será apresentado abaixo do Texto
  output$plot_DE <- plotly::renderPlotly({
    
    # validando se o usuário selecionou o mínimo necessário de variáveis para gerar o gráfico
    validate(
      need(length(input$variavel_mapa_DE) >= 2, 'Por favor selecione pelo menos 2 variáveis para gerar o gráfico')
    )
    
    p <- df_Final() %>%
      ggplot(aes(
        x = .data[[substr(input$variavel_mapa_DE[1], 0, 20)]], # estou fazendo o substr() para que as colunas tenham exatamente o mesmo nome da variável de Input
        y = .data[[substr(input$variavel_mapa_DE[2], 0, 20)]],
        text = .data[["Nome Município"]]
      )) +
      geom_point() +
      theme_minimal()
    
    plotly::ggplotly(p)
  })
  
  #------------#------------#------------#------------------------------
  #------------ PAINEL 3 - RAIO X MUNICIPAL  LADO SERVIDOR --------------
  #------------#------------#------------#------------------------------
  
  # Reactive para gerar a base de dados que será utilizada por todo o Painel 3
  df_RM <- reactive({
    # Retorne NULL se a escolha for o placeholder
    if (input$data_type_RM == "Escolha uma opção...") return(NULL)
    
    # Filtrando a tabela a partir dos dados selecionados na interface
    if (input$data_type_RM == "Dados Brutos") {
      st_drop_geometry(dados_brutos)
    } else {
      st_drop_geometry(dados_per_capita)
    }
  })
  
  # Verificação ao selecionar o estado sem escolher a base
  observeEvent(input$estado_mapa_RM, {
    if (input$data_type_RM == "Escolha uma opção...") {
      showNotification("Por favor, selecione uma base de dados antes de escolher o estado.", type = "warning")
      
      # Desmarcando a seleção do estado
      updateSelectInput(session, "estado_mapa_RM", selected = character(0))
    }
  })
  
  # Observador para atualizar as variáveis de acordo com a base selecionada no terceiro painel
  observe({
    req(input$data_type_RM != "Escolha uma opção...")  # Verifica se a base foi selecionada
    
    variaveis_RM <- names(df_RM())[sapply(df_RM(), is.numeric)]
    variaveis_RM <- variaveis_RM[!variaveis_RM %in% c("Código UF", "Código Região")]
    variaveis_RM <- sort(variaveis_RM)
    updateSelectInput(session, "variavel_mapa_RM", choices = variaveis_RM, selected = variaveis_RM[1])
  })
  
  # Interface UI para o seletor de variáveis
  output$variavel_mapa_ui_RM <- renderUI({
    selectInput("variavel_mapa_RM", "Selecione as Variáveis para o Gráfico Radar:",
                choices = NULL,
                multiple = TRUE)
  })
  
  # Observador para apresentar apenas as cidades do estado previamente selecionado dentro do PAINEL 3
  observe({
    req(input$data_type_RM != "Escolha uma opção...")  # Verifica se a base foi selecionada
    
    Cidades_RM <- if (is.null(input$estado_mapa_RM)) character(0) else {
      filter(df_RM(), df_RM()$Sigla_UF %in% input$estado_mapa_RM) %>%
        `$`('Nome Município') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cidades_RM[input$cidades_RM %in% Cidades_RM])
    updateSelectizeInput(session,
                         "cidades_RM",
                         choices = Cidades_RM,
                         selected = stillSelected,
                         server = TRUE)
  })
  
  # Calculando os valores mínimos e máximos globais da base de dados completa
  valores_min_max <- reactive({
    req(input$variavel_mapa_RM)  # Verifica se há variáveis selecionadas
    
    df_RM() %>%
      select(input$variavel_mapa_RM) %>%
      summarise(across(everything(), list(Mínimo = min, Máximo = max), na.rm = TRUE)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = c("Variável", ".value"), names_sep = "_")
  })
  
  # Reactive para gerar a base de dados que será utilizada para gerar o gráfico do Raio-X Municipal
  df_RM_Final <- reactive({
    req(input$variavel_mapa_RM)  # Certifica-se de que variáveis foram selecionadas
    
    # Filtrando a tabela a partir dos dados selecionados na interface
    df_RM() %>%
      mutate(Nome_Completo = paste(df_RM()$`Nome Município`, ",", Sigla_UF)) %>%
      filter(
        is.null(input$estado_mapa_RM) | df_RM()$Sigla_UF %in% input$estado_mapa_RM,
        is.null(input$cidades_RM) | df_RM()$`Nome Município` %in% input$cidades_RM) %>%
      select("Nome_Completo", input$variavel_mapa_RM) %>%
      # Normalização min-max baseada nos valores globais da base de dados completa
      mutate(across(-Nome_Completo,
                    ~ (. - valores_min_max()$Mínimo[which(valores_min_max()$Variável == cur_column())]) /
                      (valores_min_max()$Máximo[which(valores_min_max()$Variável == cur_column())] -
                         valores_min_max()$Mínimo[which(valores_min_max()$Variável == cur_column())]),
                    .names = "Valor_Escalonado_{col}")) %>%
      tidyr::pivot_longer(cols = starts_with("Valor_Escalonado"),
                          names_to = "Variável",
                          names_prefix = "Valor_Escalonado_",
                          values_to = "Valor_Escalonado")
  })
  
  # Renderizando o gráfico scatterpolar
  output$scatterpolar_rm <- plotly::renderPlotly({
    validate(
      need(length(input$cidades_RM) >= 2, 'Por favor selecione ao menos 2 cidades'),
      need(length(input$variavel_mapa_RM) >= 3, 'Por favor selecione no mínimo 3 variáveis para gerar o gráfico')
    )
    
    plot_ly(
      df_RM_Final(),
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) %>%
      add_trace(
        r = ~Valor_Escalonado,
        theta = ~Variável,
        showlegend = TRUE,
        mode = "markers",
        name = ~Nome_Completo
      )
  })
  
  # Reactive para gerar a base de dados e também a tabela informativa com os valores originais, mínimos, máximos e valores escalonados
  df_RM_Info <- reactive({
    req(input$variavel_mapa_RM, input$cidades_RM)  # Certifica-se de que variáveis e cidades foram selecionadas
    
    # Filtrando para os municípios e variáveis selecionados
    df_selecionado <- df_RM() %>%
      filter(`Nome Município` %in% input$cidades_RM) %>%
      select(Nome_Completo = `Nome Município`, input$variavel_mapa_RM)
    
    # Calculando os valores mínimos e máximos de cada variável com base no universo completo da base de dados
    valores_min_max <- df_RM() %>%
      select(input$variavel_mapa_RM) %>%
      summarise(across(everything(), list(Mínimo = min, Máximo = max), na.rm = TRUE)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = c("Variável", ".value"), names_sep = "_")
    
    # Adicionando a coluna "Valor_Escalonado" ao dataframe df_selecionado usando os valores globais
    valores_escalonados <- df_selecionado %>%
      mutate(across(-Nome_Completo,
                    ~ (. - valores_min_max$Mínimo[which(valores_min_max$Variável == cur_column())]) /
                      (valores_min_max$Máximo[which(valores_min_max$Variável == cur_column())] -
                         valores_min_max$Mínimo[which(valores_min_max$Variável == cur_column())]),
                    .names = "Valor_Escalonado_{col}")) %>%
      tidyr::pivot_longer(cols = starts_with("Valor_Escalonado"),
                          names_to = "Variável",
                          names_prefix = "Valor_Escalonado_",
                          values_to = "Valor_Escalonado")
    
    # Transformando para o formato long e juntando com os valores mínimo e máximo
    valores_long <- df_selecionado %>%
      tidyr::pivot_longer(cols = -Nome_Completo, names_to = "Variável", values_to = "Valor Original") %>%
      left_join(valores_min_max, by = "Variável") %>%
      left_join(valores_escalonados, by = c("Nome_Completo", "Variável")) %>%
      select(Nome_Completo, Variável, `Valor Original`, Mínimo, Máximo, Valor_Escalonado)  # Selecionando apenas as colunas necessárias
    
    return(valores_long)
  })
  
  # Código para gerar a tabela resumo abaixo do gráfico de scatterplot
  output$informacao_tabela_RM <- DT::renderDataTable({
    req(df_RM_Info())
    
    # 1) Identificar colunas numéricas
    numeric_cols <- which(sapply(df_RM_Info(), is.numeric))
    
    # 2) Apresentar a tabela com arredondamento
    DT::datatable(
      df_RM_Info(),
      options = list(pageLength = 5, scrollX = TRUE)
    ) %>%
      DT::formatRound(columns = numeric_cols, digits = 5)
  })
  
  #------------#------------#------------#------------------------------
  #------------ PAINEL 4 - Modelos Regressão LADO SERVIDOR -------------
  #------------#------------#------------#------------------------------
  
  # Reactive para definir a coluna de resíduos com base na variável dependente e no modelo selecionado
  modelo_residuos_coluna <- reactive({
    req(input$var_dependente != "Escolha uma opção...", input$modelo)
    
    # Seleciona o nome da coluna de resíduos com base nos inputs
    if (input$var_dependente == "Total de Idosos per capita (TIpc)") {
      switch(input$modelo,
             "Regressão Linear" = "model_Linear_Res_TIpc",
             "Regressão SAR" = "model_SAR_Res_TIpc",
             "GWR Básico" = "model_GWR.BASIC_Res_TIpc",
             "GWR Multiscale" = "model_GWR.MULT_Res_TIpc")
    } else if (input$var_dependente == "Mediana de Idade dos Idosos (MII)") {
      switch(input$modelo,
             "Regressão Linear" = "model_Linear_Res_MII",
             "Regressão SAR" = "model_SAR_Res_MII",
             "GWR Básico" = "model_GWR.BASIC_Res_MII",
             "GWR Multiscale" = "model_GWR.MULT_Res_MII")
    } else if (input$var_dependente == "Expectativa de Vida ao Nascer (EdVN)") {
      switch(input$modelo,
             "Regressão Linear" = "model_linear_EdVN",
             "Regressão SAR" = "model_SAR_EdVN",
             "GWR Básico" = "model_GWR.Basic_EdVN",
             "GWR Multiscale" = "model_GWR_EdVN")
    }
  })
  
  # Título dinâmico para o mapa
  output$titulo_mapa_residuos <- renderUI({
    if (input$var_dependente == "Escolha uma opção...") {
      NULL  # Não exibe título
    } else {
      titulo_mapa <- paste("Resíduos do modelo de regressão:", input$modelo, "-", input$var_dependente)
      HTML(paste0("<h4>", titulo_mapa, "</h4>"))
    }
  })
  
  # Título dinâmico para o sumário do modelo
  output$titulo_summary_modelos <- renderUI({
    if (input$var_dependente == "Escolha uma opção...") {
      NULL  # Não exibe título
    } else {
      titulo_sumario <- paste("Resumo do modelo de regressão criado:", input$modelo, "-", input$var_dependente)
      HTML(paste0("<h4>", titulo_sumario, "</h4>"))
    }
  })
  
  # Renderiza o mapa base apenas uma vez
  output$mapa_residuos <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -55, lat = -14, zoom = 4)  # Centraliza o mapa no Brasil
  })
  
  # Observa mudanças nos inputs e atualiza o mapa com leafletProxy
  observeEvent({
    input$var_dependente
    input$modelo
    input$estado_mapa_modelos
    input$exibir_legenda_modelos
  }, {
    if (input$var_dependente == "Escolha uma opção...") {
      leafletProxy("mapa_residuos") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = "<div style='font-size: 16px; text-align: center;'><strong>Por favor, selecione uma variável dependente para visualizar o mapa.</strong></div>",
          position = "topright"
        )
    } else {
      req(modelo_residuos_coluna())
      
      # Filtra os dados conforme o estado selecionado
      dados_mapa <- if (input$estado_mapa_modelos != "Todos os estados") {
        sf_objeto_GWR %>% filter(Sigla_UF == input$estado_mapa_modelos)
      } else {
        sf_objeto_GWR
      }
      
      # Certifica-se de que a projeção está em WGS84
      dados_mapa <- st_transform(dados_mapa, crs = 4326)
      
      residuos <- dados_mapa[[modelo_residuos_coluna()]]
      pal <- colorNumeric("RdYlBu", domain = residuos, na.color = "transparent", reverse = TRUE)
      
      leafletProxy("mapa_residuos", data = dados_mapa) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor = ~pal(residuos),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          popup = ~paste(
            "<strong>Município:</strong>", `Nome.Município`, "<br>",
            "<strong>UF:</strong>", Sigla_UF, "<br>",
            "<strong>Resíduo:</strong>", residuos
          )
        ) %>%
        {if (input$exibir_legenda_modelos) addLegend(., pal = pal, values = residuos,
                                                     title = paste("Resíduos -", input$modelo),
                                                     position = "bottomright") else .}
      
      # Reposicionar o mapa para o estado selecionado
      if (input$estado_mapa_modelos != "Todos os estados") {
        # Verifica se existem dados após o filtro
        if (nrow(dados_mapa) > 0) {
          bbox <- st_bbox(dados_mapa)
          
          leafletProxy("mapa_residuos") %>%
            fitBounds(lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
                      lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"]))
        } else {
          # Se não houver dados, centraliza o mapa no Brasil
          leafletProxy("mapa_residuos") %>%
            setView(lng = -55, lat = -14, zoom = 4)
        }
      } else {
        # Se "Todos os estados" for selecionado, ajustar o mapa para mostrar o Brasil inteiro
        leafletProxy("mapa_residuos") %>%
          setView(lng = -55, lat = -14, zoom = 4)
      }
    }
    
  })
  
  # Função para sumarizar o modelo GWR Multiscale de forma detalhada
  summary_gwr <- function(modelo) {
    cat("Resumo do Modelo GWR Multiscale \n")
    cat("----------------------------------\n")
    
    print(modelo$this.call)
    cat("----------------------------------\n")
    
    # R² Global e AICc Global, conforme disponíveis
    if (!is.null(modelo$GW.diagnostic$R2.val)) {
      cat("R² Global:", modelo$GW.diagnostic$R2.val, "\n")
      cat("R² Global Ajustado:", modelo$GW.diagnostic$R2adj, "\n")
    }
    if (!is.null(modelo$GW.diagnostic$AICc)) {
      cat("AICc Global:", modelo$GW.diagnostic$AICc, "\n")
    }
    
    # Estatísticas locais (por exemplo, estatísticas resumidas)
    cat("\nResumo das larguras de banda calculadas:\n")
    if (!is.null(modelo$GW.arguments$bws)) {
      print(modelo$GW.arguments$bws)
    } else {
      cat("Estatísticas locais não disponíveis\n")
    }
    
    # Resumo dos coeficientes globais e das estatísticas adicionais
    cat("\nCoeficientes Estimados por Variável com Erros Padrão e Valores-T:\n")
    if (!is.null(modelo$SDF)) {
      # Extrai os nomes dos coeficientes, erros padrão e valores-t
      coeficientes <- names(modelo$SDF)[!grepl("_SE|_TV", names(modelo$SDF))]
      
      for (coef in coeficientes) {
        cat("\nResumo para:", coef, "\n")
        
        # Mostra o summary do coeficiente principal
        cat("Coeficiente:\n")
        print(summary(modelo$SDF[[coef]]))
        
        # Checa se o erro padrão e valor-t estão presentes e imprime
        coef_se <- paste0(coef, "_SE")
        if (coef_se %in% names(modelo$SDF)) {
          cat("Erro Padrão:\n")
          print(summary(modelo$SDF[[coef_se]]))
        } else {
          cat("Erro Padrão não disponível\n")
        }
        
        coef_tv <- paste0(coef, "_TV")
        if (coef_tv %in% names(modelo$SDF)) {
          cat("Valor-T:\n")
          print(summary(modelo$SDF[[coef_tv]]))
        } else {
          cat("Valor-T não disponível\n")
        }
      }
    } else {
      cat("Coeficientes não disponíveis\n")
    }
    
    cat("----------------------------------\n")
    cat("FIM DO RESUMO\n")
  }
  
  # Função para sumarizar o modelo GWR Basico de forma detalhada
  summary_gwr_basic <- function(modelo) {
    cat("Resumo do Modelo GWR Basic \n")
    cat("----------------------------------\n")
    
    print(modelo$this.call)
    cat("----------------------------------\n")
    
    # R² Global e AICc Global, conforme disponíveis
    if (!is.null(modelo$GW.diagnostic$gw.R2)) {
      cat("R² Global:", modelo$GW.diagnostic$gw.R2, "\n")
      cat("R² Global Ajustado:", modelo$GW.diagnostic$gwR2.adj, "\n")
    }
    if (!is.null(modelo$GW.diagnostic$AICc)) {
      cat("AICc Global:", modelo$GW.diagnostic$AICc, "\n")
    }
    
    # Estatísticas locais (por exemplo, estatísticas resumidas)
    cat("\nLargura de banda calculada:\n")
    if (!is.null(modelo$GW.arguments$bw)) {
      print(modelo$GW.arguments$bw)
    } else {
      cat("Estatísticas locais não disponíveis\n")
    }
    
    # Resumo dos coeficientes globais e das estatísticas adicionais
    cat("\nCoeficientes Estimados por Variável com Erros Padrão e Valores-T:\n")
    if (!is.null(modelo$SDF)) {
      # Extrai os nomes dos coeficientes, erros padrão e valores-t
      coeficientes <- names(modelo$SDF)[!grepl("_SE|_TV", names(modelo$SDF))]
      
      for (coef in coeficientes) {
        cat("\nResumo para:", coef, "\n")
        
        # Mostra o summary do coeficiente principal
        cat("Coeficiente:\n")
        print(summary(modelo$SDF[[coef]]))
        
        # Checa se o erro padrão e valor-t estão presentes e imprime
        coef_se <- paste0(coef, "_SE")
        if (coef_se %in% names(modelo$SDF)) {
          cat("Erro Padrão:\n")
          print(summary(modelo$SDF[[coef_se]]))
        } else {
          cat("Erro Padrão não disponível\n")
        }
        
        coef_tv <- paste0(coef, "_TV")
        if (coef_tv %in% names(modelo$SDF)) {
          cat("Valor-T:\n")
          print(summary(modelo$SDF[[coef_tv]]))
        } else {
          cat("Valor-T não disponível\n")
        }
      }
    } else {
      cat("Coeficientes não disponíveis\n")
    }
    
    cat("----------------------------------\n")
    cat("FIM DO RESUMO\n")
  }
  
  
  # Renderização do sumário do modelo com ajuste para o GWR Multiscale e SAR
  output$summary_output_modelos <- renderPrint({
    req(input$var_dependente != "Escolha uma opção...", input$modelo)
    
    var_dep <- switch(input$var_dependente,
                      "Total de Idosos per capita (TIpc)" = "TIpc",
                      "Mediana de Idade dos Idosos (MII)" = "MII",
                      "Expectativa de Vida ao Nascer (EdVN)" = "EdVN")
    
    modelo_selecionado <- models[[var_dep]][[input$modelo]]
    
    if (input$modelo == "GWR Multiscale") {
      summary_gwr(modelo_selecionado)
    }
    if (input$modelo == "GWR Básico") {
      summary_gwr_basic(modelo_selecionado)
    }
    else {
      print(summary(modelo_selecionado))
    }
  })

  #------------#------------#------------#-------------------------------
  #------------ PAINEL 5 - Modelos Locais  LADO Servidor- ---------------
  #------------#------------#------------#-------------------------------
  
  # "Mapeia" o nome que aparece na UI para a "chave" que criamos na lista
  varDepKey_locais <- reactive({
    req(input$var_dependente_locais)
    switch(input$var_dependente_locais,
           "Total de Idosos per capita (TIpc)" = "TIpc",
           "Mediana de Idade dos Idosos (MII)" = "MII",
           "Expectativa de Vida ao Nascer (EdVN)" = "EdVN",
           "Escolha uma opção..." = NULL)
  })
  
  # Retorna o sf correto
  sf_escolhido_locais <- reactive({
    req(varDepKey_locais(), input$tipo_modelo_locais)
    
    # A lista final
    sf_locais[[ varDepKey_locais() ]][[ input$tipo_modelo_locais ]]
  })
  
  output$coeficiente_locais_ui <- renderUI({
    req(sf_escolhido_locais())
    
    # 1) Remove a geometria para evitar a coluna geometry seja selecionada
    dados_sem_geom <- sf::st_drop_geometry(sf_escolhido_locais())
    
    # 2) Pega apenas as colunas numéricas e remove colunas irrelevantes para o estudo
    colunas <- names(dados_sem_geom)
    colunas_num <- colunas[sapply(dados_sem_geom[, colunas], is.numeric)]
    colunas_num <- setdiff(colunas_num, c("Código.UF", "Código.Região"))
    
    # Remove yhat, residual e colunas terminando em "_SE" ou "_TV"
    colunas_filtradas <- colunas_num[!grepl("^(yhat|residual)$|_SE$|_TV$", colunas_num)]
    
    # Se não sobrou nada, usar uma frase direto dentro do mapa
    if (length(colunas_filtradas) == 0) {
      colunas_filtradas <- "Nenhum coeficiente encontrado"
    }
    
    # 3) Criamos a versão amigável (sem pontos) para exibir o nome dos coeficientes
    colunas_friendly <- gsub("\\.", " ", colunas_filtradas)
    
    # 4) Construímos o vetor nomeado de choices, de forma que:
    #    - O "valor" (o que o Shiny salva em input$coeficiente_locais) é o nome real (com ponto).
    #    - O "nome" (o que aparece na UI) é a versão amigável (sem ponto).
    choices_nomeado <- setNames(colunas_filtradas, colunas_friendly)
    
    selectInput(
      inputId = "coeficiente_locais",
      label   = "Selecione o Coeficiente a ser mapeado:",
      choices = choices_nomeado,
      selected = colunas_filtradas[1],  # O valor real (com ponto)
      width = "100%"                    # Ajusta a caixa à largura do painel
    )
  })
  
  output$municipio_locais_ui <- renderUI({
    req(sf_escolhido_locais())
    df <- sf::st_drop_geometry(sf_escolhido_locais())
    # Use distinct para obter os municípios únicos
    df <- df %>% distinct(`Código.Município`, `Nome.Município`, Sigla_UF)
    choices_amigaveis <- paste0(df$`Nome.Município`, " (", df$Sigla_UF, ")")
    selectInput("municipio_locais", "Selecione o Município:", 
                choices = setNames(df$`Código.Município`, choices_amigaveis),
                selected = NULL,
                width = "100%")
  })
  
  # Renderizar o mapa base (vazio) do painel "Modelos Locais" apenas uma vez
  output$mapa_locais <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -55, lat = -14, zoom = 4)  # Centro do Brasil
  })
  
  # Observador para atualizar o mapa quando as entradas mudarem
  observeEvent({
    input$var_dependente_locais
    input$tipo_modelo_locais
    input$coeficiente_locais
    input$estado_locais
    input$exibir_legenda_locais
    input$exibir_significancia_locais
    input$exibir_nsignificancia_locais
  }, {
    # Se a variável dependente não estiver selecionada
    if (input$var_dependente_locais == "Escolha uma opção...") {
      leafletProxy("mapa_locais") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = "<div style='font-size: 16px; text-align: center;'>
                  <strong>Por favor, selecione uma variável dependente e o modelo local.</strong>
                </div>",
          position = "topright"
        )
      return(NULL)
    }
    
    req(sf_escolhido_locais(), input$coeficiente_locais)
    
    # Pega o sf correspondente ao modelo e variável (função reativa)
    sf_dados <- sf_escolhido_locais()
    
    # Filtro pelo estado, caso não seja "Todos os estados"
    if (input$estado_locais != "Todos os estados") {
      sf_dados <- dplyr::filter(sf_dados, Sigla_UF == input$estado_locais)
    }
    
    # Se não sobrou linhas após o filtro, exibir mensagem
    if (nrow(sf_dados) == 0) {
      leafletProxy("mapa_locais") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = "<div style='font-size: 16px; text-align: center; color:red;'>
                  <strong>Nenhum dado encontrado após o filtro.</strong>
                </div>",
          position = "topright"
        )
      return(NULL)
    }
    
    # Certificar projeção WGS84
    sf_dados <- sf::st_transform(sf_dados, crs = 4326)
    
    # Vetor de coeficientes
    valores_coef <- sf_dados[[ input$coeficiente_locais ]]
    
    if (length(valores_coef) == 0 || all(is.na(valores_coef))) {
      leafletProxy("mapa_locais") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = "<div style='font-size: 16px; text-align: center; color:red;'>
                  <strong>Não há dados disponíveis para esse coeficiente.</strong>
                </div>",
          position = "topright"
        )
      return(NULL)
    }
    
    # Criando a paleta de cores
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = valores_coef,
      na.color = "transparent",
      reverse = TRUE
    )
    
    # Desenha os polígonos
    leafletProxy("mapa_locais", data = sf_dados) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor   = ~ pal(valores_coef),
        fillOpacity = 0.7,
        color       = "white",
        weight      = 1,
        popup = ~paste(
          "<strong>Município:</strong>", `Nome.Município`, "<br>",
          "<strong>Estado:</strong>", Sigla_UF, "<br>",
          "<strong>", input$coeficiente_locais, ":</strong>", round(valores_coef, 4)
        )
      ) %>%
      {
        if (input$exibir_legenda_locais) {
          addLegend(.,
                    pal    = pal,
                    values = valores_coef,
                    title  = paste("Coeficiente:", input$coeficiente_locais),
                    position = "bottomright")
        } else {
          .
        }
      }
    
    # Agora, ajustamos o zoom (fitBounds) para o bounding box do que foi desenhado
    bbox <- sf::st_bbox(sf_dados)  # bounding box do estado (ou de todo o Brasil se "Todos os estados")
    
    leafletProxy("mapa_locais") %>%
      fitBounds(
        lng1 = as.numeric(bbox["xmin"]), lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]), lat2 = as.numeric(bbox["ymax"])
      )
    # ========= NOVO BLOCO PARA A CAMADA EXTRA ============
    # Obtenha o nome da coluna t-value:
    t_value_col <- paste0(input$coeficiente_locais, "_TV")
    
    if (t_value_col %in% names(sf_dados)) {
      # Se o checkbox para significância estiver marcado:
      if (isTRUE(input$exibir_significancia_locais)) {
        # Filtra municípios com |t-value| > 1.96
        sf_extra <- sf_dados[ abs(sf_dados[[t_value_col]]) > 1.96, ]
        if (nrow(sf_extra) > 0) {
          leafletProxy("mapa_locais", data = sf_extra) %>%
            addPolygons(
              fill = FALSE,
              color = "black",
              weight = 2,
              opacity = 1,
              popup = ~paste(
                "<strong>Município:</strong>", `Nome.Município`, "<br>",
                "<strong>Estado:</strong>", Sigla_UF, "<br>",
                "<strong>T-Value:</strong>", round(sf_extra[[t_value_col]], 2)
              )
            )
        } else {
          showNotification("Nenhum município com |t-value| > 1.96.", type = "message")
        }
      } else if (isTRUE(input$exibir_nsignificancia_locais)) {
        # Filtra municípios com |t-value| ≤ 1.96
        sf_extra <- sf_dados[ abs(sf_dados[[t_value_col]]) <= 1.96, ]
        if (nrow(sf_extra) > 0) {
          leafletProxy("mapa_locais", data = sf_extra) %>%
            addPolygons(
              fill = FALSE,
              color = "black",
              weight = 2,
              opacity = 1,
              popup = ~paste(
                "<strong>Município:</strong>", `Nome.Município`, "<br>",
                "<strong>Estado:</strong>", Sigla_UF, "<br>",
                "<strong>T-Value:</strong>", round(sf_extra[[t_value_col]], 2)
              )
            )
        } else {
          showNotification("Nenhum município com |t-value| ≤ 1.96.", type = "message")
        }
      }
    } else {
      showNotification("Não foi encontrada a coluna de T-Value para esse coeficiente.", type = "warning")
    }
  })
  
  output$titulo_detalhes_municipio <- renderUI({
    req(sf_escolhido_locais(), input$municipio_locais)
    df <- sf::st_drop_geometry(sf_escolhido_locais())
    df_mun <- df %>% filter(`Código.Município` == input$municipio_locais)
    if(nrow(df_mun) > 0) {
      nome <- df_mun$`Nome.Município`[1]
      uf <- df_mun$Sigla_UF[1]
      h4(paste0("Análise das variáveis locais no município '", nome, " (", uf, ")'"))
    } else {
      NULL
    }
  })
  
  
  output$titulo_mapa_locais <- renderUI({
    if (input$var_dependente_locais == "Escolha uma opção...") {
      return(NULL)  # Não exibe título
    }
    # Montamos um texto dinâmico
    titulo <- paste(
      "Mapa de Coeficientes Locais –", 
      input$tipo_modelo_locais, "–", 
      input$var_dependente_locais,
      "<br>Coeficiente Selecionado:", input$coeficiente_locais
    )
    HTML(paste0("<h4>", titulo, "</h4>"))
  })
  
  
  coeficientes_municipio <- reactive({
    req(sf_escolhido_locais(), input$municipio_locais)
    df <- sf::st_drop_geometry(sf_escolhido_locais())
    # Remove as colunas que não usaremos
    df <- df %>% select(-one_of("Código.UF", "Código.Região", "Local_R2"))
    
    # Filtra pelo município selecionado (usando "Código.Município")
    df_mun <- df %>% filter(`Código.Município` == input$municipio_locais)
    
    # Seleciona as colunas numéricas (coeficientes) que não terminam com _SE ou _TV
    coef_cols <- names(df_mun)[sapply(df_mun, is.numeric)]
    coef_cols <- coef_cols[ !grepl("^(yhat|residual)$|_SE$|_TV$", coef_cols) ]
    
    if (length(coef_cols) == 0) return(NULL)
    
    df_long <- data.frame(
      Coeficiente = coef_cols,
      `Valor Coeficiente` = as.numeric(df_mun[1, coef_cols]),
      stringsAsFactors = FALSE
    )
    
    # Extração dos t-values
    t_values <- sapply(coef_cols, function(col) {
      tcol <- paste0(col, "_TV")
      if (tcol %in% names(df_mun)) {
        return(as.numeric(df_mun[1, tcol]))
      } else {
        return(NA)
      }
    })
    df_long$T_Value <- t_values
    df_long$Significativo <- ifelse(!is.na(t_values) & abs(t_values) > 1.96, "Sim", "Não")
    
    # Extração da largura de banda conforme o modelo selecionado
    var_dep <- switch(input$var_dependente_locais,
                      "Total de Idosos per capita (TIpc)" = "TIpc",
                      "Mediana de Idade dos Idosos (MII)" = "MII",
                      "Expectativa de Vida ao Nascer (EdVN)" = "EdVN")
    modelo <- models[[var_dep]][[input$tipo_modelo_locais]]
    
    if (input$tipo_modelo_locais == "GWR Básico") {
      bw <- modelo$GW.arguments$bw
      df_long$Largura_de_Banda <- bw
    } else if (input$tipo_modelo_locais == "GWR Multiscale") {
      bws <- modelo$GW.arguments$bws
      #print(bws)  # Para depuração
      # Se o número de elementos não coincidir com o número de coeficientes, um aviso será gerado.
      if(length(bws) != nrow(df_long)) {
        warning("O número de larguras de banda não coincide com o número de coeficientes.")
      }
      df_long$Largura_de_Banda <- bws
    }
    
    # Agora, adicionamos os valores reais do município
    # Na base "dados_per_capita" o código do município é "Código Município"
    df_valores <- st_drop_geometry(dados_per_capita) %>%
      mutate(`Código.Município` = `Código Município`) %>%
      filter(`Código.Município` == input$municipio_locais)
    
    # Para cada coeficiente, convertemos o nome (com pontos) para o nome usado na base de valores (com espaços)
    # Em algum ponto do seu código, ao montar a tabela de detalhes:
    # Dentro da função reativa que gera 'coeficientes_municipio', logo após definir df_long:
    
    # Obter os valores reais (da base de valores) para cada variável, tratando o intercept
    df_long$Valor_Variável <- sapply(df_long$Coeficiente, function(col) {
      # Se for intercept, retornamos 1 (ou outro valor que você deseje)
      if (tolower(col) == "intercept") {
        return(NA)
      }
      # Mapear o nome do coeficiente para o nome usado na base de valores
      percapita_col <- if (col %in% names(nome_mapeamento)) {
        nome_mapeamento[[col]]
      } else {
        # Substituir os pontos por espaços
        gsub("\\.", " ", col)
      }
      # Se a coluna mapeada existe na base de valores, retornar seu valor; caso contrário, NA
      if (percapita_col %in% names(df_valores)) {
        return(as.numeric(df_valores[[percapita_col]]))
      } else {
        return(NA)
      }
    })
    
    # Agora, calculamos o produto entre o coeficiente e o valor real da variável
    #df_long$`Produto Coeficiente x Variável` <- df_long$`Valor Coeficiente` * df_long$Valor_Variável
    
    
    
    df_long
  })
  
  
  output$detalhes_municipio <- DT::renderDataTable({
    req(coeficientes_municipio())
    
    # Vamos salvar em um objeto para facilitar
    df_det <- coeficientes_municipio()
    
    # Identificar colunas numéricas
    numeric_cols <- which(sapply(df_det, is.numeric))
    
    DT::datatable(
      df_det,
      options = list(pageLength = 27, scrollX = TRUE)
    ) %>%
      DT::formatRound(columns = numeric_cols, digits = 5)
  })
  
  
  
    
  #------------#------------#------------#-------------------------------
  #------------ PAINEL 6 - Análise de Documento PDF LADO SERVIDOR -------
  #------------#------------#------------#-------------------------------
  
  # Definir reactiveValues para armazenar os resultados
  summaries <- reactiveValues(
    elderly_summary = NULL,
    wordcloud_data = NULL,
    word_freq = NULL
  )
  
  # Reactive expression para ler o texto do PDF
  pdfText <- reactive({
    req(input$pdfFile)
    # Desabilitar os botões ao iniciar o processamento de um novo arquivo
    shinyjs::disable("download_elderly_summary")
    shinyjs::disable("generate_plots")
    texto <- pdftools::pdf_text(input$pdfFile$datapath)
    # Excluir o arquivo após a leitura
    file.remove(input$pdfFile$datapath)
    paste(texto, collapse = " ")
  })
  
  # Observador para gerar o resumo para pessoas idosas imediatamente após o upload
  observeEvent(pdfText(), {
    texto <- pdfText()
    summaries$elderly_summary <- generate_elderly_summary(texto)
    
    # Verificar se o resumo foi gerado com sucesso
    if (!is.null(summaries$elderly_summary) && summaries$elderly_summary != "") {
      # Habilitar o botão de download após a geração do resumo
      shinyjs::enable("download_elderly_summary")
      # Habilitar o botão para gerar nuvem de palavras e gráfico
      shinyjs::enable("generate_plots")
    } else {
      # Mostrar notificação de erro
      showNotification("Erro ao gerar o resumo. Por favor, tente novamente.", type = "error")
    }
  })
  
  # Observador para gerar a nuvem de palavras e o gráfico quando o botão é clicado
  observeEvent(input$generate_plots, {
    texto <- pdfText()
    summaries$wordcloud_data <- prepare_wordcloud(texto)
    summaries$word_freq <- prepare_word_freq(texto)
  })
  
  # Função para gerar resumo do ponto de vista de uma pessoa idosa em formato de bullet points
  generate_elderly_summary <- function(text) {
    response <- tryCatch({
      openai::create_chat_completion(
        model = "gpt-3.5-turbo",
        messages = list(
          list(
            role = "system",
            content = "Você é um assistente que resume textos em português do ponto de vista de uma pessoa idosa brasileira. Destaque as principais ações e medidas adotadas para a população idosa e como os idosos podem se beneficiar dessas ações."
          ),
          list(role = "user", content = paste("Resuma o seguinte texto em formato de lista com bullet points considerando o ponto de vista de uma pessoa idosa:\n\n", text))
        ),
        temperature = 0.5,
        max_tokens = 500  # Ajustado para o tamanho do resumo
      )
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.list(response) && !is.null(response$choices) && length(response$choices) > 0) {
      content <- response$choices$message.content[1]
      if (!is.null(content)) {
        content <- gsub("\n+", "\n", content)
        content <- trimws(content)
        return(content)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }
  
  # Função para preparar os dados para a nuvem de palavras
  prepare_wordcloud <- function(text) {
    tokens <- tibble(text = text) %>%
      unnest_tokens(word, text) %>%
      mutate(word = tolower(word)) %>%
      filter(!word %in% stopwords::stopwords("pt")) %>%
      count(word, sort = TRUE) %>%
      filter(n > 1)
    return(tokens)
  }
  
  # Função para preparar os dados para o gráfico de recorrência
  prepare_word_freq <- function(text) {
    tokens <- tibble(text = text) %>%
      unnest_tokens(word, text) %>%
      mutate(word = tolower(word)) %>%
      filter(!word %in% stopwords::stopwords("pt")) %>%
      count(word, sort = TRUE) %>%
      filter(n > 1) %>%
      top_n(20, wt = n)
    return(tokens)
  }
  
  # Renderizar o resumo para pessoas idosas
  output$elderly_summary <- renderText({
    req(summaries$elderly_summary)
    summaries$elderly_summary
  })
  
  # Renderizar a nuvem de palavras
  output$wordcloud <- wordcloud2::renderWordcloud2({
    req(summaries$wordcloud_data)
    wordcloud2::wordcloud2(summaries$wordcloud_data, size = 1, color = "random-light", backgroundColor = "white")
  })
  
  # Renderizar o gráfico de recorrência por palavras
  output$word_freq_plot <- renderPlot({
    req(summaries$word_freq)
    ggplot(summaries$word_freq, aes(x = reorder(word, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Recorrência das 20 Palavras Mais Frequentes",
           x = "Palavras",
           y = "Frequência") +
      theme_minimal()
  })
  
  # Download do Resumo para Pessoas Idosas
  output$download_elderly_summary <- downloadHandler(
    filename = function() {
      paste("Resumo_Pessoas_Idosas_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      resumo_idosos <- summaries$elderly_summary
      writeLines(resumo_idosos, con = file)
    }
  )
  
  #------------#------------#------------#------------------------------
  #-------------- PAINEL 7 - Município Aptos LADO SERVIDOR -------------
  #------------#------------#------------#------------------------------
  
  # ---- Mapa vazio inicial do painel “Municípios aptos” ] ----
  output$mapa_aptos <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -55, lat = -14, zoom = 4)  # Centralizado no Brasil
  })
  
  # ---- Atualizar o mapa sempre que o usuário mudar o estado ou a legenda] ----
  observeEvent({
    input$estado_mapa_aptos
    input$show_legend_aptos
  }, {
    
    # Caso o usuário ainda não tenha selecionado nada (valor == ""), mostramos uma mensagem amistosa
    if (input$estado_mapa_aptos == "Escolha uma UF...") {
      cat("Nenhum estado selecionado: exibindo a mensagem\n")
      leafletProxy("mapa_aptos") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = "<div style='font-size: 16px; text-align: center;'><strong>Por favor, selecione uma Unidaed da Federação para visualizar o mapa.</strong></div>",
          position = "topright"
        )
      return(NULL)
    }
    
    # Se o usuário escolheu "all" (ou "Todos os estados"):
    if (input$estado_mapa_aptos == "all") {
      # Exibimos a base 'estados_aptos.rds'
      
      # Escolha uma paleta para perc_aptos (sendo um valor numérico de 0 a 100, por exemplo)
      pal_estados <- colorNumeric(palette = "YlOrRd", domain = estados_aptos$perc_aptos, na.color = "transparent")
      
      leafletProxy("mapa_aptos", data = estados_aptos) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor   = ~pal_estados(perc_aptos),
          fillOpacity = 0.7,
          color       = "white",
          weight      = 1,
          # No popup, não existe NM_MUN; mostramos UF e perc_aptos
          popup = ~paste(
            "<strong>UF:</strong>", SIGLA_UF, "<br>",
            "<strong>Fundo Estadual Apto? </strong>", `Fundo Apto para receber doações`, "<br>",
            "<strong>CNPJ do Fundo Estadual:</strong>", CNPJ, "<br>",
            "<strong>Percentual de municípios aptos:</strong>", round(perc_aptos, 2), "%"
          )
        ) %>%
        {
          if (input$show_legend_aptos) {
            addLegend(.,
                      pal = pal_estados,
                      values = ~perc_aptos,
                      position = "bottomright",
                      title = "Percentual de municípios aptos"
            )
          } else {
            .
          }
        }
      
      # Ajustar o zoom para o Brasil inteiro
      leafletProxy("mapa_aptos") %>%
        setView(lng = -55, lat = -14, zoom = 4)
      
      return(NULL)
    }
    
    # --------------------------------------------

    # Filtrar apenas os municípios com Fundo Apto = "Sim"
    dados_exibir <- municipios_aptos %>%
      dplyr::filter(`Fundo Apto para receber doações` == "Sim") %>%
      dplyr::filter(SIGLA_UF == input$estado_mapa_aptos)
    
    pal_mun <- colorFactor("DarkGreen", domain = c("Sim"))
    
    leafletProxy("mapa_aptos", data = dados_exibir) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor   = ~pal_mun(`Fundo Apto para receber doações`),
        fillOpacity = 0.7,
        color       = "white",
        weight      = 1,
        popup = ~paste(
          "<strong>Município:</strong>", `NM_MUN`, "<br>",
          "<strong>Estado:</strong>", SIGLA_UF, "<br>",
          "<strong>Apto:</strong>", `Fundo Apto para receber doações`, "<br>",
          "<strong>CNPJ do Fundo:</strong>", CNPJ
        )
      ) %>%
      {
        if (input$show_legend_aptos) {
          addLegend(.,
                    pal = pal_mun,
                    values = c("Sim"),
                    position = "bottomright",
                    title = "Municípios aptos"
          )
        } else {
          .
        }
      }
    
    # Ajustar o zoom ao estado inteiro – inclusive municípios "Não"
    dados_bounding <- municipios_aptos %>%
      dplyr::filter(SIGLA_UF == input$estado_mapa_aptos)
    
    if (nrow(dados_bounding) > 0) {
      bbox <- st_bbox(dados_bounding)
      leafletProxy("mapa_aptos") %>%
        fitBounds(
          lng1 = as.numeric(bbox["xmin"]),
          lat1 = as.numeric(bbox["ymin"]),
          lng2 = as.numeric(bbox["xmax"]),
          lat2 = as.numeric(bbox["ymax"])
        )
    } else {
      # Se não houver dados para esse estado na base
      leafletProxy("mapa_aptos") %>%
        setView(lng = -55, lat = -14, zoom = 4)
    }
  })
  
  
  
}

#------------#------------#------------#------------------------------
#-------------- FINAL LADO SERVIDOR --------------
#------------#------------#------------#------------------------------

# Executa a aplicação Shiny
shinyApp(ui = ui, server = server)
