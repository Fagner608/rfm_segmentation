options(warn = -1)
library(plotly)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsankey)
library(forcats)
library(treemapify)
library(rfm)
library(maps)

## funções auxiliares
source("./modules/funcoes_auxiliares.R")

## Acesso aos dados
source("./modules/dataWrangling.R")

## Transformação dos dados
source("./modules/rfm.R")

## ribon-chart
source("./modules/ribon_chart.R")




#carregando dados

data_dir <- "data"

segment_chart <- segment_overview_segment_chart
ribbon_chart <- rfm_score_table_ribon_chart # usado na função viz_ribon
data_chart <- segment_data_analitcs
data_map <- data_chart
data_map[, c('customer_id', 'segment', 'lat', 'lon')] # usado na função viz_map



# Definir um vetor de cores fixas mapeado para os segmentos
segment_labels <- c(
  "Loyal Customers", 
  "At Risk", 
  "About To Sleep", 
  "Need Attention", 
  "Lost", 
  "Potential Loyalist", 
  "Champions", 
  "Others", 
  NA
)


fixed_colors <- c(
  "#66c2a5",  # Loyal Customers
  "#fc8d62",  # At Risk
  "#cccccc",  # About To Sleep
  "#8da0cb",  # Need Attention
  "#FF7F7F",  # Lost
  "#a6d854",  # Potential Loyalist
  "#E6A8D7",  # Champions
  "#e5c494",  # Others
  "#b3b3b3"   # NA (sem segmento)
)

segment_color_map <- setNames(fixed_colors, segment_labels)

viz_map <- function(data_map, label = NULL){
  
  if(!is.null(label)){data_map  = data_map %>% filter(segment == label)}
  
  brasil <- map_data("world", region = "Brazil")
  
  grafico_map <- ggplot() +
    geom_polygon(data = brasil, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
    
    geom_point(data = data_map, aes(x = lon, y = lat, color = as.factor(segment)), size = 2) + 
    
    scale_color_manual(values = segment_color_map) +
    
    labs(title = "Distribuição dos parceiros com RFM Score",
         x = "",
         y = "",
         color = "RFM Score") +
    
    theme_minimal() +
    theme(axis.text = element_blank(), axis.ticks = element_blank())
  
  
  ggplotly(grafico_map, tooltip = 'all')
  
}


plot_partiner <- function(parceiro_dados, partiner){
  
  
  parceiro <- partiner
  
  data_wide <- parceiro_dados[grepl(parceiro, parceiro_dados$customer_id, ignore.case = T), ]
  
  data_wide <- data_wide[, c("customer_id", 'mes', 'recencia', "rfm", "segment")]
  #data_wide <- data_wide %>% filter(!mes %in% c(1,2))
  
  data_wide <- data_wide %>% 
    group_by(customer_id) %>%
    mutate(
      total_index = n(),
      segmento_inicial = first(segment)
    )
  
  data_wide <- data_wide %>%
    pivot_wider(
      id_cols = c(customer_id, total_index, segmento_inicial),
      names_from = mes,
      values_from = segment,
      names_prefix = 'segment'
    )
  
  data_wide <- data_wide[, !names(data_wide) %in% c('total_index', 'segmento_inicial', 'customer_id')]
  
  
  colunas_desejadas <- paste0('segment', 6:12)
  
  colunas_presentes <- colunas_desejadas[colunas_desejadas %in% names(data_wide)]
  
  ## AJUSTAR ##
  # data_wide_log <- data_wide %>%
  #    make_long("segment6", "segment7", "segment8", "segment9", "segment10", "segment11", "segment12")
  
  #passando argumento de forma dinamica
  data_wide_log <- do.call(make_long, c(list(data_wide), colunas_presentes))
  
  
  viz <- ggplot(data_wide_log, aes(x = x, 
                                   next_x = next_x, 
                                   node = node, 
                                   next_node = next_node,
                                   fill = node)) +
    geom_sankey() +
    theme_sankey(base_size = 12) +  # Apenas um base_size
    scale_fill_manual(values = segment_color_map) +
    scale_x_discrete(labels = function(x) gsub('segment', "", x)) +
    labs(fill = "Segmentos", title = "Trajetória do parceiro", x = "Mês") + 
    theme(
      plot.title = element_text(size = 14),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.margin = margin(50, 50 ,50 ,50)
    )
  
  ggplotly(viz, tooltip = c("text", "size"))
  
  
}


viz_ribbon <- function(data, label = NULL, segment = NULL){
  rfm_score_table = data
  data_wide <- rfm_score_table[, c("customer_id", 'mes', 'recencia', "rfm", "segment")]
  #data_wide <- data_wide %>% filter(!mes %in% c(1,2))
  
  
  
  data_wide <- data_wide %>% 
    group_by(customer_id) %>%
    mutate(
      total_index = n(),
      segmento_inicial = first(segment)
    )
  
  data_wide <- data_wide %>%
    pivot_wider(
      id_cols = c(customer_id, total_index, segmento_inicial),
      names_from = mes,
      values_from = segment,
      names_prefix = 'segment'
    )
  
  data_wide <- data_wide[, !names(data_wide) %in% c('total_index', 'segmento_inicial', 'customer_id')]
  
  
  if (!is.null(segment) && !is.null(label)) {
    label <- as.character(label)
    
    data_wide <- data_wide %>%
      filter(segment6 %in% c(label))
    
  } else if (!is.null(segment) && is.null(label)) {
    stop("Informe o label do segmento")
  } else if (is.null(segment) && !is.null(label)) {
    stop("Para segmentar, deve informar True no argumento segment")
  }
  
  
  
  
  data_wide <- data_wide %>% arrange(segment6, segment7, segment8, segment9, segment10, segment11, segment12, segment1, segment2, segment3)
  
  data_wide_log <- data_wide %>%
    make_long("segment6", "segment7", "segment8", "segment9", "segment10", "segment11", "segment12", 'segment1', 'segment2', 'segment3')
  
  
  
  
  segment_order <- c(
    "About To Sleep", 
    "Need Attention", 
    "At Risk", 
    NA, 
    "Lost", 
    "Potential Loyalist", 
    "Others", 
    "Loyal Customers", 
    "Champions"
  )
  
  data_wide_log <- data_wide_log %>%
    mutate(node = factor(node, levels = segment_order, exclude = NULL) %>% 
             fct_explicit_na(na_level = "No Segment"),
           next_node = factor(next_node, levels = segment_order, exclude = NULL) %>%
             fct_explicit_na(na_level = "No Segment"))
  
  viz <- ggplot(data_wide_log, aes(x = x, 
                                   next_x = next_x, 
                                   node = node, 
                                   next_node = next_node,
                                   fill = node)) +
    geom_sankey() +
    theme_sankey(base_size = 12) +  # Apenas um base_size
    scale_fill_manual(values = segment_color_map) +
    scale_x_discrete(labels = function(x) gsub('segment', "", x)) +
    labs(fill = "Segmentos", title = paste("Fluxo dos clientes que iniciaram com status: ", label), x = "Mês") + 
    theme(
      plot.title = element_text(size = 14),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
  
  ggplotly(viz, tooltip = c("text", "size"))
}


ui <- fluidPage(
  # Application title
  titlePanel("Acompanhamento de segmentação"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,  
      selectInput("segmento",
                  "Escolha o segmento que deseja filtrar:",
                  choices = c(
                    "About To Sleep", 
                    "Need Attention", 
                    "At Risk",
                    "Lost", 
                    "Potential Loyalist", 
                    "Others", 
                    "Loyal Customers", 
                    "Champions"
                  ),
                  selected = 'Champions'
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Visão Geral",
                 fluidRow(
                   column(6, plotlyOutput("treemap_plot")), 
                   column(6, plotlyOutput("viz_map")) 
                 ),
                 fluidRow(
                   column(12, plotlyOutput("viz_ribbon")) 
                 ),
                 fluidRow(
                   column(12, textOutput('definicao_text', container = pre)), 
                   column(12, textOutput("recomendacao_text", container = pre))  
                   
                 )
        ),
        tabPanel("Parceiros",
                 fluidRow(
                   textInput("partiner", 'Insira o nome do parceiro: ', value = ""),
                   column(3, tableOutput("parceiros")),
                   column(2, tableOutput("data")),
                   column(12, plotlyOutput('fluxo_segmento'))
                   
                 ))
      )
      
      
    )
    
    
  )
)

server <- function(input, output) {
  label_choice <- reactive({input$segmento})
  partiner_choice <- reactive({ input$partiner })
  
  
  
  output$treemap_plot <- renderPlotly({
    plot_ly(
      type = "treemap",
      labels = segment_chart$segment,
      parents = rep("", nrow(segment_chart)),
      values = segment_chart$customers,
      #text = segment_chart$recomendacao,  # Adiciona a recomendação como texto
      hoverinfo = "label+percent entry",  # Exibe o label e a porcentagem no hover
      marker = list(
        colors = segment_color_map[segment_chart$segment]
      )
    ) %>% layout(title = list(text = "Distribuição dos perceiros por segmento"))
  })
  
  
  
  output$viz_map <- renderPlotly(({
    
    viz_map(data_map = data_map, label = label_choice())
    
  }))
  
  
  
  output$recomendacao_text <- renderText({
    label = label_choice()
    definicao <- segment_chart[segment_chart$segment == label, 'recomendacao']
    texto <- paste(paste("Recomenação: ", label), definicao, sep = "\n")
    return(texto)
    
  })
  
  
  output$definicao_text <- renderText({
    
    label = label_choice()
    definicao <- segment_chart[segment_chart$segment == label, 'definicao']
    texto <- paste(paste("Definicao: ", label), definicao, sep = "\n")
    return(texto)
    
    
    
  })
  #   
  
  output$fluxo_segmento <- renderPlotly({
    
    req(partiner_choice())
    
    plot_partiner(parceiro_dados = ribbon_chart,
                  partiner = partiner_choice())
    
    
  })
  
  
  
  output$parceiros <- renderTable({
    
    req(partiner_choice())
    data <- ribbon_chart
    unique(data.frame(Nomes = data[grepl(partiner_choice(), data$customer_id, ignore.case = T), 'customer_id']))
    
    
  })
  
  
  output$data <- renderTable({
    
    req(partiner_choice())
    resume <- data_map[grepl(partiner_choice(), data_map$customer_id, ignore.case = T), ]
    resume %>% 
      select(recency_days) %>%
      rename(
        dias_sem_produzir = recency_days
      )
    
    
  })
  
  output$viz_ribbon <- renderPlotly({
    
    viz_ribbon(data = ribbon_chart, 
               label = label_choice(), 
               segment = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

