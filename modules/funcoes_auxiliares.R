#returns moda
moda <- function(data){
  
  h <- hist(data, plot = F, breaks = sqrt(length(data)))
  idx <- which.max(h$counts)
  return(h$mids[idx])  
}


# drop outliers with iqr
drop_outliers <- function(dataset){
  
  q1 <- quantile(dados$comissao_v8, .25)
  q3 <- quantile(dados$comissao_v8, .75)
  iqr <- IQR(dados$comissao_v8)
  
  superior <- q3 + 1.5 * iqr
  inferior <- q1 - 1.5 * iqr
  
  return(dados[dados$comissao_v8 >= inferior & dados$comissao_v8 <= superior, ])
}


# faz o rfm_df
rfm_segment_customized <- function (data, segment_names = NULL, recency_lower = NULL, recency_upper = NULL, 
                                    frequency_lower = NULL, frequency_upper = NULL, monetary_lower = NULL, 
                                    monetary_upper = NULL) 
{
  customer_id <- NULL
  
  segment <- NULL
  
  #rfm_score_table <- data$rfm
  
  rfm_score_table <- data
  
  rfm_score_table$segment <- 1
  
  n_segments <- length(segment_names)
  
  for (i in seq_len(n_segments)) {
    
    rfm_score_table$segment[((rfm_score_table$recency_score %>% 
                                
                                between(recency_lower[i], recency_upper[i])) & (rfm_score_table$frequency_score %>% 
                                                                                  
                                                                                  between(frequency_lower[i], frequency_upper[i])) & 
                               
                               (rfm_score_table$monetary_score %>% between(monetary_lower[i],
                                                                           
                                                                           monetary_upper[i])) & !rfm_score_table$segment %in% 
                               
                               segment_names)] <- segment_names[i]
    
  }
  
  rfm_score_table$segment[is.na(rfm_score_table$segment)] <- "Others"
  
  rfm_score_table$segment[rfm_score_table$segment == 1] <- "Others"
  
  rfm_score_table[c("customer_id", "segment", "recencia", 
                    "frequencia", "monetary", "monetary_log", 
                    "recency_score", "frequency_score", 
                    "monetary_score", "rfm" )]
  
  
  rfm_score_table %>% select(c("customer_id", "segment", "recencia", 
                               "frequencia", "monetary", "monetary_log", 
                               "recency_score", "frequency_score", 
                               "monetary_score", "rfm" ), everything())
  
  
  
  

}


# faz o resumo
rfm_segment_summary_manual <- function (segments) 
{
  segments %>% group_by(segment) %>% summarise(customers = n(), 
                                               orders = sum(frequencia), revenue = sum(monetary_log)) %>% 
    mutate(aov = round((revenue/orders), 2))
}



# Ribon chart
viz_ribbon <- function(data, label = NULL, segment = NULL, segment_color_map){
  rfm_score_table = data
  data_wide <- rfm_score_table[, c("customer_id", 'mes', 'recencia', "rfm", "segment")]
  data_wide <- data_wide %>% filter(!mes %in% c(1,2))
  
  
  
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
  
  data_wide <- data_wide %>% arrange(segment6, segment7, segment8, segment9, segment10, segment11, segment12)
  
  data_wide_log <- data_wide %>%
    make_long("segment6", "segment7", "segment8", "segment9", "segment10", "segment11", "segment12")
  
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

