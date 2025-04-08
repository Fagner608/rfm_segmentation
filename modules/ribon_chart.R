library(lubridate)
library(ggsankey)
library(forcats)
library(ggplot2)

# renomeando colunas
data_loop <- result %>%
  select(nome_corretor, data_pagamento_comissao, valor_base) %>%
  rename(
    customer_id = nome_corretor,
    order_date = data_pagamento_comissao,
    revenue = valor_base
  )

data_loop$revenue <- log(data_loop$revenue)

data_loop <- data_loop %>%
  mutate(ano = year(order_date),
         mes = month(order_date),
         ultimo_dia_mes = as.Date(paste(ano, mes, "01", sep = "-")) %m+% months(1) - days(1))





data_loop <- data_loop %>%
  group_by(customer_id, ano, mes) %>%
  summarise(
    recencia = as.integer(ultimo_dia_mes - max(order_date)), 
    frequencia = n(),
    monetary = sum(revenue),
    .groups = 'drop'
  )


data_loop <- data_loop %>%
  arrange(customer_id, ano, mes)

data_loop <- unique(data_loop)

data_loop$monetary_log <- log(data_loop$monetary)


data_loop <- na.omit(data_loop)

# definindo scores
data_loop <- data_loop %>%
  
  group_by(ano, mes) %>%
  
  mutate(
    
    recency_score = ntile(-recencia, 5),
    frequency_score = ntile(frequencia, 5),
    monetary_score = ntile(monetary_log, 5),
    rfm = paste0(recency_score, frequency_score, monetary_score)
    
  )



# Classificando segmentos
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

recency_lower   <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper   <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower  <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper  <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)


n_segments <- length(segment_names)

rfm_score_table_ribon_chart <- data_loop

rfm_score_table_ribon_chart$segment <- NA

for (i in seq_len(n_segments)) {
  
  rfm_score_table_ribon_chart$segment[((rfm_score_table_ribon_chart$recency_score %>% 
                              
                              between(recency_lower[i], recency_upper[i])) & (rfm_score_table_ribon_chart$frequency_score %>% 
                                                                                
                                                                                between(frequency_lower[i], frequency_upper[i])) & 
                             
                             (rfm_score_table_ribon_chart$monetary_score %>% between(monetary_lower[i],
                                                                         
                                                                         monetary_upper[i])) & !rfm_score_table_ribon_chart$segment %in% 
                             
                             segment_names)] <- segment_names[i]
  
}

rfm_score_table_ribon_chart$segment[is.na(rfm_score_table_ribon_chart$segment)] <- "Others"

rfm_score_table_ribon_chart$segment[rfm_score_table_ribon_chart$segment == 1] <- "Others"

