#biblioteca
library(rfm)


#definindo dataset rfm
dataset_rfm <- result %>%
  rename(
    customer_id = nome_corretor,
    order_date = data_pagamento_comissao,
    revenue = valor_base
  )



#dataset_rfm$revenue <- log(dataset_rfm$revenue)


#outliers_revenue <- boxplot.stats(dataset_rfm$revenue)$out

#dataset_rfm <- dataset_rfm[!dataset_rfm$revenue %in% outliers_revenue, ]

#dataset_rfm <- dataset_rfm[!is.na(dataset_rfm$revenue), ]



#dataset_rfm <- na.omit(dataset_rfm)

# definindo data parâmetro para calculo da recência
data_base <- Sys.Date()

colSums(is.na(dataset_rfm))



# transformando dados em tabela RFM
rfm_df <- rfm_table_order(
  data = dataset_rfm,
  customer_id = customer_id,  
  order_date = order_date,
  revenue = revenue,
  #revenue = revenue,
  analysis_date = data_base
)






# Calculando segmentação
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

# Parâmetros para a classificação
recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

# Aplicando segmentação
segment_data_analitcs <- rfm_segment(rfm_df, segment_names, recency_lower, recency_upper,
                            frequency_lower, frequency_upper, monetary_lower, monetary_upper)




#obtendo resumo
segment_overview_segment_chart <- rfm_segment_summary(segment_data_analitcs)

# Adicionando definição e recomentação
segment_overview_segment_chart <- segment_overview_segment_chart %>%
  mutate(
    definicao = 
      case_when(
        segment == "About To Sleep" ~ 'Parceiros que estão prestes a se tornar inativos. Eles podem ter digitado recentemente, mas a frequência de sua digitação está diminuindo, e se não forem incentivados, poderão deixar de digitar novamente.',
        segment == "Need Attention" ~ 'parceiros que são regulares, mas apresentam sinais de desinteresse. Eles podem ter diminuído a frequência de digitação, mas ainda são valiosos',
        segment == "At Risk" ~ 'Parceiros que costumavam digitar regularmente, mas sua recência diminuiu significativamente. Esses parceiros não digitam há um tempo, mas ainda têm um histórico de digitação significativo.',
        segment == "Lost" ~ 'Parceiros que têm um histórico de digitação passado, mas que não digitaram há muito tempo. Eles são parceiros que desapareceram e não interagem mais com a empresa.',
        segment == "Potential Loyalist" ~ 'Parceiros que têm potencial para se tornar leais, mas ainda não atingiram a frequência ou valor monetário dos “Loyal Customers”. Eles digitam com certa regularidade, mas têm potencial para se tornar mais valiosos.',
        segment == "Others" ~ 'Este é um segmento genérico que abrange parceiros que não se encaixam bem em nenhum dos outros grupos. Pode incluir parceiros que não se encaixam em termos de recência, frequência ou valor monetário de forma consistente.',
        segment == "Loyal Customers" ~ 'Parceiros que digitam frequentemente, mas não geram tanto quanto os “Champions”. Eles são fiéis à empresa, mas não estão no topo em termos de volume de receita.',
        segment == "Champions" ~ 'São os melhores parceiros. Eles digitam com frequência, geram muito dinheiro e têm um bom histórico de digitação recentes. Esses parceiros são altamente leais.'
      )
    
  )


segment_overview_segment_chart <- segment_overview_segment_chart %>%
  mutate(
    recomendacao = 
      case_when(
        segment == "About To Sleep" ~ 'Ação de reengajamento urgente, como promoções especiais ou lembretes para manter o parceiro engajado.',
        segment == "Need Attention" ~ 'Interações personalizadas, como e-mails ou ofertas, para lembrar os parceiros do valor da empresa e incentivá-los a continuar comprando.',
        segment == "At Risk" ~ 'Oferecer promoções personalizadas ou descontos para tentar trazê-los de volta antes que se tornem inativos completamente.',
        segment == "Lost" ~ 'Estratégias de reconquista, como campanhas de reengajamento ou ofertas direcionadas.',
        segment == "Potential Loyalist" ~ 'Estímulos para incentivá-los a digitar mais frequentemente, como recompensas de fidelidade ou incentivos adicionais.',
        segment == "Others" ~ 'Análise mais aprofundada para tentar identificar o comportamento desses parceiros e encontrar maneiras de engajá-los.',
        segment == "Loyal Customers" ~ 'Propor ações de fidelização, como comissões especiais para grandes lotes de digitação ou incentivos para aumentar o valor das propostas.',
        segment == "Champions" ~ 'Focar em programas de fidelidade e recompensas para manter esse grupo de parceiros feliz e incentivá-los a continuar comprando.'
      )
    
  )


