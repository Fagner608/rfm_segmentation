options(warn = -1)

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

#carrega dados - carregar a partir do postgre
# definir conector
library(RPostgres)
library(DBI)

readRenviron("./.Renviron")

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = Sys.getenv('DB_NAME'), 
                 host = Sys.getenv('DB_HOST'), 
                 port = 5432, 
                 user = Sys.getenv('DB_USER'), 
                 password = Sys.getenv('DB_PASSWORD'))

query <- '
    select ct."data_pagamento_comissao", ct."valor_base", co."nome_corretor", co."funcionario_cidade", co."uf", co."lat", co."lon"
    
    from public.contrato ct
    left join cliente_multiloja climul on climul."id_cliente_multiloja" = ct."id_cliente_multiloja"
    left join cliente_corretor clicor on clicor."id_cliente_corretor" = climul."id_cliente_corretor"
    left join corretor co on co."id_corretor" = clicor."id_corretor"
    left join public.banco_orgao banorg on banorg."id_banco_orgao" = ct."id_banco_orgao"
    left join public.tabela_banco tabban on tabban."id_tabela_banco" = banorg."id_tabela_banco"
    where tabban."id_banco" = 3
    and data_pagamento_comissao between \'2024-06-01\' and \'2025-03-31\'
'



result <- dbGetQuery(con, query)

dbDisconnect(con)

