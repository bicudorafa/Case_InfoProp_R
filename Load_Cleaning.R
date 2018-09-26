### Abertura e inspecao do df

# Pacotes necessários
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
#library(magrittr)

# Abertura do arquivo
imoveis <- as.tbl(read.csv2('Crawler_vivareal_venda_2017_desafio.csv', stringsAsFactors = F, encoding = "UTF-8",
                            na.strings = ""))
glimpse(imoveis)

### Data celaning
imoveis_clean  <-  imoveis

# data
names(imoveis_clean) <- str_replace(names(imoveis_clean), 'X.U.FEFF.date', 'Data')
imoveis_clean$Data <- as.Date(imoveis$X.U.FEFF.date, format = '%d/%m/%Y')
year(imoveis_clean$Data) <- year(imoveis_clean$Data)+2000

# lat e long
imoveis_clean$longitude <- gsub(".", "", imoveis$longitude, fixed = TRUE) %>%
  sub("(\\d{2})", "\\1\\.", .) %>% 
  as.numeric()
imoveis_clean$latitude <- gsub(".", "", imoveis$latitude, fixed = TRUE) %>%
  sub("(\\d{2})", "\\1\\.", .) %>% 
  as.numeric()

# price_by_sqm
imoveis_clean$price_by_sqm <- as.numeric(imoveis_clean$price_by_sqm)

# condominium_fee e iptu
imoveis_clean$condominium_fee <- str_replace_all(imoveis$condominium_fee, 'R\\$', '') %>% 
  str_replace_all('[:punct:]', '') %>% 
  as.numeric()

imoveis_clean$iptu <- str_replace_all(imoveis$iptu, 'R\\$', '') %>% 
  str_replace_all('[:punct:]', '') %>% 
  as.numeric()

# visualizacao do novo df
glimpse(imoveis_clean)