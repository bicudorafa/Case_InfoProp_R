### EDA

# Pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())


## Basicas
summary(imoveis_clean)
sapply(imoveis_clean, function(x) length(unique(x)))

## Visualizacao interativa
library(leaflet)

leaflet(data = imoveis_clean) %>% addTiles() %>% 
  addMarkers(~longitude, ~latitude, popup = ~as.character(property_d), 
             clusterOptions = markerClusterOptions()
  )

# visualizacao das variaveis continuas
cont_vars <- c('area', 'iptu','condominium_fee')
plots_cont <- list()
for (var in cont_vars) {
  plots_cont[[var]] <- ggplot(imoveis_clean, aes_string(log(imoveis_clean[[var]]), log(imoveis_clean[['price']]))) + 
    geom_point() +
    geom_smooth(method = lm, se = F) + 
    ggtitle(paste(var, 'x price')) +
    theme_minimal()
  print(plots_cont[[var]])
}

# visualizacao das variaveis factor
fact_vars <- c('bairro', 'rooms', 'bathrooms','garage')

plots_fact_c <- list()
for (var in fact_vars) {
  plots_fact_c[[var]] <- ggplot(imoveis_clean, aes_string(x = as.factor(imoveis_clean[[var]]))) + 
    geom_bar(stat = 'count') +
    ggtitle(paste('Total Observations in ',var)) +
    theme_minimal()
  print(plots_fact_c[[var]])
}

### plots importantes não feitos nesta análise: há correlação entre aparições muito grandes de valores NA? Sâo sistêmicos?

## Investigacao outliers
# imoveis_clean %>% filter(property_d %in% c(53512644, 83712188, 80671730))
imoveis_clean %>% filter(log(area) > 10 | log(price) < 10 | log(condominium_fee) > 10)
imoveis_clean %>% filter(rooms > 4) %>% arrange(desc(rooms))
imoveis_clean %>% filter(bathrooms > 6) %>% arrange(desc(bathrooms))
imoveis_clean %>% filter(garage > 5) %>% arrange(desc(garage))

# Como nao e possivel checar a fonte com precisao, deixarei os valores a menos que muito bizarros

# outliers de area e price: informações muito discrepantes em relação à realidade. Provavelmente, erros de digitação
imoveis_clean$area[which(imoveis$area == 30700)] <- NA
imoveis_clean <- imoveis_clean %>% filter(!(price == 18000))
imoveis_clean$condominium_fee[which(imoveis$condominium_fee == 460000)] <- NA

# por estarem completamente fora do espaço geografico do resto da amostra, colocarei seus bairros como NA
imoveis_clean$bairro[which(imoveis$property_d %in% c(53512644, 83712188, 80671730))] <- NA

# o apartamento com mais de 54 banheiros parece muito fora da media em relacao ao seu preco e area
imoveis_clean$bathrooms[which(imoveis$bathrooms == 54)] <- NA