### Abertura e inspecao do df

library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(tidyr)
#library(magrittr)

#
imoveis <- as.tbl(read.csv2('Crawler_vivareal_venda_2017_desafio.csv', stringsAsFactors = F, encoding = "UTF-8",
                            na.strings = ""))
glimpse(imoveis)

### Data celaning
imoveis_clean = imoveis

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

### EDA

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
  plots_cont[[i]] <- ggplot(imoveis_clean, aes_string(log(imoveis_clean[[var]]), log(imoveis_clean[['price']]))) + 
    geom_point() +
    geom_smooth(method = lm, se = F) + 
    ggtitle(paste(var, 'x price')) +
    theme_minimal()
  print(plots_cont[[i]])
}

# visualizacao das variaveis factor
fact_vars <- c('bairro', 'rooms', 'bathrooms','garage')
plots_fact <- list()
for (var in fact_vars) {
  plots_fact[[i]] <- ggplot(imoveis_clean, aes_string(x = as.factor(imoveis_clean[[var]]), log(imoveis_clean[['price']]))) + 
    geom_point() +
    geom_boxplot() + 
    ggtitle(paste(var, 'x price')) +
    theme_minimal()
  print(plots_fact[[i]])
}

## Investigacao outliers
View(imoveis_clean %>% filter(property_d %in% c(53512644, 83712188, 80671730)))
View(imoveis_clean %>% filter(log(area) > 10 | log(price) < 10))
View(imoveis_clean %>% filter(bathrooms > 9))
View(imoveis_clean %>% filter(garage > 9))

# Como nao e possivel checar a fonte com precisao, deixarei os valores a menos que muito bizarros

# outliers de area e price: embora muito diferentes dos demais, não há meios de investigar melhor para checar erro

# por estarem completamente fora do espaço geografico do resto da amostra, colocarei seus bairros como NA
imoveis_clean$bairro[which(imoveis$property_d %in% c(53512644, 83712188, 80671730))] <- NA

# o apartamento com mais de 54 banheiros parece muito fora da media em relacao ao seu preco e area
imoveis_clean$bathrooms[which(imoveis$bathrooms == 54)] <- NA

## Procurar agregacoes uteis em rua, condominio e agente

# criacao dos dfs comas supostas novas variaveis de interesse
imoveis_clean_rua <- imoveis_clean %>% 
  group_by(rua) %>%
  summarise(total_a = n(),
            media_p = mean(price_by_sqm, na.rm = T)) %>%
  filter(!(is.na(rua)))# %>% 
# mutate(QTL_t = case_when(
#total_a <= quantile(total_a, probs = .2) ~ '1o',
#total_a <= quantile(total_a, probs = .4) ~ '2o',
#  total_a <= quantile(total_a, probs = .6) ~ 'baixo',
#   total_a <= quantile(total_a, probs = .9) ~ 'medio',
#   TRUE ~ 'alto'
# ),
# como media_p possui apenas as extremidades diferentes, aqui dividirei em mais quantis
# QTL_m = case_when(
#media_p <= quantile(media_p, probs = (1/6)) ~ '1o',
#media_p <= quantile(media_p, probs = (2/6)) ~ '2o',
#media_p <= quantile(media_p, probs = (3/6)) ~ '3o',
# media_p <= quantile(media_p, probs = .90) ~ 'baixa',
#  media_p <= quantile(media_p, probs = .95) ~ 'media',
# TRUE ~ 'alta'
# ))

imoveis_clean_condominio <- imoveis_clean %>% 
  group_by(condominio) %>%
  summarise(total_a = n(),
            media_p = mean(price_by_sqm, na.rm = T)) %>%
  filter(!(is.na(condominio))) #%>% 
#mutate(QTL_t = case_when(
#total_a <= quantile(total_a, probs = .2) ~ '1o',
#total_a <= quantile(total_a, probs = .4) ~ '2o',
#total_a <= quantile(total_a, probs = .6) ~ '3o',
#  total_a <= quantile(total_a, probs = .90) ~ 'baixo',
#  TRUE ~ 'alto'
# ),
# dispersAo deste é muito uniforme, então focarei apenas nos extremos
# QTL_m = case_when(
#media_p <= quantile(media_p, probs = .10) ~ 'baixo',
# media_p <= quantile(media_p, probs = .975) ~ 'baixa',
# media_p <= quantile(media_p, probs = .99) ~ 'media',
# TRUE ~ 'alta'
# ))

# como ha agencias sem nenhuma entrada, filtro antes de achar as variaves finais
imoveis_clean_agent <- imoveis_clean %>% 
  group_by(agent) %>%
  summarise(total_a = n(),
            media_p = mean(price_by_sqm, na.rm = T)) %>%
  filter(!(is.na(agent))) %>% 
  filter(!(is.na(media_p))) #%>% 
#mutate(QTL_t = case_when(
#total_a <= quantile(total_a, probs = .2) ~ '1o',
#total_a <= quantile(total_a, probs = .4) ~ '2o',
#total_a <= quantile(total_a, probs = .8) ~ 'baixo',
#total_a <= quantile(total_a, probs = .90) ~ 'baixo',
#TRUE ~ 'alto'
#),
# dispersAo deste é muito uniforme, então focarei apenas nos extremos
# QTL_m = case_when(
# media_p <= quantile(media_p, probs = .975) ~ 'baixa',
# media_p <= quantile(media_p, probs = .99) ~ 'media',
# TRUE ~ 'alta'
#))


# plot total_a
var_t <- list(imoveis_clean_rua %>% arrange(desc(total_a)), 
              imoveis_clean_condominio %>% arrange(desc(total_a)), 
              imoveis_clean_agent %>% arrange(desc(total_a)))

plots_total <- list()

for (i in 1:length(var_t)){
  plots_total[[i]] <- ggplot(var_t[[i]], aes(x = factor(var_t[[i]][[1]], levels = var_t[[i]][[1]]), 
                                             y = total_a)) + 
    geom_bar(stat="identity", width=.5) +
    coord_flip() +
    labs(title=paste('Anúncios x ', names(var_t[[i]])[[1]]), 
         x = names(names(var_t[[i]])[[1]]),
         y = 'Anúncios') 
  
  print(plots_total[[i]])
}

# plot da media_p 
var_m <- list(imoveis_clean_rua %>% arrange(desc(media_p)), 
              imoveis_clean_condominio %>% arrange(desc(media_p)), 
              imoveis_clean_agent %>% arrange(desc(media_p)))

plots_media <- list()

for (i in 1:length(var_m)){
  plots_media[[i]] <- ggplot(var_m[[i]], aes(x = factor(var_m[[i]][[1]], levels = var_m[[i]][[1]]), 
                                             y = media_p)) + 
    geom_bar(stat="identity", width=.5) +
    coord_flip() +
    labs(title=paste('Media x ', names(var_m[[i]])[[1]]), 
         x = names(names(var_m[[i]])[[1]]),
         y = 'Media') 
  
  print(plots_media[[i]])
}

# variaveis criadas
vars_rua <- imoveis_clean_rua %>% select(rua, anuncio_rua = total_a, valores_rua = media_p) 
vars_condo <- imoveis_clean_condominio %>% select(condominio, anuncio_condo = total_a, valores_condo = media_p)  
vars_agente <- imoveis_clean_agent %>% select(agent, anuncio_agente = total_a, valores_agente = media_p) 

### Modelo preditivo

# df final para analise e com variaveis novas
imoveis_final <- imoveis_clean %>% 
  left_join(vars_rua) %>% 
  left_join(vars_condo) %>% 
  left_join(vars_agente) %>% 
  select(-c(Data, rua, numero, bairro, price_by_sqm, condominio, agent, agent_number, latitude, longitude, property_d, url))

## dados de teste e treino

# Carregando pacote necessario e iniciando seed do projeto
library(caret)
library(mlbench)
set.seed(666)

sample <- createDataPartition(imoveis_final$price, times = 1, list = F, p = .8)
train_sample <- imoveis_final[sample, ]
test_sample <- imoveis_final[-sample, ]

# funcao para transformar variaveis em fact
toFactor <- function(df, features) {
  for (feature in features) {
    df[[feature]] <- factor(df[[feature]], exclude = NULL)
  }
  return(df)
}

fact_vars_final <- c('rooms', 'bathrooms','garage')
train_fact <- toFactor(train_sample, fact_vars_final)
test_fact <- toFactor(test_sample, fact_vars_final)

# calculate the pre-process parameters from the dataset
preprocessParams_train <- preProcess(train_fact, method=c("knnImpute", "center", "scale"))
preprocessParams_test <- preProcess(test_fact, method=c("knnImpute", "center", "scale"))

# transform the dataset using the parameters
train_preproc <- predict(preprocessParams_train, train_fact)
test_preproc <- predict(preprocessParams_test, test_fact)


control <- trainControl(method="cv", number=10)

model1 <- train(price ~ .,
                data = train_preproc,
                method = "lm",
                trControl = control,
                metric = 'RMSE')

plot(varImp(model1, scale=FALSE))

control_rfe <- rfeControl(functions=lmFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(price ~ ., data = train_preproc, sizes=c(1:15), rfeControl=control_rfe)
# summarize the results
varImp(results, scale=FALSE)
plot(results, type=c("g", "o"))

# score modelo
pred <- predict(model1, test_preproc)

modelvalues<-data.frame(obs = test_preproc$price, pred= pred)

defaultSummary(modelvalues)
