## Engenharia de Features

# Pacotes necessários
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(tidyr)

# agregação dos valores para não causar overfitting pela baixa quantidade de valores
roomsBathGarage <- imoveis_clean %>% 
  select(c(rooms, bathrooms, garage, property_d)) %>% 
  mutate(rooms_coerc = ifelse(rooms > 4, 4, rooms),
         bathrooms_coerc = ifelse(bathrooms > 6, 6, bathrooms),
         garage_coerc = ifelse(garage > 5, 5, garage)) %>% 
  select(-c(rooms, bathrooms, garage))

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
  left_join(roomsBathGarage, by = c('property_d')) %>% 
  left_join(vars_rua) %>% 
  left_join(vars_condo) %>% 
  left_join(vars_agente) %>% 
  select(-c(Data, rua, numero, bairro, price_by_sqm, condominio, rooms, bathrooms, garage,agent, agent_number, 
            latitude, longitude, property_d, url))
glimpse(imoveis_final)