### Modelos usando pacotes vtreat e xgmboost

# Carregando pacotes necessarios e iniciando seed do projeto
library(caret)
library(vtreat)
library(xgboost)
set.seed(666)

## separação das amostras de treino e teste
sample <- createDataPartition(imoveis_final$price, times = 1, list = F, p = .7)
train_sample <- imoveis_final[sample, ]
test_sample <- imoveis_final[-sample, ]

# funcao para transformar variaveis em fact e escalar
toFactor <- function(df, features) {
  for (feature in features) {
    df[[feature]] <- factor(df[[feature]], exclude = NULL)
    # exclude é importantíssimo pra considerar NA como classe
  }
  return(df)
}

# dfs em factor
train_fact <- toFactor(train_sample, c('rooms_coerc', 'bathrooms_coerc','garage_coerc'))
test_fact <- toFactor(test_sample, c('rooms_coerc', 'bathrooms_coerc','garage_coerc'))

# Montagem dos X e y de ambos os dfs
X_train <- train_fact[-1]
y_train <- train_fact$price
X_test <- test_fact[-1]
y_test <- test_fact$price

## Pre processamento usando vtreat
vars_input <- names(X_train)
treatplan <- designTreatmentsZ(X_train, vars_input)

# df Scoreframe para conseguir nomes das novas variáveis
scoreFrame <- treatplan %>%
  magrittr::use_series(scoreFrame) %>% # forma dplyr de fazer subsetting "$"
  select(varName, origName, code)

# string vector somente com as variáveis requeridas (usa-se, neste caso, só as clean e lev)
novas_vars <- scoreFrame %>%
  filter(code %in% c('clean', 'lev')) %>%
  magrittr::use_series(varName)

# dfs tratados (opção de colocar argumento scale pra testar com modelos sensitivos como knn, lineares em geral)
X_train_treat <- prepare(treatplan, X_train, varRestriction = novas_vars)
X_test_treat <- prepare(treatplan, X_test, varRestriction = novas_vars)

# Modelo XGMBoost

# Algo similar a um tunning grid para achar número ideal de rounds através de cv
xgb_cv <- xgb.cv(data = as.matrix(X_train_treat), 
             label = y_train,
             nrounds = 1000, # n de rounds total
             nfold = 5, # número de cvs
             objective = "reg:linear", # continuos outcome
             eta = 0.1, # taxa de aprendizagem
             max_depth = 10, # maior profundidade das árvores
             early_stopping_rounds = 10, # para após x rounds sem melhora
             verbose = 0    # silêncio
)

# n de round ideal para cada um dos sets (preferência sempre pelo de treino devido ao overfit)
xgb_cv %>% 
  magrittr::use_series(evaluation_log) %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean)) # find the index of min(test_rmse_mean)

# Modelo
imoveis_xgb <- xgboost(data = as.matrix(X_train_treat), 
                          label = y_train,  
                          nrounds = 57,       
                          objective = 'reg:linear', 
                          eta = 0.1,
                          depth = 10,
                          verbose = 0  
)

# Make predictions
comparacao <- data_frame(pred = predict(imoveis_xgb, as.matrix(X_test_treat)), price = y_test)

# Plot predictions (on x axis) vs actual bike rental count
comparacao %>% 
  gather(tipo, valor, price, pred) %>% 
  ggplot(aes(valor, fill = tipo)) +
  geom_density(alpha = .5)

ggplot(comparacao, aes(x = pred, y = price)) + 
  geom_point() + 
  geom_abline()

comparacao %>% 
  mutate(residuals = price - pred) %>%
  summarize(rmse = sqrt(mean(residuals**2)))

## Caret

model2 <- train(price ~ .,
                data = train_preproc,
                method = "lm",
                trControl = control,
                metric = 'RMSE')
