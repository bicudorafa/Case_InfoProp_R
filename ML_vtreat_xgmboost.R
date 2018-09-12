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

# funcao para transformar variaveis em fact
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

## Pre processamento usando vtreat
X_train <- train_sample[-1]
y_trains <- train_sample[1]
vars_input <- names(X_train)
treatplan <- designTreatmentsZ(X_train, vars_input)
