# Carregando pacote necessario e iniciando seed do projeto
library(caret)
library(mlbench)
set.seed(666)

# separação das amostras de treino e teste
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

train_fact <- toFactor(train_sample, c('rooms_coerc', 'bathrooms_coerc','garage_coerc'))
test_fact <- toFactor(test_sample, c('rooms_coerc', 'bathrooms_coerc','garage_coerc'))

# Pre processamento dos sets

# knnImpute é extremamente lento, só é usável em outas situações. Optei pelo median
preprocessParams_train <- preProcess(train_fact, method=c("medianImpute", "center", "scale"))
preprocessParams_test <- preProcess(test_fact, method=c("medianImpute", "center", "scale"))

# Sets prontos para uso
train_preproc <- predict(preprocessParams_train, train_fact)
test_preproc <- predict(preprocessParams_test, test_fact)

# Modelos

# 1 - variáveis iniciais,
## valre ressaltar como o cv é necessário com um df deste tamanho: é muito mais difícil de inverter a matriz, r não tem memória para
#model1 <- train(price ~ .,
#                data = train_preproc_bench,
#                method = "lm",
#                trControl = control,
#                metric = 'RMSE')
# desativado pela alta dificuldade em conseguir amostras de treino e teste com todas os levels nas categóricas

#RFE
control_rfe <- rfeControl(functions=lmFuncs, method="cv", number=10)
results <- rfe(price ~ ., data = train_preproc, sizes=c(1:15), rfeControl=control_rfe)
varImp(results, scale=FALSE)
plot(results, type=c("g", "o"))


# Comparação entre modelo com features originais x  + criadas

# Controle para todos os modelos
control <- trainControl(method="cv", number=10)

# modelo com as originais
model1 <- train(price ~ 
                  area +
                  condominium_fee +
                  iptu +
                  rooms_coerc +
                  bathrooms_coerc +
                  garage_coerc,
                data = train_preproc,
                method = "lm",
                trControl = control,
                metric = 'RMSE')

# modelo com as features criadas
model2 <- train(price ~ .,
                data = train_preproc,
                method = "lm",
                trControl = control,
                metric = 'RMSE')

# Comparação gráfica dos 2 modelos
model_list <- list(Originais = model1, Originais_eNovas = model2)
resamples <- resamples(model_list)
dotplot(resamples, metric = 'RMSE')

# score dos modelos
pred1 <- predict(model1, test_preproc)
pred2 <- predict(model2, test_preproc)

test_Score1 <-data.frame(obs = test_preproc$price, pred= pred1)
test_Score2 <-data.frame(obs = test_preproc$price, pred= pred2)

defaultSummary(test_Score1)
defaultSummary(test_Score2)