## Machine Learning usando pacote MLR

# Carregando Pacotes Necessários
library(dplyr)
library(tidyr)
library(mlr)
set.seed(666)
# Função mais eficiente para featurização
imoveis_fact <- imoveis_final %>%
  mutate_at(
    .vars = vars('rooms_coerc', 'bathrooms_coerc','garage_coerc'),
    .funs = funs(as.factor(.))
  )

glimpse(imoveis_fact)

## Pacote MLR

# Imputador dos valores missing
imp <- impute(
  imoveis_fact,
  classes = list(
    factor = imputeMode(),
    integer = imputeMean(),
    numeric = imputeMean()
  )
)
imoveis_imp <- imp$data

# Sumarizando as colunas
summarizeColumns(imoveis_imp) %>%
  knitr::kable(digits = 2) # gerador de tabelas muito prático do knitr

# Normalização
imoveis_norm <- normalizeFeatures(imoveis_imp, target = "price")

# One hot encoding
imoveis_preProc <- createDummyFeatures(
  imoveis_norm, target = "price",
  cols = c(
    "rooms_coerc",
    "bathrooms_coerc",
    "garage_coerc"
  )
)

# Criação do Task para Regressão (objeto usado nas operações do pacote)
task <- makeRegrTask(id = 'Imoveis_SP', data = imoveis_preProc, target = 'price')

# train test split (holdout é o método de resample de separar em train/test)
holdout <- makeResampleInstance('Holdout', task)
task_train <- subsetTask(task, holdout$train.inds[[1]])
task_test <- subsetTask(task, holdout$test.inds[[1]])

## Modelo linear básico

# modelo padrão linear
regr.lm <- makeLearner(id = 'lm', 'regr.lm')

# treinamento modelo
lm <- train(regr.lm, task_train)

# predição nos dados de teste
pred_lm <- predict(lm, task_test)
performance(pred_lm, measures = list(rmse, mae))

## XGBoost

# Criação do modelo simples
xgb_model <- makeLearner("regr.xgboost")

# Grid de alguns parâmetros
xgb_params <- makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 2000),
  # number of splits in each tree
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
  makeNumericParam("eta", lower = .1, upper = .5),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x)
)

# Controle da tunagem randômica
control <- makeTuneControlRandom(maxit = 1) # maxit represente tempo máximo de 1 min para iterações

# Plano de resampling
resample_desc <- makeResampleDesc("CV", iters = 5)

# tunador dos hiper parâmetros
tuned_params <- tuneParams(
  learner = xgb_model,
  task = task_train,
  resampling = resample_desc,
  par.set = xgb_params,
  control = control
)

# Modelo tunado
xgb_tuned_model <- setHyperPars(
  learner = xgb_model,
  par.vals = tuned_params$x
)

# Treinamento
XGBoost <- train(xgb_tuned_model, task_train)

# predição nos dados de teste
pred_xgb <- predict(XGBoost, task_test)
performance(pred_xgb, measures = list(rmse, mae))