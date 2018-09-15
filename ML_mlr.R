## Machine Learning usando pacote MLR

# Carregando Pacotes Necessários
library(dplyr)
library(tidyr)
library(mlr)

# Função mais eficiente para featurização
imoveis_fact <- imoveis_final %>%
  mutate_at(
    .vars = vars('rooms_coerc', 'bathrooms_coerc','garage_coerc'),
    .funs = funs(as.factor(.))
  )

glimpse(imoveis_fact)

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