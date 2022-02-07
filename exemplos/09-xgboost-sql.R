library(tidymodels)
library(tidyverse)
library(tidypredict)

# dados ----------------------------------------------------
data <- tribble(
  ~dose, ~curou,
  2, "não curou",
  8, "curou",
  12, "curou",
  16, "não curou"
) %>%
  mutate(
    curou = factor(curou)
  )

# especificacao do modelo ---------------------------------
# mapa dos hiperparâmetros:
#
# tree_depth = tree_depth
# loss_reduction = gamma
# lambda = lambda
# learn_rate = eta

xgb_model <- boost_tree(
  mode = "classification",
  mtry = 1,
  sample_size = 1,
  min_n = 1,

  # -----------------------------------
  loss_reduction = 0,
  learn_rate = 0.3,
  tree_depth = 2,
  trees = 2

  #-------------------------------------
) %>%
  set_engine("xgboost", lambda = 0, params = list(min_child_weight = 0))

# fit
xgb_fit <- fit(xgb_model, curou ~ dose, data = data)
xgb_fit



# tidypredict + SQL -----------------------------------------------------------
con = DBI::dbConnect(RSQLite::SQLite(), "teste.db")

copy_to(con, data, "data")
predicoes <- tbl(con, "data") %>%
  mutate(
    pred = !!tidypredict_fit(xgb_fit)
  )

predicoes

show_query(predicoes)


# 1 - 1/(1 + exp(0 + case_when(dose >= 14 ~ 0.600000024, (dose < 5 | is.na(dose)) & (dose < 14 | is.na(dose)) ~ 0.600000024,
#                              dose >= 5 & (dose < 14 | is.na(dose)) ~ -0.600000024) +
#                    case_when(dose >= 14 ~ 0.464643508, (dose < 5 | is.na(dose)) & (dose < 14 | is.na(dose)) ~ 0.464643508,
#                              dose >= 5 & (dose < 14 | is.na(dose)) ~ -0.464643508)))


# 0 + case_when(dose >= 14 ~ -0.150000006,
#               (dose < 5 | is.na(dose)) & (dose < 14 | is.na(dose)) ~ -0.150000006,
#               dose >= 5 & (dose < 14 | is.na(dose)) ~ 0.150000006) +
#   case_when(dose >= 14 ~ -0.105000004,
#             (dose < 5 | is.na(dose)) & (dose < 14 | is.na(dose)) ~ -0.105000004,
#             dose >= 5 & (dose < 14 | is.na(dose)) ~ 0.105000012) +
#   0.5
