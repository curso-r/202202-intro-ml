# Pacotes ------------------------------------------------------------------

library(tidymodels)
library(ISLR)
library(tidyverse)
library(modeldata)
library(pROC)
library(vip)


# Bases de dados ----------------------------------------------------------

data("credit_data")
help(credit_data)
glimpse(credit_data) # German Risk

credit_data %>% count(Status)

# Base de treino e teste --------------------------------------------------

set.seed(1)
credit_initial_split <- initial_split(credit_data, strata = "Status", prop = 0.75)

credit_train <- training(credit_initial_split)
credit_test  <- testing(credit_initial_split)

# Reamostragem ------------------------------------------------------------

credit_resamples <- vfold_cv(credit_train, v = 5, strata = "Status")

# Exploratória ------------------------------------------------------------

# skimr::skim(credit_train)
# visdat::vis_miss(credit_train)
# credit_train %>%
#   select(where(is.numeric)) %>%
#   cor(use = "pairwise.complete.obs") %>%
#   corrplot::corrplot()


# Regressão Logística -----------------------------------------------------

## Data prep

credit_lr_recipe <- recipe(Status ~ ., data = credit_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_impute_linear(Income, Assets, Debt, impute_with = imp_vars(Expenses)) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_poly(all_numeric_predictors(), degree = 9) %>%
  step_dummy(all_nominal_predictors())

## Modelo

credit_lr_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

## Workflow

credit_lr_wf <- workflow() %>%
  add_model(credit_lr_model) %>%
  add_recipe(credit_lr_recipe)

## Tune

grid_lr <- grid_regular(
  penalty(range = c(-4, -1)),
  levels = 20
)

credit_lr_tune_grid <- tune_grid(
  credit_lr_wf,
  resamples = credit_resamples,
  grid = grid_lr,
  metrics = metric_set(roc_auc)
)

autoplot(credit_lr_tune_grid)

# Árvore de decisão -------------------------------------------------------

## Data prep

credit_dt_recipe <- recipe(Status ~ ., data = credit_train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors())

## Modelo

credit_dt_model <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("rpart")

## Workflow

credit_dt_wf <- workflow() %>%
  add_model(credit_dt_model) %>%
  add_recipe(credit_dt_recipe)

## Tune

grid_dt <- grid_random(
  cost_complexity(c(-9, -2)),
  tree_depth(range = c(5, 15)),
  min_n(range = c(20, 40)),
  size = 20
)

# doParallel::registerDoParallel(4)

credit_dt_tune_grid <- tune_grid(
  credit_dt_wf,
  resamples = credit_resamples,
  grid = grid_dt,
  metrics = metric_set(roc_auc)
)

# doParallel::stopImplicitCluster()

autoplot(credit_dt_tune_grid)
collect_metrics(credit_dt_tune_grid)


# Desempenho dos modelos finais ----------------------------------------------

credit_lr_best_params <- select_best(credit_lr_tune_grid, "roc_auc")
credit_lr_wf <- credit_lr_wf %>% finalize_workflow(credit_lr_best_params)
credit_lr_last_fit <- last_fit(credit_lr_wf, credit_initial_split)

credit_dt_best_params <- select_best(credit_dt_tune_grid, "roc_auc")
credit_dt_wf <- credit_dt_wf %>% finalize_workflow(credit_dt_best_params)
credit_dt_last_fit <- last_fit(credit_dt_wf, credit_initial_split)


credit_test_preds <- bind_rows(
  collect_predictions(credit_lr_last_fit) %>% mutate(modelo = "lr"),
  collect_predictions(credit_dt_last_fit) %>% mutate(modelo = "dt")
)

## roc
credit_test_preds %>%
  group_by(modelo) %>%
  roc_curve(Status, .pred_bad) %>%
  autoplot()

## lift
credit_test_preds %>%
  group_by(modelo) %>%
  lift_curve(Status, .pred_bad) %>%
  autoplot()

# Variáveis importantes
credit_lr_last_fit_model <- credit_lr_last_fit$.workflow[[1]]$fit$fit
vip(credit_lr_last_fit_model)

credit_dt_last_fit_model <- credit_dt_last_fit$.workflow[[1]]$fit$fit
vip(credit_dt_last_fit_model)

# Guardar tudo ------------------------------------------------------------

write_rds(credit_lr_last_fit, "credit_lr_last_fit.rds")
write_rds(credit_lr_model, "credit_lr_model.rds")

# Modelo final ------------------------------------------------------------

credit_final_lr_model <- credit_lr_wf %>% fit(credit_data)



