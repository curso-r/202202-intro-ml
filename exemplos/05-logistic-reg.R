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

# Exploratória ------------------------------------------------------------

skimr::skim(credit_train)
visdat::vis_miss(credit_train)
credit_train %>%
  select(where(is.numeric)) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot::corrplot()

# Data prep ---------------------------------------------------------------

credit_recipe <- recipe(Status ~ ., data = credit_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_impute_linear(Income, Assets, Debt, impute_with = imp_vars(Expenses)) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_poly(all_numeric_predictors(), degree = 9) %>%
  step_dummy(all_nominal_predictors())


bake(prep(credit_recipe), new_data = NULL)
visdat::vis_miss(bake(prep(credit_recipe), new_data = NULL))

# Modelo  -----------------------------------------------------------------

# Definição de
# a) a f(x): logistc_reg()
# b) modo (natureza da var resp): classification
# c) hiperparametros que queremos tunar: penalty = tune()
# d) hiperparametros que não queremos tunar: mixture = 1 # LASSO
# e) o motor que queremos usar: glmnet

credit_lr_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# Workflow ----------------------------------------------------------------

credit_wf <- workflow() %>%
  add_model(credit_lr_model) %>%
  add_recipe(credit_recipe)


# Tune --------------------------------------------------------------------

# a) bases de reamostragem para validação: vfold_cv()
# b) (opcional) grade de parâmetros: parameters() %>% update() %>% grid_regular()
# c) tune_grid(y ~ x + ...)
# d) escolha das métricas (rmse, roc_auc, etc)
# d) collect_metrics() ou autoplot() para ver o resultado
credit_resamples <- vfold_cv(credit_train, v = 5, strata = "Status")
grid <- grid_regular(
  penalty(range = c(-4, -1)),
  levels = 20
)

credit_lr_tune_grid <- tune_grid(
  credit_wf,
  resamples = credit_resamples,
  grid = grid,
  metrics = metric_set(
    mn_log_loss, #binary cross entropy
    accuracy,
    roc_auc,
    #kap, # KAPPA
    # precision,
    # recall,
    # f_meas,
  )
)

autoplot(credit_lr_tune_grid)

# minha versão do autoplot()
collect_metrics(credit_lr_tune_grid)

collect_metrics(credit_lr_tune_grid) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_ribbon(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.1) +
  facet_wrap(~.metric, ncol = 2, scales = "free_y") +
  scale_x_log10()


# Desempenho do modelo final ----------------------------------------------

# a) extrai melhor modelo com select_best()
# b) finaliza o modelo inicial com finalize_model()
# c) ajusta o modelo final com todos os dados de treino (bases de validação já era)
credit_lr_best_params <- select_best(credit_lr_tune_grid, "roc_auc")
credit_wf <- credit_wf %>% finalize_workflow(credit_lr_best_params)

credit_lr_last_fit <- last_fit(
  credit_wf,
  credit_initial_split
)

# Variáveis importantes
credit_lr_last_fit_model <- credit_lr_last_fit$.workflow[[1]]$fit$fit
vip(credit_lr_last_fit_model)


# Guardar tudo ------------------------------------------------------------

write_rds(credit_lr_last_fit, "credit_lr_last_fit.rds")
write_rds(credit_lr_model, "credit_lr_model.rds")

collect_metrics(credit_lr_last_fit)

credit_test_preds <- collect_predictions(credit_lr_last_fit)

# roc
credit_roc_curve <- credit_test_preds %>% roc_curve(Status, .pred_bad)
autoplot(credit_roc_curve)

credit_lift_curve <- credit_test_preds %>% lift_curve(Status, .pred_bad)
autoplot(credit_lift_curve)

# confusion matrix
credit_test_preds %>%
  mutate(
    Status_class = factor(if_else(.pred_bad > 0.6, "bad", "good"))
  ) %>%
  conf_mat(Status, Status_class)

# gráficos extras!

# risco por faixa de score
credit_test_preds %>%
  mutate(
    score =  factor(ntile(.pred_bad, 10))
  ) %>%
  count(score, Status) %>%
  ggplot(aes(x = score, y = n, fill = Status)) +
  geom_col(position = "fill") +
  geom_label(aes(label = n), position = "fill") +
  coord_flip()

# gráfico sobre os da classe "bad"
percentis = 20
credit_test_preds %>%
  mutate(
    score = factor(ntile(.pred_bad, percentis))
  ) %>%
  filter(Status == "bad") %>%
  group_by(score, .drop = FALSE) %>%
  summarise(
    n = n(),
    media = mean(.pred_bad)
  ) %>%
  mutate(p = n/sum(n)) %>%
  ggplot(aes(x = p, y = score)) +
  geom_col() +
  geom_label(aes(label = scales::percent(p))) +
  geom_vline(xintercept = 1/percentis, colour = "red", linetype = "dashed", size = 1)


# Modelo final ------------------------------------------------------------

credit_final_lr_model <- credit_wf %>% fit(credit_data)



