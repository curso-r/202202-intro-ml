# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)
library(ISLR2)


# Dados -------------------------------------------------------------------
data("Hitters")
#Hitters <- na.omit(Hitters)

# base treino e teste -----------------------------------------------------
set.seed(123)
hitters_initial_split <- Hitters %>% initial_split(3/4)

hitters_train <- training(hitters_initial_split)
hitters_test <- testing(hitters_initial_split)

# Dataprep ----------------------------------------------------------------

hitters_recipe <- recipe(Salary ~ ., data = hitters_train) %>%
  step_naomit(everything(), skip = TRUE) %>%
  step_rm(all_nominal()) %>%
  step_normalize(all_numeric_predictors())

# dar uma olhada no resultado da receita.
hitters_recipe %>%
  prep() %>%
  bake(new_data = hitters_train) %>%
  glimpse()


# definicao do modelo -----------------------------------------------------

# OBS: repare que agora colocamos "tune()" nos hiperparâmetros para os quais
# queremos encontrar o melhor valor.
hitters_model <- linear_reg(
  penalty = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Criando o workflow ------------------------------------------------------

hitters_wflow <- workflow() %>%
  add_recipe(hitters_recipe) %>%
  add_model(hitters_model)

# tunagem de hiperparametros ----------------------------------------------

# reamostragem com cross-validation ---------------------------------------
hitters_resamples <- vfold_cv(hitters_train, v = 10)

hitters_grid <- grid_regular(
  penalty(c(-1, 2)),
  levels = 10
)

hitters_tune_grid <- tune_grid(
  hitters_wflow,
  resamples = hitters_resamples,
  grid = hitters_grid,
  metrics = metric_set(rmse, rsq),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# inspecao da tunagem -----------------------------------------------------
autoplot(hitters_tune_grid)
collect_metrics(hitters_tune_grid)
show_best(hitters_tune_grid)

# seleciona o melhor conjunto de hiperparametros
hitters_best_hiperparams <- select_best(hitters_tune_grid, "rmse")
hitters_wflow <- hitters_wflow %>% finalize_workflow(hitters_best_hiperparams)

# desempenho do modelo final ----------------------------------------------
hitters_last_fit <- hitters_wflow %>% last_fit(split = hitters_initial_split)

collect_metrics(hitters_last_fit)
collect_predictions(hitters_last_fit) %>%
  ggplot(aes(.pred, Salary)) +
  geom_point()

# modelo final ------------------------------------------------------------
hitters_final_model <- hitters_wflow %>% fit(data = Hitters)

# predicoes ---------------------------------------------------------------

hitters_com_previsao <- Hitters %>%
  mutate(
    salary_pred = predict(hitters_final_model, new_data = .)$.pred
  )

predict(hitters_final_model, new_data = Hitters)

# guardar o modelo para usar depois ---------------------------------------
saveRDS(hitters_final_model, file = "hitters_final_model.rds")

