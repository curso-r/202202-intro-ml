
# Pacotes -----------------------------------------------------------------
library(tidymodels)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(pROC)
library(vip)

# import ------------------------------------------------------------------

ga <- readr::read_csv("ga_train.csv")

# Bases de dados ----------------------------------------------------------

glimpse(ga)
ga %>% count(comprou)

# Base de treino e teste --------------------------------------------------

ga_initial_split <- make_splits(
  ind = list(
    analysis = which(!as.character(ga$month) %in% c("2018-01-01", "2018-02-01")),
    assessment = which(as.character(ga$month) %in% c("2018-01-01", "2018-02-01"))
  ),
  data = ga
)

ga_train <- training(ga_initial_split)
ga_valid <- testing(ga_initial_split)

# Reamostragem ------------------------------------------------------------
ga_resamples <- sliding_period(ga_train, index = month, period = "month",
                               lookback = 5, step=2)

# Exploratória ------------------------------------------------------------

# skimr::skim(ga_train)
# visdat::vis_miss(ga_train)
# ga_train %>%
#   select(where(is.numeric)) %>%
#   cor(use = "pairwise.complete.obs") %>%
#   corrplot::corrplot()

# Árvore de decisão -------------------------------------------------------

## Data prep

ga_dt_recipe <- recipe(comprou ~ ., data = ga_train) %>%
  update_role(month, new_role = "date") %>%
  update_role(fullVisitorId, new_role = "id") %>%
  step_rm(skip = TRUE,
          last_region,
          last_metro,
          last_city,
          last_networkDomain,
          last_source,
          last_browser
  ) %>%
  themis::step_downsample(comprou, under_ratio = 10) %>%
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_other(
    last_subContinent,
    last_operatingSystem
  )


## Modelo

ga_dt_model <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("rpart")

## Workflow

ga_dt_wf <- workflow() %>%
  add_model(ga_dt_model) %>%
  add_recipe(ga_dt_recipe)

## Tune

grid_dt <- grid_random(
  cost_complexity(c(-9, -2)),
  tree_depth(range = c(5, 15)),
  min_n(range = c(20, 40)),
  size = 3
)

# doParallel::registerDoParallel(4)

ga_dt_tune_grid <- tune_grid(
  ga_dt_wf,
  resamples = ga_resamples,
  grid = grid_dt,
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE)
)

# doParallel::stopImplicitCluster()

autoplot(ga_dt_tune_grid)
collect_metrics(ga_dt_tune_grid)


# Desempenho dos modelos finais ----------------------------------------------

ga_dt_best_params <- select_best(ga_dt_tune_grid, "roc_auc")
ga_dt_wf <- ga_dt_wf %>% finalize_workflow(ga_dt_best_params)
ga_dt_last_fit <- last_fit(ga_dt_wf, ga_initial_split)


ga_test_preds <- collect_predictions(ga_dt_last_fit) %>% mutate(modelo = "dt")

## roc
ga_test_preds %>%
  group_by(modelo) %>%
  roc_curve(comprou, `.pred_não`) %>%
  autoplot()


# Variáveis importantes
ga_dt_last_fit_model <- ga_dt_last_fit$.workflow[[1]]$fit$fit
vip(ga_dt_last_fit_model)

rpart.plot(ga_dt_last_fit_model$fit, faclen = 2)

# Modelo final ------------------------------------------------------------

ga_final_dt_model <- ga_dt_wf %>% fit(ga)


# arquivo de submissao ----------------------------------------------------

ga_test <- readr::read_csv("ga_test.csv")

ga_submission <- ga_test %>%
  mutate(
    target = predict(ga_final_dt_model, new_data = ., type = "prob")$.pred_sim
  )
ga_submission %>%
  mutate(fullVisitorId = paste(fullVisitorId, month, sep = "-")) %>%
  select(fullVisitorId, comprou = target) %>%
  write_csv("ga_submission.csv")
