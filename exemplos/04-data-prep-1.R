# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(tidymodels)
library(ISLR2)

# Dados -------------------------------------------------------------------
data(ames)

ames %>%
  ggplot(aes(x  = Sale_Price)) +
  geom_histogram()

ames <- ames %>%
  mutate(
    Sale_Price = log10(Sale_Price)
  ) %>%
  # select(
  #   Sale_Price, # Preço da casa que queremos prever.
  #   Neighborhood, # Nome do bairro
  #   Gr_Liv_Area, # Área construida
  #   Year_Built, # Ano de construção
  #   Bldg_Type, # Tipo da casa (1 familia, etc)
  #   Latitude,
  #   Longitude,
  #   Bedroom_AbvGr # Numero de quartos (sem ser no porão)
  # ) %>%
  select(
    - Sale_Condition,
    - Sale_Type,
    - Year_Sold,
    - Mo_Sold
  )

ames %>%
  ggplot(aes(x  = Sale_Price)) +
  geom_histogram()


# base treino e teste -----------------------------------------------------
set.seed(123)
ames_initial_split <- ames %>% initial_split(3/4)

ames_train <- training(ames_initial_split)
ames_test <- testing(ames_initial_split)


# EDA ---------------------------------------------------------------------


# Discretização
ames %>%
  ggplot(aes(x = Bedroom_AbvGr, y = Sale_Price)) +
  geom_point() +
  stat_summary(color = "red")

ames %>%
  mutate(
    Bedroom_AbvGr = case_when(
      Bedroom_AbvGr <= 1 ~ "a. até 1",
      Bedroom_AbvGr <= 2 ~ "b. 2",
      Bedroom_AbvGr <= 3 ~ "c. 3",
      Bedroom_AbvGr <= 4 ~ "d. 4",
      Bedroom_AbvGr >= 5 ~ "e. 5 ou mais"
    )
  ) %>%
  ggplot(aes(x = Bedroom_AbvGr, y = Sale_Price)) +
  geom_boxplot()


# Agrupamento
ames %>%
  dplyr::count(Neighborhood) %>%
  ggplot(aes(y = fct_reorder(Neighborhood, n), x = n)) +
  geom_col()

# Interacao numerico x categorico
ames %>%
  ggplot(aes(x = log10(Gr_Liv_Area), y = Sale_Price)) +
  geom_point(alpha = .2) +
  facet_wrap(~ Bldg_Type) +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, col = "red")

# Interação -  Numerico x Numerico
ames %>%
  ggplot(aes(x = Latitude, y = Sale_Price)) +
  geom_point() +
  geom_smooth(method = "lm")

ames %>%
  ggplot(aes(x = Longitude, y = Sale_Price)) +
  geom_point() +
  geom_smooth(method = "lm")

ames %>%
  ggplot(aes(x = Latitude, y = Longitude)) +
  stat_summary_2d(aes(z = Sale_Price), fun = mean) +
  scale_fill_viridis_c(option = "A")


# Dataprep ----------------------------------------------------------------


ames_recipe <- recipe(Sale_Price ~ ., data = ames_train) %>%
  step_cut(Bedroom_AbvGr, breaks = c(1,2,3,4), include_outside_range = TRUE, ) %>%
  # step_discretize(Bedroom_AbvGr, num_breaks = 4) %>%
  step_other(Neighborhood, threshold = tune()) %>%
  step_log(Gr_Liv_Area, base = 10) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_interact(~Gr_Liv_Area:starts_with("Bldg_Type_")) %>%
  step_interact(~Latitude:Longitude)


# dar uma olhada no resultado da receita.
# ames_recipe %>%
#   prep() %>%
#   bake(new_data = ames_train)
#   glimpse()

# definicao do modelo -----------------------------------------------------

# OBS: repare que agora colocamos "tune()" nos hiperparâmetros para os quais
# queremos encontrar o melhor valor.
ames_model <- linear_reg(
  penalty = tune()
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Criando o workflow ------------------------------------------------------

ames_wflow <- workflow() %>%
  add_recipe(ames_recipe) %>%
  add_model(ames_model)

# tunagem de hiperparametros ----------------------------------------------

# reamostragem com cross-validation ---------------------------------------
ames_resamples <- vfold_cv(ames_train, v = 5)

ames_grid <- grid_regular(
  penalty(c(-6, -1)),
  threshold(range = c(10, 100)),
  levels = 10
)

ames_tune_grid <- tune_grid(
  ames_wflow,
  resamples = ames_resamples,
  grid = ames_grid,
  metrics = metric_set(rmse, mae),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

# inspecao da tunagem -----------------------------------------------------
autoplot(ames_tune_grid)
collect_metrics(ames_tune_grid) %>% View()
show_best(ames_tune_grid)

# seleciona o melhor conjunto de hiperparametros
ames_best_hiperparams <- select_best(ames_tune_grid, "rmse")
ames_wflow <- ames_wflow %>% finalize_workflow(ames_best_hiperparams)

# desempenho do modelo final ----------------------------------------------
ames_last_fit <- ames_wflow %>% last_fit(split = ames_initial_split)

collect_metrics(ames_last_fit)
collect_predictions(ames_last_fit) %>%
  ggplot(aes(.pred, Sale_Price)) +
  geom_point()

# https://github.com/koalaverse/vip/pull/99
vip::vip(extract_fit_parsnip(ames_last_fit$.workflow[[1]]))

# modelo final ------------------------------------------------------------
ames_final_model <- ames_wflow %>% fit(data = ames)

# predicoes ---------------------------------------------------------------

ames_com_previsao <- ames %>%
  mutate(
    pred_price = predict(ames_final_model, new_data = .)$.pred
  )

predict(ames_final_model, new_data = ames)

# guardar o modelo para usar depois ---------------------------------------
saveRDS(ames_final_model, file = "ames_final_model.rds")
