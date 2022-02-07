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
# exercicio 1) Adicione os steps que forem necessários.

ames_recipe <- recipe(Sale_Price ~ ., data = ames_train)


# dar uma olhada no resultado da receita.
# ames_recipe %>%
#   prep() %>%
#   bake(new_data = ames_train)
#   glimpse()

# definicao do modelo -----------------------------------------------------
# exercicio 2) Defina um modelo de random forest e os hiperparâmetros que
# deseja tunar.


# Criando o workflow ------------------------------------------------------
# exercicio 3) Crie o objeto de workflow com o modelo e com a receita.


# reamostragem com cross-validation ---------------------------------------
# exercicio 4) Crie os folds de cross-validation.

# tunagem de hiperparametros ----------------------------------------------
# exercicio 5) crie uma grid com os hiperparâmetros a serem testados.
# exercicio 6) Tune com tune_grid() usando as métricas rmse e mae.

# inspecao da tunagem -----------------------------------------------------
# autoplot(ames_tune_grid)
# collect_metrics(ames_tune_grid)
# show_best(ames_tune_grid)

# exercicio 7) finalize o workflow com os melhores hiperparâmetros segundo
# o RMSE.

# desempenho do modelo final ----------------------------------------------
# exercicio 8) Ajuste o modelo na base de treino e analise o resultado na base
# de teste.

# exercicio 9) Faça um gráfico com a importância das variáveis.

# modelo final ------------------------------------------------------------
# exercicio 10) ajuste o modelo final usando a base ames inteira.

# predicoes ---------------------------------------------------------------

# exercicio 11) crie uma coluna na base ames com as prediçoes retornadas pelo
# modelo final.

# guardar o modelo para usar depois ---------------------------------------
# exercicio 12) Guarde o objeto do modelo final usando o saveRDS()

