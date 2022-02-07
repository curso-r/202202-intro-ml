library(tidymodels)
library(tidyverse)
library(rpart)
library(rpart.plot)

# exemplo 1) diabetes -----------------------------------------------------
dados <- tribble(
  ~Pressão,	~Glicose,	~Diabetes,
  "hipertensao" ,92  ,    "nao",
  'normal'	    ,130 ,    "sim",
  "normal"	    ,130 ,    "nao",
  "normal"      ,55  ,    "nao",
  "hipertensao"	,220 ,    "sim",
  "normal"	    ,195 ,    "sim"
) %>%
  mutate(
    Diabetes = as.factor(Diabetes)
  )

diabetes_tree_model <- decision_tree(
  min_n = 1,
  cost_complexity = -1,
  tree_depth = 20
) %>%
  set_mode("classification") %>%
  set_engine("rpart")

credit_tree_fit <- fit(
  diabetes_tree_model,
  Diabetes ~.,
  data = dados
)

rpart.plot(credit_tree_fit$fit, roundint=FALSE, cex = 2)
cp <- as.data.frame(credit_tree_fit$fit$cptable)
cp



# exemplo 2) credit data --------------------------------------------------
# remotes::install_github("grantmcdermott/parttree")
library(parttree)
data("credit_data")
help(credit_data)
credit_data <- credit_data

credit_tree_model <- decision_tree(
  min_n = 1,
  tree_depth = 7,
  cost_complexity = -0.000001
) %>%
  set_mode("classification")

credit_tree_recipe <- recipe(Status ~ Seniority + Income, data = credit_data)

credit_tree_wf <- workflow() %>%
  add_model(credit_tree_model) %>%
  add_recipe(credit_tree_recipe)

credit_tree_fit <- fit(
  credit_tree_wf,
  data = credit_data
)

credit_tree_mod <- extract_fit_engine(credit_tree_fit)
rpart.plot(credit_tree_mod, roundint=FALSE)
vip::vip(credit_tree_mod)

credit_data %>%
  ggplot(aes(x = Seniority, y = Income)) +
  geom_parttree(data = credit_tree_fit, aes(fill = Status), alpha = 0.3) +
  geom_point(aes(col = Status)) +
  theme_minimal()

cp <- as.data.frame(credit_tree_mod$cptable)
cp


