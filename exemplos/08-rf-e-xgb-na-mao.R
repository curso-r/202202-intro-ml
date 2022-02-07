library(tree)
library(magrittr)
library(ggplot2)
library(tidyverse)
x <- runif(100, -1, 1)
y <- sin(x*3) + rnorm(100, sd = 0.1)

dados <- tibble(x, y)

dados %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()


# random forest -----------------------------------------------------------

arvores <- list()
trees <- 200
tree_depth <- 4
mtry <- 1

n <- nrow(dados)
features <- setdiff(names(dados), "y")
for(i in 1:trees) {
  amostra_bootstrap <- dados[sample.int(n, n, replace = TRUE), c("y", sample(features, mtry))]
  arvores[[i]] <- rpart::rpart(y ~ .,
                               data = amostra_bootstrap,
                               control = rpart::rpart.control(maxdepth = tree_depth))
}

f <- function(x, arvores) {
  trees <- length(arvores)
  pred <- rep(0, length = length(x))
  for(i in 1:trees){
    pred <- pred + predict(arvores[[i]], tibble(x = x))
  }
  return(pred/trees)
}

dados %>%
  mutate(pred_rf = f(x, arvores)) %>%
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_step(aes(x = x, y = pred_rf, colour = "RF"), size = 1)

# boosting ----------------------------------------------------------------

loss <- function(y, y_hat) (y - y_hat)^2

# gradiente (G)
G <- function(y, y_hat) - 2 * (y - y_hat)

# hessiana (H)
H <- function(y, y_hat) 2


# f(x) = a + b*x
# f(x, arvores) = 0.0 + lr * arvore1 + lr * arvore2 + ... + lr * arvoreN
f <- function(x, arvores) {
  r <- rep(0, length(x))

  # soma as árvores (os case_whens)
  for (arvore in arvores) {
    r <- r + lr * predict(arvore, tibble(x = x))
  }
  r
}

arvores <- list()
y_hat <- 0.5
lr <- 0.5
trees <- 200
lambda <- 15
gamma <- 40
tree_depth <- 3

for (i in 1:trees) {
  r <- -G(y, y_hat)/(H(y, y_hat) + lambda) # output = - G/H
  arvores[[i]] <-  rpart::rpart(r ~ x,
                                control = rpart::rpart.control(maxdepth = tree_depth))
  y_hat <- f(x, arvores)
}

tibble(x = x, y = y, y_hat = y_hat) %>%
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_step(aes(x = x, y = y_hat), colour = "red", size = 1)
