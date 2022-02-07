# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(skimr)
library(tidymodels)


# Dados -------------------------------------------------------------------
data("diamonds")

set.seed(8)
diamondsinho <- diamonds %>%
  filter(x > 0) %>%
  group_by(x) %>%
  sample_n(1) %>%
  ungroup() %>%
  sample_n(60)

# definicao do modelo -----------------------------------------------------
especificacao_modelo <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# ajuste do modelo --------------------------------------------------------

ajuste_modelo1 <- especificacao_modelo %>%
  fit(price ~ poly(x, 4), data = diamondsinho)
ajuste_modelo2 <- especificacao_modelo %>%
  fit(price ~ poly(x, 20), data = diamondsinho)

# predicoes ---------------------------------------------------------------
diamondsinho_com_previsao <- diamondsinho %>%
  mutate(
    price_pred1 = predict(ajuste_modelo1, new_data = diamondsinho)$.pred,
    price_pred2 = predict(ajuste_modelo2, new_data = diamondsinho)$.pred
  )

# qualidade dos ajustes e graficos ----------------------------------------
# Métricas de erro
diamondsinho_com_previsao_longo <- diamondsinho_com_previsao %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  )

diamondsinho_com_previsao_longo %>%
  group_by(modelo) %>%
  rmse(truth = price, estimate = price_pred)

diamondsinho_com_previsao_longo %>%
  group_by(modelo) %>%
  rsq(truth = price, estimate = price_pred)

# Pontos observados + curva da f
diamondsinho_com_previsao_g1 <- diamondsinho_com_previsao %>%
  ggplot() +
  geom_point(aes(x, price), size = 3) +
  geom_line(aes(x, price_pred2, color = 'modelo2'), size = 1) +
  geom_line(aes(x, price_pred1, color = 'modelo1'), size = 1) +
  theme_bw()
diamondsinho_com_previsao_g1

# Observado vs Esperado
diamondsinho_com_previsao_g2 <- diamondsinho_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price, colour = modelo), size = 3) +
  geom_abline(slope = 1, intercept = 0, colour = "purple", size = 1) +
  theme_bw()
diamondsinho_com_previsao_g2

# resíduos vs Esperado
diamondsinho_com_previsao_g3 <- diamondsinho_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price - price_pred, colour = modelo), size = 3) +
  geom_abline(slope = 0, intercept = 0, colour = "purple", size = 1) +
  ylim(c(-10000,10000)) +
  labs(y = "resíduo (y - y_chapeu)") +
  theme_bw()
diamondsinho_com_previsao_g3

############################################################################
############################################################################
############################################################################
# Agora vamos fingir que estamos em produção! (pontos vermelhos do slide)

set.seed(3)
# "dados novos chegaram..."
diamondsinho_novos <- diamonds %>%
  filter(x > 0) %>%
  sample_n(100)

# predicoes ---------------------------------------------------------------
diamondsinho_novos_com_previsao <- diamondsinho_novos %>%
  mutate(
    price_pred1 = predict(ajuste_modelo1, new_data = diamondsinho_novos)$.pred,
    price_pred2 = predict(ajuste_modelo2, new_data = diamondsinho_novos)$.pred
  )

# qualidade dos ajustes e graficos ----------------------------------------
# Métricas de erro
diamondsinho_novos_com_previsao_longo <- diamondsinho_novos_com_previsao %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  )

diamondsinho_novos_com_previsao_longo %>%
  group_by(modelo) %>%
  rmse(truth = price, estimate = price_pred)

diamondsinho_novos_com_previsao_longo %>%
  group_by(modelo) %>%
  rsq(truth = price, estimate = price_pred)

# Pontos observados + curva da f
diamondsinho_novos_com_previsao_g1 <- diamondsinho_novos_com_previsao %>%
  ggplot() +
  geom_point(aes(x, price), size = 3) +
  geom_line(aes(x, price_pred2, color = 'modelo2'), size = 1) +
  geom_line(aes(x, price_pred1, color = 'modelo1'), size = 1) +
  theme_bw()
diamondsinho_com_previsao_g1 / diamondsinho_novos_com_previsao_g1

# Observado vs Esperado
diamondsinho_novos_com_previsao_g2 <- diamondsinho_novos_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price, colour = modelo), size = 3) +
  geom_abline(slope = 1, intercept = 0, colour = "purple", size = 1) +
  theme_bw()
diamondsinho_com_previsao_g2 / diamondsinho_novos_com_previsao_g2

# resíduos vs Esperado
diamondsinho_novos_com_previsao_g3 <- diamondsinho_novos_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"),
    names_to = "modelo",
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price - price_pred, colour = modelo), size = 3) +
  geom_abline(slope = 0, intercept = 0, colour = "purple", size = 1) +
  ylim(c(-10000,10000)) +
  labs(y = "resíduo (y - y_chapeu)") +
  theme_bw()
diamondsinho_com_previsao_g3 / diamondsinho_novos_com_previsao_g3

