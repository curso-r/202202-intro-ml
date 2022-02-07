library(purrr)
library(broom)
library(tidyverse)
library(gganimate)
set.seed(1)
theme_set(theme_minimal(20))

# Criando banco de dados --------------------------------------------------

criar_amostra <- function(n) {
  tibble(
    x = runif(n, 0, 20),
    y = 500 + 0.4 * (x-10)^3 + rnorm(n, sd = 50)
  )
}

df_treino <- criar_amostra(10)
df_teste <- criar_amostra(10)

ggplot(df_treino, aes(x = x, y = y)) +
  geom_point()


polinomios <- tibble(
  grau = 1:9
) %>%
  mutate(
    modelo_polinomial = map(grau, ~ lm(as.formula(paste0("y ~ poly(x, ", .x, ")")), data = df_treino)),
    eqm_treino = map_dbl(modelo_polinomial, ~sqrt(mean((df_treino$y - predict(.x, newdata = df_treino))^2))),
    eqm_teste = map_dbl(modelo_polinomial, ~sqrt(mean((df_teste$y - predict(.x, newdata = df_teste))^2))),
    cor = c("#F8766D", "#D39200", "#93AA00", "#00BA38", "#00C19F", "#00B9E3", "#619CFF", "#DB72FB", "#FF61C3"),
    dados_grafico = map(modelo_polinomial, ~ {
      tibble(x = seq(min(df_treino$x), max(df_treino$x),length.out = 500)) %>%
        mutate(y = predict(.x, newdata = .))
    })
  )


gif <- polinomios %>%
  select(grau, dados_grafico) %>%
  unnest(cols = c("dados_grafico")) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(show.legend = FALSE, colour = "purple", size = 1) +
  geom_point(aes( colour = "TREINO"), data = df_treino, size = 4) +
  geom_point(aes( colour = "TESTE"), data = df_teste, size = 3) +
  scale_colour_manual(values = c("TREINO" = "#00BFC4", "TESTE" = "salmon")) +
  # scale_colour_manual(values = c("TREINO" = "#00BFC4")) +
  coord_cartesian(ylim = (c(-200,1300))) +
  labs(colour = "") +
  labs(title = 'Complexidade do modelo: {closest_state}') +
  transition_states(grau,
                    transition_length = 2,
                    state_length = 2,
                    wrap = TRUE) +
  ease_aes("sine-in-out")
animate(gif, height = 400, width =650)
gganimate::save_animation(gganimate::last_animation(), file = "slides/img/overfiting_sem_teste.gif")














###########################################################
# scatter + eqm
gif_scatter <- polinomios %>%
  select(grau, dados_grafico) %>%
  unnest(cols = c("dados_grafico")) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(show.legend = FALSE, colour = "purple", size = 1) +
  geom_point(aes( colour = "TREINO"), data = df_treino, size = 4, show.legend = FALSE) +
  geom_point(aes( colour = "TESTE"), data = df_teste, size = 3, show.legend = FALSE) +
  # scale_colour_manual(values = c("TREINO" = "purple", "TESTE" = "salmon")) +
  coord_cartesian(ylim = (c(-200,1300))) +
  labs(colour = "") +
  labs(title = 'Complexidade do modelo: {closest_state}') +
  transition_states(grau,
                    transition_length = 2,
                    state_length = 2,
                    wrap = TRUE)
animate(gif_scatter, height = 400, width =500)
gganimate::save_animation(gganimate::last_animation(), file = "slides/img/gif_scatter.gif")





eqm_grafico <- polinomios %>%
  select(grau, eqm_treino, eqm_teste) %>%
  gather(base, eqm, -grau)

gif_eqm <- eqm_grafico %>%
  mutate(base = ifelse(base == "eqm_treino", "TREINO", "TESTE")) %>%
  ggplot() +
  geom_line(aes(x = grau2, y = eqm2, group = base2), color = "grey80", data = eqm_grafico %>% rename_all(~paste0(.,"2"))) +
  geom_point(aes(x = grau, y = eqm, colour = base), size = 4) +
  coord_cartesian(ylim = (c(0, 100))) +
  scale_x_continuous(breaks = 1:9) +
  labs(colour = "") +
  labs(title = '', y = "Erro do Modelo (EQM)", x = "Complexidade do modelo") +
  transition_states(grau,
                    transition_length = 2,
                    state_length = 2,
                    wrap = TRUE) +
  shadow_trail(distance = 0.008, size = 1)
animate(gif_eqm, height = 400, width =400)
gganimate::save_animation(gganimate::last_animation(), file = "slides/img/gif_eqm.gif")








library(magick)

gif_scatter <- image_read("slides/img/gif_scatter.gif")
gif_eqm <- image_read("slides/img/gif_eqm.gif")

combined_gif <- image_append(c(gif_scatter[1], gif_eqm[1]))
pb <- progress::progress_bar$new(total = 100)
for(i in 2:100){
  pb$tick()
  combined <- image_append(c(gif_scatter[i], gif_eqm[i]))
  combined_gif <- c(combined_gif, combined)
}

gganimate::save_animation(combined_gif, "slides/img/overfiting_scatter_eqm.gif")




