# Objetivo: prever quais clientes tem o maior potencial de compra no próximo mês
# para fins de ação de marketing.


library(tidyverse)
# download.file("https://storage.googleapis.com/deep-learning-com-r/ga-crp-train.rds", "ga-crp-train.rds")
ga_transactions <- readRDS("ga-crp-train.rds")

glimpse(ga_transactions)


# dataprep ----------------------------------------------------------------

ga_transactions <- ga_transactions %>%
  mutate(
    date = lubridate::ymd(date),
    totalTransactionRevenue = coalesce(totalTransactionRevenue, 0)
  )

# variavel resposta -------------------------------------------------------
# definição: compra na próxima semana.
resposta <- ga_transactions %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  group_by(month, fullVisitorId) %>%
  summarise(
    target = log1p(sum(totalTransactionRevenue))
  )

glimpse(resposta)
hist(resposta$target[resposta$target > 0 & resposta$month == "2018-04-01"], breaks = 1000)
summary(resposta$target > 0)


# features ----------------------------------------------------------------
library(lubridate)

# gera_features_de_x_semanas_atras()
gera_features_de_x_semanas_atras <- function(data, month, n_semanas_atras = 4) {
  month <- ymd(month)

  fatures_de_quatro_semanas_atras <- data %>%
    dtplyr::lazy_dt() %>%
    filter(date < month, date > (month - weeks(n_semanas_atras)))  %>%
    group_by(fullVisitorId) %>%
    summarise(
      last_channel_grouping = last(channelGrouping),
      last_ses_from_the_period_end = month - max(date),
      interval_dates = max(date) - min(date),
      unique_date_num = length(unique(date)),
      max_visit_num = max(visitNumber, na.rm = TRUE),
      across(c(browser, deviceCategory, continent,
               operatingSystem, subContinent, country,
               region, metro, city, networkDomain,
               source, medium),
             last,
             .names = "last_{.col}"),
      across(c(isMobile, isTrueDirect),
             ~mean(ifelse(is.na(.x), 0, 1)),
             .names = "prop_{.col}"),
      across(c(hits, pageviews),
             list(sum = sum, mean = mean, min = min, max = max, median = median, sd= sd),
             na.rm = TRUE,
             .names = "{.fn}_{.col}"),
      bounce_sessions = sum(ifelse(is.na(bounces), 0, 1)),
      session_cnt = n(),
      totalTransactionRevenue = sum(totalTransactionRevenue),
      transactions  = sum(transactions,na.rm = TRUE)
    ) %>%
    as_tibble()

  return(fatures_de_quatro_semanas_atras)
}

# amostra sistematica de 4 em 4 semanas para fins meramente computacionais.
library(purrr)
library(furrr)
plan(multisession, workers = 7)
historico <- resposta %>%
  ungroup() %>%
  distinct(month) %>%
  #filter(row_number() %% 2 == 0) %>%
  mutate(
    features = future_map(month, gera_features_de_x_semanas_atras, data = ga_transactions, .progress = TRUE)
  )
plan(sequential)

ga_full <- historico %>%
  unnest(features) %>%
  left_join(resposta, by = c("month", "fullVisitorId"))

ga_full <- ga_full %>%
  mutate(comprou = factor(if_else(target > 0, "sim", "não", "não"))) %>%
  select(-target)

ga_full <- ga_full %>%
  mutate(
    last_ses_from_the_period_end = as.numeric(last_ses_from_the_period_end),
    interval_dates = as.numeric(interval_dates)
  )

ga_train <- ga_full %>% filter(month < "2018-03-01")
ga_test <- ga_full %>% filter(month >= "2018-03-01")

readr::write_csv(ga_train, "ga_train.csv")
readr::write_csv(ga_test, "ga_test_gabarito.csv")
readr::write_csv(ga_test %>% select(-comprou), "ga_test.csv")
