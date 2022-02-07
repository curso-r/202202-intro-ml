library(dplyr)
library(archive)

# Read --------------------------------------------------------------------

# base full: https://storage.googleapis.com/deep-learning-com-r/ga-customer-revenue-prediction.zip

# usamos o archive p/ não precisar des-compactar
con <- archive::archive_read("ga-customer-revenue-prediction.zip", "train_v2.csv")
x <- readr::read_csv(con)


# Comprou vs nao comprou --------------------------------------------------

totals <- jsonlite::stream_in(textConnection(x$totals))
totals$fullVisitorId <- x$fullVisitorId
totals$totalTransactionRevenue <- readr::parse_number(totals$totalTransactionRevenue)
totals$transactionRevenue <- readr::parse_number(totals$transactionRevenue)
summary(totals$transactionRevenue)

compraram <- totals %>%
  filter(!is.na(totalTransactionRevenue)) %>%
  distinct(fullVisitorId)

nao_compraram <- totals %>%
  filter(is.na(totalTransactionRevenue)) %>%
  distinct(fullVisitorId) #%>%
#sample_n(2 * nrow(compraram))

selecionados <- bind_rows(compraram, nao_compraram)

visitas_selecionados <- x %>%
  filter(fullVisitorId %in% selecionados$fullVisitorId)


# Arrumar colunas de json -------------------------------------------------

totals_cols <- jsonlite::stream_in(textConnection(visitas_selecionados$totals)) %>%
  mutate(across(everything(), readr::parse_number))

device_cols <- jsonlite::stream_in(textConnection(visitas_selecionados$device)) %>%
  select(browser, operatingSystem, isMobile, deviceCategory)

traffic_cols <- jsonlite::stream_in(textConnection(visitas_selecionados$trafficSource)) %>%
  select(-adwordsClickInfo)

# col <- visitas_selecionados$customDimensions %>%
#   stringr::str_replace_all("'", '"') %>% # troca p/ aspas duplas
#   stringr::str_replace_all("\\[\\]", "{}") %>% # troca [] (vetor vazio) por {} (dict vazio)
#   stringr::str_remove_all("[\\[\\]]") # remove os vetorzinhos p/ o parse ficar mais legal
#
# custom_cols <- jsonlite::stream_in(textConnection(col)) %>%
#   select(region = value)

geo_cols <- jsonlite::stream_in(textConnection(visitas_selecionados$geoNetwork)) %>%
  select(-latitude, -longitude, -networkLocation, -cityId)


# Juntar tudo e salvar ----------------------------------------------------

v <- visitas_selecionados %>%
  select(-customDimensions, -device, -hits, -totals, -trafficSource, -geoNetwork) %>%
  bind_cols(totals_cols, geo_cols, device_cols, traffic_cols)

saveRDS(v, "ga-crp-train.rds")

# base 'arrumadinha': https://storage.googleapis.com/deep-learning-com-r/ga-crp-train.rds
