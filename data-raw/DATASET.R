## code to prepare `DATASET` dataset goes here

stocks <-
  tidyquant::tq_get(
    c("TSM", "GOOG", "MSFT", "AAPL", "XOM", "CVX"),
    from = "2015-01-01",
    to = Sys.Date(),
    get = "Stock.prices") %>%
  dplyr::select(date, series = symbol, value = adjusted) %>%
  dplyr::group_by(series) %>%
  dplyr::mutate(return = log(value/dplyr::lag(value))) %>%
  na.omit() %>%
  dplyr::select(date, series, return) %>%
  tidyr::pivot_wider(names_from = series, values_from = return)

usethis::use_data(stocks, overwrite = T)
