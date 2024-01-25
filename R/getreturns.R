#' Create a long data frame with log or normal returns.
#'
#' This function takes a vector of stock tickers and a date range to
#' generates a dataframe of returns.
#'
#' @param tickers A long dataframe with columns 'date', 'series', and 'value'.
#' @param from Start Date
#' @param to End Date
#' @param returntype "log" versus "normal"
#' @return A long dataframe of log or normal returns
#'
#' @export
#'

getreturns <- function (tickers = tickers, from = from, to = to, type = type) {
if (!requireNamespace("tidyquant", quietly = TRUE)) {stop("Package \"tidyquant\" needed for this function to work. Please install it.", call. = FALSE)}
if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" needed for this function to work. Please install it.", call. = FALSE)}
  # Check if type is one of the specified values
  if (!type %in% c('log', 'abs', 'rel')) {
    stop("Invalid type. Please enter 'log', 'abs', or 'rel'.")
  }
  tidyquant::tq_get(tickers, get = 'stock.prices', from = from, to = to) %>%
      dplyr::select(date, symbol, adjusted) %>%
      dplyr::group_by(symbol) %>%
      dplyr::mutate(returns = dplyr::case_when(type == 'log' ~ log(adjusted /dplyr::lag(adjusted)),
                                               type == 'abs' ~ adjusted - dplyr::lag(adjusted),
                                               type == 'rel' ~ ((adjusted / dplyr::lag(adjusted)) - 1),
                                               TRUE ~ 0)) %>%
      dplyr::select(date, symbol, adjusted, returns) %>%
    tidyr::drop_na()
  }
