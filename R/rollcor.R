#' Create a plot of rolling correlations of returns for 3 different time frames.
#'
#' This function takes a wide dataframe as input and generates a plot of rolling correlations
#' using the ggplot library.
#'
#' @param data A wide dataframe with columns 'date', 'returns1', 'returns2'. In that order.
#' @param tf1 Time frame 1
#' @param tf2 time frame 2
#' @param tf3 Time frame 3
#' @return An interactive Plotly plot.
#'
#' @export

rollcor <- function (data, tf1, tf2, tf3) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("slider", quietly = TRUE)) {stop("Package \"slider\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("ggplot2", quietly = TRUE)) {stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("stats", quietly = TRUE)) {stop("Package \"stats\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("tidyr", quietly = TRUE)) {stop("Package \"tidyr\" needed for this function to work. Please install it.", call. = FALSE)}
  rollcor <- data %>%
    dplyr::mutate(cor1 = slider::pslide_dbl(
      .l = list(data[[2]], data[[3]]),
      .f = ~ cor(.x, .y,),
      .before = tf1,
      .after = 0,
      .complete = T
    )) %>%
    dplyr::mutate(cor2 = slider::pslide_dbl(
      .l = list(data[[2]], data[[3]]),
      .f = ~ cor(.x, .y,),
      .before = tf2,
      .after = 0,
      .complete = T
    )) %>%
    dplyr::mutate(cor3 = slider::pslide_dbl(
      .l = list(data[[2]], data[[3]]),
      .f = ~ cor(.x, .y,),
      .before = tf3,
      .after = 0,
      .complete = T
    )) %>%
    tidyr::drop_na()

  # plot it

  rollcorplot <- rollcor %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = cor1)) +
    ggplot2::geom_line(ggplot2::aes( color = "tf1")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = stats::cor(data[[2]], data[[3]], method = "kendall"), color = "Static")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = cor2, color = "tf2")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = cor3, color = "tf3")) +
    ggplot2::scale_color_manual(name = "Window",
                       breaks = c("static", "tf1", "tf2", "tf3"),
                       values = c("black", "red", "blue", "green")) +
    ggplot2::labs(title = "Rolling Correlations",
         x = "", y = "Correlation Coefficient")

  rollcorplot
}
