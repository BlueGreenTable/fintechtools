#' Creates recursive multivariate regressions for wide data frames
#'
#' This function takes a wide dataframe as input and generates recursive multivariate
#' regressions. First column must be date, second must be dependent variable,
#' all columns thereafter are independent variable.
#'
#' @param data A wide dataframe as input. First column must be date, second must be dependent variable,
#' all columns thereafter are independent variable.
#'
#' @export

multiRegAuto <- function(data) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {stop("Package \"dplyr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("purrr", quietly = TRUE)) {stop("Package \"purrr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("tibble", quietly = TRUE)) {stop("Package \"tibble\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("glue", quietly = TRUE)) {stop("Package \"glue\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("broom", quietly = TRUE)) {stop("Package \"broom\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("tidyr", quietly = TRUE)) {stop("Package \"tidyr\" needed for this function to work. Please install it.", call. = FALSE)}
  if (!requireNamespace("stats", quietly = TRUE)) {stop("Package \"stats\" needed for this function to work. Please install it.", call. = FALSE)}
  ## data is a df, first column being data, second is dependent, onward is indep
  dependent <- names(data)[2]
  independents <- names(data)[-1:-2]
  ## rename dependent var for reg formula
  dataRef <- data
  data <- data %>% dplyr::rename(y = all_of(dependent))
  indepReg <- data %>%
    dplyr::select(-1:-2) %>% ## excluding dependent and date
    purrr::map(~lm(formula = data$y ~ .x, data = data)) %>%
    purrr::map(summary) %>%
    purrr::map_dbl("r.squared") %>%
    tibble::tibble(Predictors = names(.), r.squared = .) %>%
    dplyr::arrange(desc(.[[2]])) ## indep done
  ## instantiate output df outside loop
  out <- dplyr::tibble(model = "init")
  ## create df of regression results
  for(i in 1:length(independents)) {
    betas <- paste(independents[1:i], collapse = " + ")
    out[i,1] <- glue::glue("{dependent} ~ {betas}")
  }
  ## run recursive regs on formulas created
  out <- out %>% dplyr::mutate(output = purrr::pmap(.l = list(formula = model, data = list(dataRef)),
                                                    .f = stats::lm),
                               res = purrr::map(.x = output, .f = broom::glance)) %>%
    tidyr::unnest(res) %>%
    dplyr::select(1:4, p.value) %>%
    dplyr::mutate_if(is.numeric, round, 3)
  return(out)
}
