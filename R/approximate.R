#' @title Linear approximation of missing values
#'
#' @description Fill missing values in a time series (or in a column of a longitudinal data set)
#' with the approximation method.
#'
#' @param dataset A data frame, tibble or dataset object created by \code{\link{dataset}},
#' with time, geo, value, frequency, obs_status and method columns.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols arrange
#' @importFrom zoo na.approx
#' @family approximation functions
#' @return A tibble updated with with approximated values.
#' @examples{
#' example_df <- data.frame (
#'    time = rep(as.Date (paste0(2018:2021, "-01-01")),3),
#'    geo = c( rep("NL", 4), rep("BE", 4), rep("LU", 4)),
#'    value = c(10,11, NA_real_, 12, NA_real_, 21,22,23, 5,6,7,NA_real_),
#'    freq = rep("A", 12)
#' )
#' example_df$method <- ifelse(is.na(example_df$value), "missing", "actual")
#' example_df$obs_status <- example_df$method
#'
#' na_approx ( example_df )
#' }
#' @export

na_approx <- function (dataset, ...) {

  test_unique_observations(dataset)
  params <- match.call(expand.dots = TRUE)

  if ( "method"  %in% names(params) ) method <- params$method else method <- "linear"
  if ( "maxgap"  %in% names(params) ) maxgap <- params$maxgap else maxgap <- 1
  if ( "rule"  %in% names(params) )   rule <- params$rule else rule <- 1
  if ( "fromLast"  %in% names(params) ) fromLast <- params$fromLast else fromLast <- FALSE
  if ( "na.rm" %in% names(params)) na.rm <- params$na.rm else na.rm <- FALSE

  to_add_back <- dataset %>%
    select ( -all_of(c("value")))

  tmp <- dataset %>%
    select ( all_of(c("time", "geo", "value", "freq"))) %>%
    arrange (.data$time) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value") %>%
    remove_empty_rows()

  dataset_ts <- create_time_series(tmp)

  approximated <- zoo::na.approx(dataset_ts,
                                 maxgap = maxgap,
                                 na.rm = na.rm,
                                 method = method,
                                 rule = rule)

  approx_df <- as.data.frame (approximated) %>% # assigned for easier debugging
    bind_cols( tmp  %>%
                 select (
                   all_of(c("time", "freq")))
    )


  long_form_approx <- approx_df  %>%
    pivot_longer( cols = -all_of(c("time", "freq")),
                  names_to = 'geo',
                  values_to = 'value') %>%
    left_join ( to_add_back,
                by = c("time", "freq", "geo")
    ) %>%
    mutate ( method  = ifelse(!is.na(.data$value) & .data$method=="O",
                              "approx", .data$method),
             obs_status  = ifelse(!is.na(.data$value) & .data$obs_status=="O",
                                  "E", .data$obs_status))

  long_form_approx %>%
    select ( all_of(names(dataset)))

}


#' Last observation carry forward
#'
#' Fill missing values in a time series (or in a column of a longitudional data set)
#' with the last observation carry forward method.
#'
#' @inheritParams na_approx
#' @param ... Pass on arguments of \code{\link[zoo]{na.locf}}. The zoo defaults settings are
#' changed to \code{maxgap=1}.
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols arrange
#' @importFrom zoo na.locf
#' @family approximation functions
#' @return A tibble updated with the forward carried values.
#' @examples{
#' example_df <- data.frame (
#'    time = rep(as.Date (paste0(2018:2021, "-01-01")),3),
#'    geo = c( rep("NL", 4), rep("BE", 4), rep("LU", 4)),
#'    value = c(10,11, NA_real_, 12, NA_real_, 21,22,23, 5,6,7,NA_real_),
#'    freq = rep("A", 12)
#' )
#' example_df$method <- ifelse(is.na(example_df$value), "missing", "actual")
#' example_df$obs_status <- example_df$method
#'
#' na_locf ( example_df )
#' }
#' @export
#'

na_locf <- function (dataset, ...) {

  test_unique_observations(dataset)

  params <- match.call(expand.dots = TRUE)

  if ( "method"  %in% names(params) ) method <- params$method else method <- "linear"
  if ( "maxgap"  %in% names(params) ) maxgap <- params$maxgap else maxgap <- 1
  if ( "rule"  %in% names(params) )   rule <- params$rule else rule <- 2
  if ( "fromLast"  %in% names(params) ) fromLast <- params$fromLast else fromLast <- FALSE
  if ( "na.rm" %in% names(params)) na.rm <- params$na.rm else na.rm <- FALSE

  to_add_back <- dataset %>%
    select ( -all_of(c("value")))

  tmp <- dataset %>%
    select ( all_of(c("time", "geo", "value", "freq" ))) %>%
    dplyr::arrange (.data$time) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value") %>%
    remove_empty_rows()

  dataset_ts <- create_time_series(tmp)
  locf <- zoo::na.locf(dataset_ts,
                       method = method,
                       maxgap = maxgap,
                       rule = rule,
                       na.rm = na.rm,
                       fromLast = fromLast )

  long_form_locf <- as.data.frame (locf) %>% # assigned for easier debugging
    bind_cols( tmp  %>%
                 select (
                   all_of(c("time", "freq")))
    )   %>%
    pivot_longer( cols = -all_of(c("time", "freq")),
                  names_to = 'geo',
                  values_to = 'value') %>%
    left_join ( to_add_back,
                by = c("time", "freq", "geo")
    )   %>%
    mutate ( method  = ifelse(!is.na(.data$value) & .data$method=="O",
                              "locf", .data$method),
             obs_status  = ifelse(!is.na(.data$value) & .data$obs_status=="O",
                                "E", .data$obs_status))

  long_form_locf

}

#' Next observation carry back
#'
#' Fill missing values in a time series (or in a column of a longitudional data set)
#' with the next observation carry back method.
#'
#' @inheritParams na_approx
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select filter left_join mutate bind_cols arrange
#' @importFrom zoo na.locf
#' @family approximation functions
#' @return A tibble updated with the values carried back.
#' @examples {
#' example_df <- data.frame (
#'    time = rep(as.Date (paste0(2018:2021, "-01-01")),3),
#'    geo = c( rep("NL", 4), rep("BE", 4), rep("LU", 4)),
#'    value = c(10,11, NA_real_, 12, NA_real_, 21,22,23, 5,6,7,NA_real_),
#'    freq = rep("A", 12)
#' )
#' example_df$method <- ifelse(is.na(example_df$value), "missing", "actual")
#' example_df$obs_status <- example_df$method
#'
#' na_nocb ( example_df )
#' }
#' @export


na_nocb <- function (dataset, ...) {

  test_unique_observations(dataset)

  params <- match.call(expand.dots = TRUE)

  if ( "method"  %in% names(params) ) method <- params$method else method <- "linear"
  if ( "maxgap"  %in% names(params) ) maxgap <- params$maxgap else maxgap <- 1
  if ( "rule"  %in% names(params) )   rule <- params$rule else rule <- 2
  if ( "fromLast"  %in% names(params) ) fromLast <- params$fromLast else fromLast <- TRUE
  if ( "na.rm" %in% names(params)) na.rm <- params$na.rm else na.rm <- FALSE


  to_add_back <- dataset %>%
    select ( -all_of(c("value")))

  tmp <- dataset %>%
    select ( all_of(c("time", "geo", "value", "freq" ))) %>%
    dplyr::arrange (.data$time) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value") %>%
    remove_empty_rows()

  dataset_ts <- create_time_series(tmp)
  nocb <- zoo::na.locf(dataset_ts,
                       fromLast = TRUE,
                       maxgap = maxgap)

  long_form_nocb <- as.data.frame (nocb) %>% # assigned for easier debugging
    bind_cols( tmp  %>%
                 select (
                   all_of(c("time", "freq")))
    )   %>%
    pivot_longer( cols = -all_of(c("time", "freq")),
                  names_to = 'geo',
                  values_to = 'value') %>%
    left_join ( to_add_back,
                by = c("time", "freq", "geo")
    )   %>%
    mutate ( method  = ifelse(!is.na(.data$value) & .data$method=="O",
                              "nocb", .data$method),
             obs_status  = ifelse(!is.na(.data$value)& .data$obs_status=="O",
                                "E", .data$obs_status))

  long_form_nocb
}

#' @title Forecast the dataset value
#'
#' @description Uses the \code{\link[forecast]{forecast}} function on a dataset.
#'
#' @inheritParams na_approx
#' @param forecast_periods The number of expected forecasts.  If set to default \code{NULL},
#' annual datasets will be forecasted to 3 periods, quarterly to 5 periods,
#' monthly to 12 periods and daily to 30 periods.
#' @importFrom timetk tk_ts tk_tbl
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom purrr possibly
#' @importFrom dplyr select filter left_join mutate case_when full_join bind_cols anti_join
#' @importFrom dplyr arrange
#' @importFrom stringr str_sub
#' @importFrom forecast forecast
#' @importFrom dplyr all_of
#' @importFrom assertthat assert_that
#' @return A tibble updated with with forecasted values.
#' @seealso dataset_backcast
#' @export

dataset_forecast <- function (dataset, forecast_periods = NULL) {

  test_unique_observations(dataset)

  to_add_back <- dataset %>%
    select ( -all_of(c("value")))

  freq <- unique(dataset$freq)

  if (is.null(forecast_periods)) {
    forecast_periods <- case_when (
      freq == "A" ~ 3,
      freq == "M" ~ 12,
      freq == "Q" ~ 5,
      freq == "D" ~ 30,
      TRUE ~ 5)
  }

  assertthat::assert_that(is.numeric(forecast_periods),
                          msg = "forecast_periods must be an (integer) number.")

  if (!is.null(forecast_periods)) {
    if ( freq == "A" ) {
      new_periods_df <- data.frame (
        time  = add_new_periods(indic = dataset,
                                years = forecast_periods),
        freq = freq)
    } else  if ( freq == "D" ) {
      new_periods_df <- data.frame (
        add_new_periods(indic = dataset,days = forecast_periods),
        freq = freq)
    } else {
      stop ( "Forecasting for quarterly or monthly periods is not yet implemented.")
    }
  }

  assertthat::assert_that(inherits(new_periods_df, "data.frame"),
                          msg = 'new_periods_df was not created.')

  tmp <- dataset %>%
    select ( all_of(c("time", "geo", "value", "freq" ))) %>%
    dplyr::arrange (.data$time) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  dataset_ts <- create_time_series(tmp)

  ## We cannot be sure that a time series can be forecasted.  In longitudional panel data
  ## some columns may yield a forecasts, others may not.

  possibly_forecast <- purrr::possibly(.f = forecast::forecast, NULL)

  forecast_per_geo <- apply (dataset_ts, 2, function(x) possibly_forecast(x, h = forecast_periods ))

  forecast_methods_geo <- lapply (forecast_per_geo, function(x) as.character(x$method))
  forecast_values_geo <- lapply (forecast_per_geo, function(x) as.numeric(x$mean))

  forecast_values_df <- as.data.frame(forecast_values_geo) %>%
    bind_cols ( new_periods_df ) %>%
    pivot_longer ( cols = -all_of(c("time", "freq")),
                   names_to = "geo")

  forecast_methods_df <-  as.data.frame ( forecast_methods_geo  ) %>%
    pivot_longer ( everything(),
                   names_to = "geo",
                   values_to = "method")


  new_forecasted_values <- forecast_values_df %>%
    mutate ( obs_status = 'E') %>%
    left_join (
      forecast_methods_df,
      by = "geo")   %>%
    mutate ( method   = ifelse ( is.na(.data$value), "O", paste0("Forecast ", .data$method) ),
             obs_status = ifelse ( is.na(.data$value), "O", .data$obs_status))

  tmp <- dataset %>%
    dplyr::full_join (
      dplyr::anti_join (new_forecasted_values, dataset,
                        by = c("time", "freq", "geo", "value", "obs_status", "method")
                        ),
      by = c("time", "geo", "value", "obs_status", "freq", "method")
      ) %>%
    tidyr::fill (all_of(names ( to_add_back) [! names(to_add_back) %in% names (new_forecasted_values)]))

  tmp
}

#' @title Backcast the dataset value
#'
#' @description Uses the \code{\link[forecast]{forecast}} function on a dataset.
#' @inheritParams na_approx
#' @param forecast_periods The number of expected forecasts.  If set to default \code{NULL},
#' annual datasets will be forecasted to 3 periods, quarterly to 5 periods,
#' monthly to 12 periods and daily to 30 periods.
#' @importFrom timetk tk_ts tk_tbl
#' @importFrom lubridate year
#' @importFrom tidyr pivot_wider
#' @importFrom purrr possibly
#' @importFrom dplyr select filter left_join mutate case_when full_join bind_cols anti_join
#' @importFrom dplyr arrange
#' @importFrom stringr str_sub
#' @importFrom forecast forecast
#' @importFrom dplyr all_of
#' @importFrom assertthat assert_that
#' @return A tibble updated with with forecasted values.
#' @seealso dataset_forecast
#' @export

dataset_backcast <- function (dataset, backcast_periods = NULL) {

  test_unique_observations(dataset)

  to_add_back <- dataset %>%
    select ( -all_of(c("value")))

  freq <- unique(dataset$freq)

  if (is.null(backcast_periods)) {
    backcast_periods <- case_when (
      freq == "A" ~ -3,
      freq == "M" ~ -12,
      freq == "Q" ~ -5,
      freq == "D" ~ -30,
      TRUE ~ 5)
  }

  assertthat::assert_that(is.numeric(backcast_periods),
                          msg = "forecast_periods must be an (integer) number.")

  if (!is.null(backcast_periods)) {
    if ( freq == "A" ) {
      new_periods_df <- data.frame (
        time  = add_new_periods(indic = dataset,
                                years = backcast_periods),
        freq = freq)
    } else  if ( freq == "D" ) {
      new_periods_df <- data.frame (
        add_new_periods(indic = dataset,days = backcast_periods),
        freq = freq)
    } else {
      stop ( "Forecasting for quarterly or monthly periods is not yet implemented.")
    }
  }

  assertthat::assert_that(inherits(new_periods_df, "data.frame"),
                          msg = 'new_periods_df was not created.')

  tmp <- dataset %>%
    select ( all_of(c("time", "geo", "value", "freq" ))) %>%
    dplyr::arrange (.data$time) %>%
    pivot_wider( names_from = "geo",
                 values_from = "value")

  dataset_ts <- create_time_series(tmp)

  reversed_ts <- as_tibble(apply ( dataset_ts, 2, rev ))

  dataset_ts <- create_time_series(
    bind_cols(tmp %>% select ( all_of(c("time", "freq"))), reversed_ts)
    )

  ## We cannot be sure that a time series can be forecasted.  In longitudional panel data
  ## some columns may yield a forecasts, others may not.

  possibly_forecast <- purrr::possibly(.f = forecast::forecast, NULL)

  forecast_per_geo <- apply (dataset_ts, 2, function(x) possibly_forecast(x,
                                                                           h = -backcast_periods )
                             )

  forecast_methods_geo <- lapply (forecast_per_geo, function(x) as.character(x$method))
  forecast_values_geo <- lapply (forecast_per_geo, function(x) as.numeric(x$mean))

  forecast_values_df <- as.data.frame(forecast_values_geo) %>%
    bind_cols ( new_periods_df ) %>%
    pivot_longer ( cols = -all_of(c("time", "freq")),
                   names_to = "geo")

  forecast_methods_df <-  as.data.frame ( forecast_methods_geo  ) %>%
    pivot_longer ( everything(),
                   names_to = "geo",
                   values_to = "method")


  new_forecasted_values <- forecast_values_df %>%
    mutate ( obs_status = 'E') %>%
    left_join (
      forecast_methods_df,
      by = "geo")   %>%
    mutate ( method   = ifelse ( is.na(.data$value), "O", paste0("Backcast ", .data$method)),
             obs_status = ifelse ( is.na(.data$value), "O", .data$obs_status))

  if ( nrow(new_forecasted_values) == 0 ) {
    dataset
  } else {
    tmp <- dataset %>%
      dplyr::full_join (
        dplyr::anti_join (new_forecasted_values, dataset,
                          by = c("time", "freq", "geo", "value", "obs_status", "method")
        ),
        by = c("time", "geo", "value", "obs_status", "freq", "method")
      ) %>%
      tidyr::fill (all_of(names ( to_add_back) [! names(to_add_back) %in% names (new_forecasted_values)]))

    tmp
  }


}
