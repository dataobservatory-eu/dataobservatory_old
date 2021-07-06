#' Test Unique Observations
#'
#' Tidy datasets have observations that are unique.
#'
#' Approximation and other filling techniques require unique observations.
#'
#' @param dataset A data frame or a dataset table to test.
#' @importFrom dplyr select group_by add_count filter distinct_all
#' @importFrom rlang .data
#' @return \code{TRUE} if the test is met, otherwise \code{FALSE}.
#' @examples{
#' test_dataset <- data.frame (
#'    geo = c("DE", "DE", "CH", "CH"),
#'    value = 1:4,
#'    time = as.Date(paste0(2020:2021, "-01-01")),
#'    obs_status = rep("actual", 4)
#'  )
#' is_unique_observations(test_dataset)
#' }
#' @export

is_unique_observations <- function( dataset ) {

  assertthat::assert_that(
    all ( c("geo", "time", "value", "obs_status") %in% names(dataset)),
    msg = "The dataset must have geo, time, value, obs_status variable columns."
  )

  if ( "unit" %in% names (dataset) ) {
    assertthat::assert_that(
      length(unique(dataset$unit)) < 2,
      msg = "The dataset must have a unique unit."
    )
  }

  if ( "unit_name" %in% names (dataset) ) {
    assertthat::assert_that(
      length(unique(dataset$unit_name)) < 2,
      msg = "The dataset must have a unique unit_name."
    )
  }

  uniqueness <- dataset %>%
    dplyr::select ( all_of(c("geo", "time", "value", "obs_status")) ) %>%
    dplyr::group_by ( .data$geo, .data$time, .data$value ) %>%
    dplyr::add_count() %>%
    filter ( .data$n != 1 )

  ifelse (nrow(uniqueness)==0, TRUE, FALSE)

}

#' Test Unique Observations for Approximation functions.
#'
#' Tidy datasets have observations that are unique. They must have on of the three
#' types of values: an actual, an value_typed or a missing value.
#'
#' Approximation and other filling techniques require unique observations.
#'
#' This is an internal function and can give either a warning or
#'
#' @param dataset An dataset table to test.
#' @param stop_on_error Defaults to \code{TRUE} when the code stops with an error
#' message.  If \code{FALSE}, it displays non-unique values.
#' @importFrom dplyr select group_by add_count filter distinct_all
#' @importFrom assertthat assert_that
#' @importFrom utils head
#' @return \code{TRUE} if the test is met, otherwise warning if \code{stop_on_error = FALSE}
#' and returns \code{FALSE} or stops with an error if \code{stop_on_error = TRUE}.
#' @keywords internal

test_unique_observations <- function( dataset, stop_on_error = FALSE ) {

  ifelse ( is_unique_observations(dataset),
           return(TRUE), ifelse ( stop_on_error,
                          stop("Non-unique values found in ", head(dataset)),
                          warning("Non-unique values found in ", head(dataset))) )

  FALSE
}


