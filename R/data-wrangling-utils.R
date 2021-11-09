#' @title Add observation status
#'
#' @description Adds the observation status \code{A} for values that are not
#' missing and \code{O} for missing ones.
#'
#' @details The SDMX Code List for Observation Status differentiates \code{O}: Missing value and
#' \code{M}: Missing value - data cannot exist. Differentiation must be made by the data curator,
#' as it requires a knowledge of the cause of missingness.
#'
#' @param dat A data frame.
#' @importFrom rlang .data
#' @importFrom glue glue
#' @importFrom dplyr mutate
#' @examples{
#' add_observation_status(
#'   data.frame (
#'        time = rep(as.Date (paste0(2010:2020, "-01-01")),2),
#'        geo = c(rep("SE", 11), rep("FI", 11)),
#'        value = c(1:10, NA_real_, NA_real_, 2:11)
#'   )
#' )
#' }
#' @keywords export

add_observation_status <-function(dat) {

  assert_that(inherits(dat, "data.frame"),
              msg = "dat must be a data.frame or inherited from data.frame."
  )

  mandatory_vars <- c("time", "geo", "value")
  missing_vars <- mandatory_vars [which(!mandatory_vars %in% names(dat))]
  missing_text <- paste(missing_vars, collapse = ", ", sep = ", ")

  assert_that( length(missing_vars)==0,
               msg = glue::glue ( "Missing variables in the dataset: {missing_text}."))

  if ( !"obs_status" %in% names(dat) ) {
    dat <- dat %>%
      mutate ( obs_status = ifelse (is.na(dat$value), "O", "A"))
  }

  if ( !"method" %in% names(dat) ) {
    dat <- dat %>%
      mutate ( method = .data$obs_status )
  }

  validate_dataframe(dat)

  dat

}


#' @title Remove empty rows
#'
#' @description Remove completely empty rows before transforming into ts object.
#'
#' @param dat A data frame, tibble or dataset object created by \code{\link{dataset}},
#' with time, geo, value, frequency, obs_status and method columns.
#' @importFrom dplyr select
#' @keywords internal

remove_empty_rows <- function(dat) {

  assertthat::assert_that('freq' %in% names(dat),
                          msg =  "Error: internal function 'remove_empty_rows' got a 'dat' object without a 'freq' column. ")

  assertthat::assert_that('time' %in% names(dat),
                          msg =  "Error: internal function 'remove_empty_rows' got a 'dat' object without a 'time' column. ")


  tmp_df <- dat  %>%
    select (
      -all_of(c("time", "freq")))

  empty_rows <- which ( rowSums(is.na(tmp_df)) == ncol(tmp_df))

  if ( length(empty_rows) == 0 ) {
    dat
  } else {
    dat[-empty_rows, ]
    }

}

#' Create Time Series Object
#'
#' Create a time series object from tmp in approximation functions.
#'
#' @param tmp A temporary dataset table created by an approximation function.
#' @importFrom timetk tk_ts
#' @importFrom dplyr case_when
#' @importFrom lubridate ymd as_date
#' @importFrom glue glue
#' @return \code{TRUE} if the test is met, otherwise and error message.
#' @keywords internal

create_time_series <- function( tmp ) {
  freq <- unique(tmp$freq)

  cts_error_msg <- glue::glue( "There are several frequency types found: {freq}. This is an error.")

  assertthat::assert_that(length(freq)==1,
                          msg = cts_error_msg )

  start_value <- lubridate::ymd(min(tmp$time))

  timetk::tk_ts(tmp,
                start = start_value,
                freq =   case_when ( freq == "A" ~ 1,
                                     freq == "Q" ~ 4,
                                     freq == "M" ~ 12),
                silent = TRUE)
}

#' Add New Periods
#'
#' Create a time series object from tmp in approximation functions.
#'
#' @param indic A temporary dataset table created by an approximation function.
#' @param years The number of years to add to the dataset's data frame.  Positive values add after the last
#' observed time, negative values add before the first observed time.
#' @param days The number of years to add to the dataset's data frame.  Positive values add after the last
#' observed time, negative values add before the first observed time.
#' @importFrom dplyr mutate
#' @importFrom purrr set_names
#' @importFrom lubridate ymd as_date days years
#' @return A new data frame with the new observation times added with missing values, labelled as
#' \code{obs_status='missing'} and \code{method='missing'}.
#' @keywords internal

add_new_periods <- function ( indic, years = NULL, days = NULL ) {

  #lubridate has no months?
  observation_time <- lubridate::as_date(indic$time)
  last_time   <- max(observation_time)
  first_time  <- min(observation_time)
  freq <- unique(indic$freq)

  if ( !is.null(years) ) {
    years <- round(years, 0)

    if (years>0) {
      new_periods <- last_time + lubridate::years(1:years)
    } else if (years<0) {
      new_periods <- first_time - lubridate::years(1:-years)
    }

    indic <- indic %>%
      dplyr::full_join (
        expand.grid(new_periods, unique(indic$geo)) %>%
          set_names (c("time", "geo")) %>%
          mutate ( freq = freq,
                   method = 'O',
                   obs_status = 'O'),
        by = c("time", "geo", "obs_status", "freq", "method")
      )
  }

  if ( !is.null(days) ) {
    days <- round(days, 0)
    if (days>0) {
      new_periods <- last_time + lubridate::days(1:days)
    } else if (days<0) {
      new_periods <- first_time - lubridate::days(1:-days)
    }
  }

  new_periods

}

#' @keywords internal
is_long_form <- function(dat) {
  if ( "geo" %in% names (dat) ) TRUE else FALSE
}


