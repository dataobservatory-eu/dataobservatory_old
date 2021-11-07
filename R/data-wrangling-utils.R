#' Remove empty rows
#'
#' Remove completely empty rows before transforming into ts object.
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

#' Reduce dataset to complete
#'
#' Find the largest complete subset of the dataset
#'
#' @param df A dataset in wide format.
#' @return A new data frame with the largest complete subset.
#' @keywords internal

reduce_dataset <- function ( df ) {
  missing_by_time <- rowSums(is.na(df))==ncol(df)-1
  missing_by_geo <- colSums(is.na(df))==nrow(df)

  df <- df[!missing_by_time, ]
  df <- subset(df, select = ! missing_by_geo)

  repeat {
    missing_by_time <- rowSums(is.na(df))
    available_by_time <- rowSums(!is.na(df))
    missing_by_geo <- colSums(is.na(df))
    available_by_geo <- colSums(!is.na(df[,-1]))
    max_missing <- max(missing_by_time, missing_by_geo)
    min_available <- min (available_by_geo, available_by_time)


    if (max_missing == 0) {
      return(df)
      break
    } else {
      to_remove_time <- which(max_missing == missing_by_time)  #start with time
      to_remove_geo  <- which(max_missing == missing_by_geo)   #start with geo

      min_available_geo <- which (available_by_geo == min_available )
      min_available_time <- which (available_by_time == min_available )

      remove_time_n <- length(min_available_time)
      remove_geo_n  <- length(min_available_geo)

      if ( remove_time_n > 0 ) {
        df <- df[-min_available_time,]
      }

      if ( remove_geo_n >0 ) {
        df <- df[, -(as.integer(min_available_geo)+1)]
      }
    }
  }
}
