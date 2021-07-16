#' @title Add frequency column
#'
#' @description Add a column for \code{"A"} annual,  \code{"S"}
#' semiannual,  \code{"Q"} quarterly,  \code{"M"} daily or
#'  \code{"D"} daily data.
#' @param dat A data frame with a time variable.
#' @importFrom lubridate day month year
#' @importFrom dplyr mutate select all_of
#' @return A tibble a \code{freq} column added.
#' @examples
#' add_freq_value( data.frame (
#'                    time = as.Date(paste0(c(2019,2019,2020, 2020), "-01-01")),
#'                    geo  = c("BE", "BE", "NL", "NL"),
#'                    value = c(10,11,12,10))
#' )
#' @export

add_freq_value <- function ( dat ) {

  dat %>%
    mutate ( year  = as.integer(lubridate::year(.data$time)),
             month = as.integer(lubridate::month(.data$time)),
             day   = as.integer(lubridate::day(.data$time))
    ) %>%
    mutate ( freq = case_when (
      # establish the frequency of the data
      length( unique(.data$month ) ) ==  1 ~ "A", # if there is only one per year, annual
      length( unique(.data$month ) ) ==  2 ~ "S", # if there is only one per year, annual
      length( unique(.data$month ) ) ==  4 ~ "Q", # if there are four months present, quarterly
      length( unique(.data$day )   ) >= 28 ~ "D", # if there are at least 28 DAYS, daily
      length( unique(.data$month)  ) == 12 ~ "M", # if there are at least 12 month, monthly
      TRUE ~"unknown"
    ))   %>%
    select ( -all_of(c("year", "month", "day")))
}




