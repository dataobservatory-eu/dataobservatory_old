#' Find frequency of the data
#'
#' @param dat A data frame with a time variable.
#' @importFrom lubridate day month year
#' @importFrom dplyr mutate select all_of
#' @return A tibble with fixed form.
#' @keywords internal
#'

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
