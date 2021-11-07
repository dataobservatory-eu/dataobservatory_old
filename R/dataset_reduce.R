#' @title Reduce dataset
#'
#' @description Find the largest complete subset of the dataset.
#'
#' @param x A dataset in wide format.
#' @return A new data frame with the largest complete subset.
#' @exaples{
#' tr1 <- data.frame (
#'     time = c(2010:2020),
#'     AT = c(NA, 1:10),
#'     BG = c(NA, NA, 1:6, NA, 8:9),
#'     CZ = c(NA, 1:9, NA),
#'     DK = c(rep(NA,10), 1),
#'     EL = c(rep(NA, 11))
#' )
#' dataset_reduce(tr1)
#' }
#' @export

dataset_reduce <- function ( x ) {

  if (is_long_form(x)){
    x <- x %>%
      select ( all_of(c("time", "geo", "value"))) %>%
      pivot_wider ( names_from = .data$geo )
  }

  missing_by_time <- rowSums(is.na(x))==ncol(x)-1
  missing_by_geo <- colSums(is.na(x))==nrow(x)

  x <- x[!missing_by_time, ]
  x <- subset(x, select = ! missing_by_geo)

  repeat {
    missing_by_time <- rowSums(is.na(x))
    available_by_time <- rowSums(!is.na(x))
    missing_by_geo <- colSums(is.na(x))
    available_by_geo <- colSums(!is.na(x[,-1]))
    max_missing <- max(missing_by_time, missing_by_geo)
    min_available <- min (available_by_geo, available_by_time)


    if (max_missing == 0) {
      return(x)
      break
    } else {
      to_remove_time <- which(max_missing == missing_by_time)  #start with time
      to_remove_geo  <- which(max_missing == missing_by_geo)   #start with geo

      min_available_geo <- which (available_by_geo == min_available )
      min_available_time <- which (available_by_time == min_available )

      remove_time_n <- length(min_available_time)
      remove_geo_n  <- length(min_available_geo)

      if ( remove_time_n > 0 ) {
        x <- x[-min_available_time,]
      }

      if ( remove_geo_n > 0 ) {
        x <- x[, -(as.integer(min_available_geo)+1)]
      }
    }
  }
}
