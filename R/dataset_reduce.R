#' @title Reduce dataset
#'
#' @description Find the largest congruent sub-dataset of the dataset.
#'
#' @details The largest congruent sub-dataset of a dataset is the largest martrix of time and geo
#' obersvations that do not contain missing values.
#'
#' @param x A dataset in wide format.
#' @param return Any of \code{'data'} for the largest congruent dataset, \code{'info'} for the
#' properties of the largest congruent dataset, or \code{'both'.}
#' @return A new data frame with the largest congruent dataset, or a info about the congruent dataset,
#' or a list containing both outputs.
#' @importFrom dplyr mutate select across
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom adagio maxsub2d
#' @examples{
#' tr1 <- data.frame (
#'     time = c(2010:2020),
#'     AT = c(NA, 1:10),
#'     BG = c(NA, NA, 1:6, NA, 8:9),
#'     CZ = c(NA, 1:9, NA),
#'     DK = c(rep(NA,10), 1),
#'     EL = c(rep(NA, 11))
#' )
#' dataset_reduce(tr1, return = "both")
#' }
#' @export

dataset_reduce <- function ( x, return = "info" ) {

  if ( any(c("dataset_code", "geo", "unit") %in% names(x)) ) {
    x <- x %>%
      select ( any_of(c("time", "geo", "value", "values"))) %>%
      pivot_wider ( names_from = "geo")
  }

  df_as_m <- x %>%
    select ( -.data$time ) %>%
    mutate ( across(everything(), function(x) ifelse (is.na(x), -99999, 1)) ) %>%
    as.matrix()

  submat_data <-  df_as_m %>%
    adagio::maxsub2d()

  min_row <- submat_data$inds[1]
  max_row <- submat_data$inds[2]
  min_col <- submat_data$inds[3]+1
  max_col <- submat_data$inds[4]+1

  largest_dat <- x[min_row:max_row, c(1, min_col:max_col)]

  if ( return == "data" ) { return(largest_dat) }

  info_vector <- tibble (
    min_time = min(largest_dat$time),
    max_time = max(largest_dat$time),
    n_geo = ncol(largest_dat)-1,
    n_time = nrow(largest_dat),
    size = n_geo*n_time,
    size_pct = size/ ((ncol(x)-1)*nrow(x)),
    all_geo = paste(names(largest_dat[,-1]), collapse="|")
    )

  if ( return == "both") {
    list ( summary = info_vector,
           data    = largest_dat)
  } else if (return == "info") {
    info_vector
  } else {
    warning ("in largest_submatrix(dat, return): return must be one of 'info', 'data', or 'both'. Returning 'info'." )
    info_vector
  }
}

