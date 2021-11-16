#' @title dataset for an Automated Data Observatory
#' @description The class inherits all methods from a data frame, but has
#' many fixed attributes.
#'
#' @details
#' The attributes of the dataset are
#' \describe{
#'   \item{dataset_code}{The dataset_code id.}
#'   \item{Title}{The title of the dataaset.}
#'   \item{source}{The data source information.}
#'   \item{unit_name}{The name of the unit variable, which is recorded by its id in the dataset.}
#'   \item{sessionInfo}{The session information added by \code{\link{add_sessioninfo}}.}
#' }
#' @param x A data frame
#' @param dataset_code A unique dataset id code.
#' @param dataset_title A title, which should be the main Title if there are several titles
#' given by \code{\link{add_titles}}.
#' @param freq A frequency variable coded with \code{\link{add_frequency}}.
#' @param unit A standardized unit id.
#' @param unit_name A unit name.
#' @param source A source, currently defaults to \code{"greendeal.dataobservatory.eu"}.
#' @param add_obs_status Defaults to \code{TRUE}, when missing \code{obs_status} is added with
#' \code{\link{add_observation_status}}.
#' @return A validated dataset object.
#' @importFrom assertthat assert_that
#' @importFrom tibble as_tibble
#' @importFrom stringr word
#' @importFrom dplyr rename
#' @examples
#'
#' my_dataset <- dataset (
#'      x =data.frame ( time = c(2019,2019,2020,2020),
#'                 geo = c("BE", "BE", "NL", "NL"),
#'                 value = c(10,11,NA,13)),
#'      dataset_code = "test_code",
#'      dataset_title = "Test dataset",
#'      freq = "A",
#'      unit = "NR", unit_name = "Number"
#' )
#' is.dataset(my_dataset)
#'
#' print(my_dataset,5)
#' @export

dataset <- function(x,
                    dataset_code,
                    dataset_title,
                    freq,
                    unit,
                    unit_name,
                    source = "greendeal.dataobservatory.eu",
                    doi = NULL,
                    add_obs_status = TRUE ) {


  assertthat::assert_that(inherits(x, "data.frame"),
                          msg = "x must be a data.frame or inherited from data.frame.")

  assert_that(nrow(x)>0,
              msg = "dat must have at least one observation."
  )


  if ( "values" %in% names(x)) {
    x <- rename (x, value = .data$values)
  }

  if ( add_obs_status == TRUE ) {
    ## Add missing obs_status and method columns, if they are not present
    x <- add_observation_status(x)
  } else {
    validate_dataframe(x)
  }

  new_dataset (x = x,
               dataset_code  = dataset_code,
               dataset_title = dataset_title,
               freq = freq,
               unit = unit,
               unit_name     = unit_name,
               source        = source,
               doi = doi)

}

#' @title Validate a data frame
#' @importFrom assertthat assert_that
#' @keywords internal
validate_dataframe <- function ( dat ) {

  assert_that(inherits(dat, "data.frame"),
              msg = "dat must be a data.frame or inherited from data.frame."
              )

  assert_that(nrow(dat)>0,
              msg = "dat must have at least one observation."
  )


  mandatory_vars <- c("time", "geo", "value",
                      "obs_status", "method")

  missing_vars <- mandatory_vars [which(!mandatory_vars %in% names(dat))]
  missing_text <- paste(missing_vars, collapse = ", ", sep = ", ")

  assert_that( length(missing_vars)==0,
               msg = glue::glue ( "Missing variables in the dataset: {missing_text}."))

  assert_that(inherits(dat, "data.frame"),
              msg = "dat must be a data.frame or inherited from data.frame."
  )

  assert_that(is.character(dat$geo),
              msg = "dat$geo must be a character vector."
  )

  assert_that(is.numeric(dat$value),
              msg = "dat$value must be a numeric vector."
  )

  assert_that(is.numeric(dat$time)|is.integer(dat$time)|inherits(dat$time, "Date")|inherits(dat$time, "POSIXt"),
              msg = "dat$time must be a Date or integer vector, or inherited from POSIXct."
  )

  if ( is.numeric(dat$time)) {
    assert_that( all((dat$time  %% 1) == 0),
                msg = "If dat$time is a numeric, it must contain only integer values."
                )
  }

  valid_obs_status <- cl_obs_status()$id
  valid_obs_status_string <- paste(valid_obs_status, collapse = "', '")

  assert_that ( all( dat$obs_status %in% valid_obs_status),
                msg = glue::glue("Valid value types are '{valid_obs_status_string }'.") )

}

#' @rdname dataset
#' @export
is.dataset <- function (x) inherits(x, "dataset")

#' @rdname dataset
#' @param n The number of observations to print.
#' @export
print.dataset <- function(x, n = 10, ... ) {

  if (nrow(x)<n ) n <- nrow(x)

  cat(paste0(attr(x, "Title"), "\n"))
  cat ("dataset code: ", attr(x, "dataset_code"), "\n")

  cat(paste0("Actual observation range: [", attr(x, "earliest_actual_observation"), "]-[",
             attr(x, "latest_actual_observation"), "], updated on ", attr(x, "updated"), ".\n"))

  coverage <- unique(x$geo)
  if ( length(coverage)>n) {
    coverage_text <- paste0(paste(coverage[1:n], collapse = ", "), " ... and further ", length(coverage)-10, " geographical units.")
  } else {
    coverage_text <- paste(coverage, collapse = ", ")
  }

  cat("Geographic coverage: ", coverage_text , "\n")
  n_observations <- nrow(x)

  if ( n_observations > n ) {
    cat (paste0("The first ", n, " observations of ", n_observations, " (unit: ", attr(x, "unit_name"), "):\n\n" ))
    print(head(as.data.frame(x),n))
  } else {
    print(as.data.frame(x))
  }

  cat(paste0('\nSource: ', attr(x, "source")), "\n")
}

#' @rdname dataset
#' @export
summary.dataset <- function(x, ...) {

  cat(paste0(attr(x, "Title"), "\n"))
  cat ("dataset code: ", attr(x, "dataset_code"), "\n")
  cat(paste0("Actual observation range: [", attr(x, "earliest_actual_observation"), "]-[",
             attr(x, "latest_actual_observation"), "], updated on ", attr(x, "updated"), ".\n"))

   coverage <- unique(x$geo)
   if (length(coverage)>10) {
     coverage_text <- paste0(paste(coverage[1:10], collapse = ", "), " ... and further ", length(coverage)-10, " geographical units.")
   } else {
   coverage_text <- paste(coverage, collapse = ", ")
  }

  cat("Geographic coverage: ", coverage_text , "\n\n")

  print(summary(subset(tibble::as_tibble(x), select = c("time", "value"))))
  cat(paste0('Source: ', attr(x, "source")), "\n")

}

#' @inheritParams dataset
new_dataset <- function(x,
                        dataset_code,
                        dataset_title,
                        freq,
                        unit,
                        unit_name,
                        source,
                        doi) {

  x$dataset_code <- dataset_code

  new_dataset <- x %>%
    select (any_of(c("dataset_code", "time", "geo", "value", "unit",
                      "obs_status", "method", "freq")))

  new_dataset$unit        <- unit
  new_dataset$freq        <- freq
  attr(new_dataset, "dataset_code") <- dataset_code
  attr(new_dataset, "Title") <- dataset_title
  attr(new_dataset, "source") <- source
  attr(new_dataset, "unit_name") <- unit_name
  class(new_dataset) <- c("dataset", class(new_dataset) )

  actual_observations <- new_dataset[ new_dataset$obs_status == "A", ]
  attr(new_dataset, "updated") <- Sys.Date()
  attr(new_dataset, "earliest_actual_observation") <- min(actual_observations$time, na.rm=TRUE)
  attr(new_dataset, "latest_actual_observation")   <- max(actual_observations$time, na.rm=TRUE)

  attr(new_dataset, "sessionInfo") <- add_sessioninfo()

  if(!is.null(doi)) attr(new_dataset, "DOI") <- doi

  new_dataset
}



