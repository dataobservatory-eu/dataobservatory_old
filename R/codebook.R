#' @title Create a codebook
#' @return An id, a list, a data.frame or a json formatted administrative metadata record.
#' @importFrom tibble tibble
#' @importFrom dplyr distinct left_join mutate arrange bind_rows select
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @examples
#' codebook()
#' @export
codebook <- function() {
  codebook_obs_status <- cl_obs_status() %>%
    mutate ( dataset_code = "dataobservatory_consolidated_codebook",
             var_name = "obs_status")
  codebook_freq <-cl_freq() %>%
    mutate ( dataset_code = "dataobservatory_consolidated_codebook",
             var_name = "obs_status")
  codebook_methods <- cl_method() %>%
    mutate ( dataset_code = "dataobservatory_consolidated_codebook",
             var_name = "method")

  codebook <- codebook_obs_status%>%
    bind_rows ( codebook_freq) %>%
    bind_rows (codebook_methods) %>%
    mutate ( id = ifelse(is.na(.data$id), "", .data$id)) %>%
    mutate ( name = ifelse (.data$id =="", "<missing>", .data$name)) %>%
    arrange ( .data$var_name, .data$id ) %>%
    select ( all_of(c("dataset_code", "var_name", "id", "name", "description", "RelatedItem")))

  attr(codebook, "class") <- c("codebook", attr(codebook, "class"))
  attr(codebook, "Title") <- "Consolidated Coodbook For the dataobservatory R package"
  attr(codebook, "Identifier") <- "dataobservatory_consolidated_codebook"
  attr(codebook, "dataset_code" ) <- "dataobservatory_consolidated_codebook"
  attr(codebook, "updated") <- add_dates()

  codebook
}

#' @title Create a codebook for a dataset
#' @param dataset A dataset object of class dataset.
#' @return An id, a list, a data.frame or a json formatted administrative metadata record.
#' @importFrom tibble tibble
#' @importFrom dplyr distinct left_join mutate arrange bind_rows select
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @examples
#' data("small_population")
#' small_population_dataset <- dataset (
#'   x= small_population,
#'   dataset_code = "small_population_total",
#'   dataset_title = "Population of Small European Countries",
#'   freq = "A",
#'   unit = "NR",
#'   unit_name = "number")
#'
#' codebook_dataset(small_population_dataset)
#' @export
codebook_dataset <- function(dataset) {

  assertthat:::assert_that(
    is.dataset(dataset),
    msg = "The dataset must be of class dataset. See ?dataset"
  )

  dataset_metadata <- attributes (dataset)

  codebook <- tibble (
    dataset_code = attr(dataset, "dataset_code"),
    var_name = "unit",
    id = unique(dataset$unit),
    name = attr(dataset, "unit_name"),
    description = NA_character_,
    RelatedItem = NA_character_)

  if (! "method" %in% names (dataset)) {
    dataset$method <- dataset$obs_status
  }

  codebook_obs_status <- dataset %>%
    distinct ( .data$dataset_code, .data$obs_status) %>%
    pivot_longer ( -all_of("dataset_code"),
                   names_to = "var_name",
                   values_to  = "id") %>%
    left_join ( cl_obs_status(), by ="id")

  codebook_freq <- dataset %>%
    distinct ( .data$dataset_code, .data$freq) %>%
    pivot_longer ( -all_of("dataset_code"),
                   names_to = "var_name",
                   values_to  = "id") %>%
    left_join (   cl_freq(),
                  by ="id" )

  codebook_methods <- dataset  %>%
    distinct ( .data$dataset_code, .data$method) %>%
    pivot_longer ( -all_of("dataset_code"),
                   names_to = "var_name",
                   values_to  = "id") %>%
    left_join ( cl_method(),  by = c("id") )

  codebook <- codebook %>%
    bind_rows ( codebook_obs_status) %>%
    bind_rows ( codebook_freq) %>%
    bind_rows (codebook_methods) %>%
    mutate ( id = ifelse(is.na(.data$id), "", .data$id)) %>%
    mutate ( name = ifelse (.data$id =="", "<missing>", .data$name)) %>%
    arrange ( .data$var_name, .data$id ) %>%
    select ( all_of(c("dataset_code", "var_name", "id", "name", "description", "RelatedItem")))

  attr(codebook, "class") <- gsub("dataset", "codebook", attr(codebook, "class"))
  attr(codebook, "Title") <- paste0( "Codebook for ", dataset_metadata $Title)
  attr(codebook, "Identifier") <- paste0( "codebook_", dataset_metadata $dataset_code)
  attr(codebook, "dataset_code" ) <- dataset_metadata $dataset_code
  attr(codebook, "updated") <- dataset_metadata$updated

  codebook
}


#' @rdname codebook_dataset
#' @export
is.codebook <- function (x) inherits(x, "codebook")

#' @rdname codebook_dataset
#' @param n How many rows to print?
#' @importFrom dplyr mutate_all everything
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#' @importFrom jsonlite fromJSON
#' @export
print.codebook <- function(x, n = 24, ... ) {

  codebook_metadata <- attributes ( x )

  if (is.null(n)) n <- nrow(x)
  if (is.na(n)) n <- nrow(x)

  if ( n>nrow(x)) n <- nrow(x)

  main_title <- codebook_metadata $Title

  cat("Codebook information for", main_title, "\n")

  to_print <- x[1:n,]

  print(as_tibble(to_print))

  further_entries <- nrow(x)-n

  if(further_entries >0) {
    cat(paste0("Only the first ", n, " entries are printed.\n"))
    cat(paste0("There are further ", further_entries, " entries in the codebook.\n"))
  }

}


