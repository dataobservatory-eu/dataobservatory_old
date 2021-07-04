#' @title Create JSON String From List
#' @param list_to_json  A list to be conversted to JSON character string.
#' @importFrom jsonlite write_json
#' @return A JSON character string
#' @examples
#' create_json_text (list (id="test_id"))
#' @export
create_json_text <- function ( list_to_json ) {
  tmp <- tempfile()
  jsonlite::write_json(list_to_json, tmp)
  my_json <- readLines(tmp)
  my_json
}

#' @rdname create_json_text
#' @param text A character string.
#' @importFrom jsonlite fromJSON
#' @importFrom purrr safely
#' @return A logical value, \code{TRUE} for JSON.
#' @examples
#' is.json( '{"id":["test_id"]}')
#' @export
is.json <- function (text) {
  result <- purrr::safely(jsonlite::fromJSON)(text)

  if ( is.null(result$error)) TRUE else FALSE
}
