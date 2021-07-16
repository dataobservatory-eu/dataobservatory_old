#' @title Create JSON String From List
#' @param list_to_json  A list to be conversted to JSON character string.
#' @importFrom jsonlite write_json
#' @return A JSON character string
#' @examples
#' create_json_text (list (id="test_id"))
#' @export
create_json_text <- function ( list_to_json ) {
  jsonlite::toJSON(list_to_json)

}

#' @rdname create_json_text
#' @param text A character string.
#' @importFrom jsonlite fromJSON validate
#' @importFrom purrr safely
#' @return A logical value, \code{TRUE} for JSON.
#' @examples
#' is.json( '{"id":["test_id"]}')
#' @export
is.json <- function (text) {
  result <- purrr::safely(jsonlite::fromJSON)(text)

  if ( is.null(result$error)) {
    jsonlite::validate(text)
  } else FALSE
}
