
add_related_items <- function (
  RelatedItem,
  relatedItemType = "Dataset",
  relationType = "IsDerivedFrom",
  relatedItemIdentifier,
  relatedItemIdentifierType,
  related_items = NULL,
  format = 'json' ) {

  resource_types <- resource_types_datacite()
  relation_types <- relation_type_datacite()
  related_id_types <- related_identifiers_datacite()

  not_one_of_id_types <- paste (related_id_types, collapse = ", ")

  assertthat::assert_that(
    length(relatedItemIdentifierType) == 1 & tolower(relatedItemIdentifierType) %in% tolower(related_id_types),
    msg = glue::glue("relatedItemType must be one of {not_one_of_id_types}")
  )

  not_one_of_resources <- paste (resource_types, collapse = ", ")

  assertthat::assert_that(
    length(relatedItemType) == 1 & relatedItemType %in% resource_types,
    msg = glue::glue("relatedItemType must be one of {not_one_of_resources}")
  )

  not_one_of_relations <- paste (relation_types, collapse = ", ")

  assertthat::assert_that(
    length(relationType) == 1 & relationType %in% relation_types,
    msg = glue::glue("relationType must be one of {not_one_of_relations}")
  )


  RelatedItem <- tibble::tibble (
    scheme = relatedItemIdentifierType,
    id = relatedItemIdentifier,
    relationType = relationType,
    relatedItemType = relatedItemType
  )

  RelatedItem.JSON <- jsonlite::toJSON (jsonlite::unbox(RelatedItem))


  if ( !is.null(related_items) ) {
    if (all(vapply(related_items, is.json, logical(1)))) {

      if ( is.json(related_items) ) {
        RelatedItem  <- paste(c(RelatedItem.JSON, related_items), collapse = ", ")
      }

      if ( format == "list") {
        append(list(related_items), list(RelatedItem))
      } else {
        RelatedItem
      }
    }
  } else if ( format == "list") {
    RelatedItem
  }  else if ( format=="json") {
    RelatedItem.JSON
  } else{
    stop("The parameter='format' must be either 'list' or 'json'")
  }
}

first_rel_item <- add_related_items (
  RelatedItem = "First Related Item",
  relatedItemType = "Dataset",
  relationType = "IsDerivedFrom",
  relatedItemIdentifierType = "Handle",
  relatedItemIdentifier = "first_rel_item",
  related_items = NULL,
  format = 'json' )

cat(first_rel_item)

second_rel_item <- add_related_items (
  RelatedItem = "Second Related Item",
  relatedItemType = "Dataset",
  relationType = "IsDerivedFrom",
  relatedItemIdentifierType = "Handle",
  relatedItemIdentifier = "second_rel_item",
  related_items = first_rel_item,
  format = 'json' )


test_that("add_description", {
  expect_true(is.json(add_description(Abstract = "My Description")))
})


test_that("add_related_items", {
  expect_true(is.json(second_rel_item))
})

