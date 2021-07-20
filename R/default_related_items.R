

default_related_items <- function(source = "eurostat") {

  relitems <- add_related_items(
    RelatedItem = "dataobservaotry",
    relatedItemType = "Software",
    relationType = "isCompiledBy",
    relatedItemIdentifier = "10.5281/zenodo.5034752",
    relatedItemIdentifierType = "DOI"
  )

  relitems <- add_related_items(
    RelatedItem = "Retrieval and Analysis of Eurostat Open Data with the eurostat Package ",
    relatedItemType = "Software",
    relationType = "isCompiledBy",
    relatedItemIdentifier = "https://ropengov.github.io/eurostat/",
    relatedItemIdentifierType = "URL",
    related_items = relitems
  )

  relitems
}
