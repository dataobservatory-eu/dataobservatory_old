

default_related_items <- function(source = "eurostat") {

  relitems <- add_related_items(
    RelatedItem = "dataobservaotry",
    relatedItemType = "Software",
    relationType = "isCompiledBy",
    relatedItemIdentifier = add_identifiers(
      id = "dataobservatory",
      DOI = "10.5281/zenodo.5034752",
      URI = "https://r.dataobservatory.eu/")
  )

  relitems <- add_related_items(
    RelatedItem = "Retrieval and Analysis of Eurostat Open Data with the eurostat Package ",
    relatedItemType = "Software",
    relationType = "isCompiledBy",
    relatedItemIdentifier = add_identifiers(
      id = "eurostat",
      URI = "https://ropengov.github.io/eurostat/"),
    related_items = relitems
  )

  create_json_text ( relitems )
}
