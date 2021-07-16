


test_that("add_description", {
  expect_true(is.json(add_description(Abstract = "My Description")))
})




first_rel_item <- add_related_items (
  RelatedItem = "First Related Item",
  relatedItemType = "Dataset",
  relationType = "IsDerivedFrom",
  relatedItemIdentifier = add_identifiers (id = "first_rel_item"),
  related_items = NULL,
  format = 'json' )


second_rel_item <- add_related_items (
  RelatedItem = "Second Related Item",
  relatedItemType = "Dataset",
  relationType = "IsDerivedFrom",
  relatedItemIdentifier = add_identifiers (id = "second_rel_item"),
  related_items = first_rel_item,
  format = 'json' )
