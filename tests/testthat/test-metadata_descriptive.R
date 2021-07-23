
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
  relatedItemIdentifierType = "URL",
  relatedItemIdentifier = "second_rel_item",
  related_items = first_rel_item,
  format = 'json' )


test_that("add_description", {
  expect_true(is.json(add_description(Abstract = "My Description")))
})


test_that("add_related_items", {
  expect_true(is.json(second_rel_item))
})

