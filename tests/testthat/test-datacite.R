data("small_population")

small_population_dataset <- dataset (x= small_population,
                                     dataset_code = "small_population_total",
                                     dataset_title = "Population of Small European Countries",
                                     freq = "A",
                                     unit = "NR",
                                     unit_name = "number")
add_contributors("Joe", "Doe", "DataCurator", "Green Deal Data Observatory")


data("datacite_properties")

mandatory_properties <- datacite_properties$Property [ datacite_properties$Obligation == "M"]

small_pop_datacite <- datacite (
  dataset_code = "small_population_total",
  Title = "Population on 1 January by age and sex",
  Subject = "demography",
  Creator = add_creators("Jane", "Doe", "Reprex"),
  Contributor = add_contributors("Joe", "Doe",
                                 "DataCurator", "Green Deal Data Observatory",
                                 format = 'json')
)

subsetted <- subset ( small_pop_datacite, select = mandatory_properties)


small_pop_datacite_2 <- datacite_dataset (small_population_dataset,
                                          dataset_code = "small_population_total",
                                          Title = "Population on 1 January by age and sex",
                                          Subject = "demography",
                                          Creator = add_creators("Jane", "Doe", "Reprex"),

                                          Contributor = add_contributors("Joe", "Doe",
                                                                         "DataCurator", "Green Deal Data Observatory",
                                                                         format = 'json')
)


test_that("datacite work", {
  expect_equal(names(small_pop_datacite), c("dataset_code", datacite_properties$Property))
  expect_equal(small_pop_datacite$Title, "Population on 1 January by age and sex")
  expect_equal(sum(unlist(lapply(subsetted, is.na))), 0)
})

test_date_string <- paste0('{\"Updated\":[\"',
                           as.character(Sys.Date()),
                           '\"],\"EarliestObservation\":[\"2014-01-01\"],\"LatestObservation\":[\"2020-01-01\"]}')

test_that("datacite_dataset work", {
  expect_equal(names(small_pop_datacite_2), datacite_names)
  expect_equal(small_pop_datacite_2$Title, "Population on 1 January by age and sex")
})

small_pop_derived_from <- add_identifiers(
  id = 'demo_pjang',
  URI = 'https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en'
)

regions_package <- add_identifiers(
  id = 'regions: Processing Regional Statistics',
  Version = 'R package version 0.1.8',
  URI = 'https://regions.dataobservatory.eu/'
)

item1 <- add_related_items(
  RelatedItem = "Small Population",
  relatedItemType = "Dataset",
  relationType = "IsDerivedFrom",
  relatedItemIdentifier = small_pop_derived_from,
  related_items = NULL,
  format = "json")




relitems <- add_related_items (
  RelatedItem = "regions R package",
  relatedItemType = "Software",
  relationType = "isCompiledBy",
  relatedItemIdentifier = add_identifiers(
    id = "regions",
    URI = "https://regions.dataobservatory.eu/"
  ),
  format = "json",
  related_items = item1 )

frequency_a <- add_frequency("A", admin_format = "list")
frequency_a$RelatedItem <- NULL

test_that("administrative entries work", {
  expect_equal(frequency_a,
               list ( id = "A", name = "Annual",
                      description = "To be used for data collected or disseminated every year",
                      iso8106 ="P1Y"))
})

test_that("exceptions are handled", {
 expect_error(add_frequency("error"))
})

#Population on 1 January by age and sex
#https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en


