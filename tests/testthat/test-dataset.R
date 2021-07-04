data("small_population")

small_population_dataset <- dataset (x= small_population,
                                     dataset_code = "small_population_total",
                                     dataset_title = "Population of Small European Countries",
                                     freq = "A",
                                     unit = "NR",
                                     unit_name = "number")

datacite_create(small_population)

create_codebook <- function(dataset_to_code = small_population_dataset){


  obs_status <- dplyr::distinct ( dataset_to_code, .data$obs_status) %>%
    tibble::as_tibble() %>%
    rename ( id = .data$obs_status ) %>%
    mutate ( var_name = "obs_status",
             codelist = "CL_OBS_STATUS") %>%
    left_join (  cl_obs_status(), by ='id')  %>%
    mutate ( iso = NA_character_,
             iso_code = NA_character_)

  frequency <- dplyr::distinct ( dataset_to_code, .data$freq) %>%
    tibble::as_tibble() %>%
    rename ( id = .data$freq ) %>%
    mutate ( var_name = "freq",
             codelist = "CL_FREQ") %>%
    left_join (  cl_freq(), by ='id') %>%
    mutate ( iso = "8106" ) %>%
    rename ( iso_code = .data$iso8106 )
}




test_that("dataset is created", {
  expect_equal(unique(small_population_dataset$unit), "NR")
  expect_equal(unique(small_population_dataset$freq), "A")
  expect_equal(unique(small_population_dataset$dataset_code), "small_population_total")
  expect_equal(attr(small_population_dataset, "observatory"), "greendeal.dataobservatory.eu")
  expect_equal(names(attributes(small_population_dataset)),
               c("names","row.names", "class", "observatory","earliest_actual_observation",
                   "latest_actual_observation"))
})
