data("small_population")
small_population_dataset <- dataset (x= small_population,
                                     dataset_code = "small_population_total",
                                     dataset_title = "Population of Small European Countries",
                                     freq = "A",
                                     unit = "NR",
                                     unit_name = "number")


benelux_linear <- data.frame (
  time = as.Date (paste0(2010:2020, "-01-01")),
  NL = seq ( 30, 50, by = 2),
  BE = seq ( 35, 45, by = 1),
  LU = seq ( 2, 22, by = 2)
  )


benelux_linear$NL[c(6, 10:11)] <- NA_real_
benelux_linear$BE[c(1:3, 10)] <- NA_real_
benelux_linear$LU[c(4:6)] <- NA_real_

benelux_linear_long <- tidyr::pivot_longer( benelux_linear,
                                            cols = all_of(c("NL", "BE", "LU")),
                                            names_to = "geo", values_to = "value") %>%
  mutate ( obs_status = ifelse (is.na(value), "O", "A")) %>%
  mutate ( freq = "A",
           method = obs_status )

test_that("unique indicator value tests", {
  expect_true(test_unique_observations (benelux_linear_long))
  expect_warning(test_unique_observations (
    x =  rbind (benelux_linear_long, benelux_linear_long)
  ))
  expect_error(test_unique_observations (
    x =  rbind (benelux_linear_long, benelux_linear_long), stop_on_error = TRUE
  ))
})

approximated_benelux_values <- na_approx (dataset=benelux_linear_long)

bl_approximated_values <- approximated_benelux_values  %>%
  select ( -all_of(c("obs_status", "method"))) %>%
  pivot_wider( names_from = "geo", values_from = "value")

test_that("linear approximation works", {
  expect_true (
    # correct OBS_STATUS
    all(approximated_benelux_values$obs_status %in% c("A", "O", "E")))
  expect_equal(bl_approximated_values %>%
                 filter ( time == as.Date("2015-01-01") ) %>%
                 select ( "NL") %>% as.numeric(), 40)
  expect_equal(bl_approximated_values %>%
                 filter ( time >=as.Date("2013-01-01"),
                          time <=as.Date("2015-01-01")) %>%
                 select ( "LU") %>% unlist() %>% as.numeric(), c(NA_real_, NA_real_, NA_real_))
  expect_true(is.na(bl_approximated_values$NL[11]))
})

approximated_benelux_values_maxgap_3 <- na_approx (dataset=benelux_linear_long, maxgap = Inf )

bl_approximated_values_maxgap_3 <- approximated_benelux_values_maxgap_3  %>%
  select ( -all_of(c("obs_status", "method"))) %>%
  pivot_wider( names_from = "geo", values_from = "value")


test_that("linear approximation works with ...", {
  expect_equal(bl_approximated_values_maxgap_3 %>%
                 filter ( time >=as.Date("2013-01-01"),
                          time <=as.Date("2015-01-01")) %>%
                 select ( "LU") %>% unlist() %>% as.numeric(), c(8,10,12))
})


locf_benelux_values <- na_locf(approximated_benelux_values, maxgap = 2)

bl_locf_values <- locf_benelux_values  %>%
  select ( -all_of(c("obs_status", "method"))) %>%
  pivot_wider( names_from = "geo", values_from = "value")

locf_labels <- locf_benelux_values %>%
  filter ( geo  == "NL") %>%
  select ( all_of(c("time", "value", "method", "obs_status")))


test_that("locf works", {
  expect_equal(bl_locf_values %>%
                 filter ( time >= as.Date("2019-01-01") ) %>%
                 select ( "NL") %>% unlist() %>% as.numeric(), c(46,46))
  expect_equal(bl_locf_values %>%
                 filter ( time < as.Date("2013-01-01")) %>%
                 select ( "BE") %>% unlist() %>% as.numeric(),rep(NA_real_,3))
  expect_true (
    # correct OBS_STATUS
    all(locf_labels$obs_status %in% c("A", "O", "E")))
  expect_true (
    # correct OBS_STATUS
    all(locf_labels$method %in% c("A", "O", "approx", "locf")))
  expect_equal(# NL labeling
               locf_labels$method,
               c(rep("A",5), "approx", rep("A", 3), c("locf", "locf"))
               )
})

nocb_benelux_values <- na_nocb(locf_benelux_values, maxgap= 3)


bl_nocb_values <- nocb_benelux_values  %>%
  select ( -all_of(c("obs_status", "method"))) %>%
  pivot_wider( names_from = "geo", values_from = "value")

nocb_labels <- nocb_benelux_values  %>%
  filter ( geo  == "BE") %>%
  select ( all_of(c("time", "value", "method", "obs_status")))

test_that("nocb works", {
  expect_equal(bl_nocb_values %>%
                 filter ( time >= as.Date("2019-01-01") ) %>%
                 select ( "NL") %>% unlist() %>% as.numeric(), c(46,46))
  expect_equal(bl_nocb_values %>%
                 filter ( time < as.Date("2013-01-01")) %>%
                 select ( "BE") %>% unlist() %>% as.numeric(),
               rep(38,3))
  expect_true (
    # correct OBS_STATUS
    all(nocb_labels$obs_status %in% c("A", "O", "E")))
  expect_true (
    # correct OBS_STATUS
    all(nocb_labels$method %in% c("A", "O", "approx", "nocb")))
  expect_equal(# Belgian labeling
    nocb_labels$method,
    c(rep("nocb",3), rep("A", 6), c("approx", "A"))
  )
})

forecasted_3 <- dataset_forecast(
  dataset =  nocb_benelux_values,
  forecast_periods = 3)

forecasted_be <- forecasted_3[forecasted_3$geo == "BE", ]

test_that("forecaseting works", {
  expect_equal(max(forecasted_3$time),as.Date ( "2023-01-01" ))
  expect_true(unlist(forecasted_be [ forecasted_be$time == as.Date ("2021-01-01"), "method"]) == "ETS(A,A,N)")
  expect_equal(as.numeric(unlist(forecasted_be [ forecasted_be$time == as.Date ("2021-01-01"), "value"])), 46)
  expect_equal(as.character(unlist(forecasted_be [ forecasted_be$time == as.Date ("2021-01-01"), "obs_status"])), "E")
})



