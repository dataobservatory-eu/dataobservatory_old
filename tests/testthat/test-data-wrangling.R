example_df  <- data.frame (
  time = rep(as.Date (paste0(2010:2020, "-01-01")),2),
  geo = c(rep("SE", 11), rep("FI", 11)),
  value = c(1:10, NA_real_, NA_real_, 2:11)
  )

added <- add_observation_status(example_df)

test_that("observation_status is added correctly", {
  expect_true(all(added$obs_status == c(rep("A", 10), rep("O", 2), rep("A", 10))))
})

test_that("method is added correctly", {
  expect_true(all(added$method == c(rep("A", 10), rep("O", 2), rep("A", 10))))
})

test_that("All mandatory columns are checked", {
  expect_error(add_observation_status(example_df[,-1]))
  expect_error(add_observation_status(example_df[,-2]))
  expect_error(add_observation_status(example_df[,-3]))
})
