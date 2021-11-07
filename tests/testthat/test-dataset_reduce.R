tr1 <- data.frame (
  time = c(2010:2020),
  AT = c(NA, 1:10),
  BG = c(NA, NA, 1:6, NA, 8:9),
  CZ = c(NA, 1:9, NA),
  DK = c(rep(NA,10), 1),
  EL = c(rep(NA, 11))
)


dfr <- dataset_reduce (x=tr1)

test_that("reduction left no NA values", {
  expect_true(sum(is.na(dfr))==0)
})



