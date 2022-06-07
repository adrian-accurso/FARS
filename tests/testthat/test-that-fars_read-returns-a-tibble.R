test_that("fars_read produces a tibble", {
  expect_true(is.data.frame(fars_read("accident_2013.csv")))
})
