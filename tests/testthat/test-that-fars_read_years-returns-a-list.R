test_that("fars_read_years returns a list", {
  expect_true(is.list(fars_read_years(c("2013"))))
})