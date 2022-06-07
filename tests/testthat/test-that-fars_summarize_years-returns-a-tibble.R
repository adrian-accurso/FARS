test_that("fars_summarize_years produces a tibble", {
  expect_true(is.data.frame(fars_summarize_years(c("1983","1993","2013"))))
})
