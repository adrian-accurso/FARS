test_that("fars_read_years gives a warning for invalid years", {
  expect_snapshot_warning(fars_read_years(c("1592")), class = "warning", cran = FALSE, variant = NULL)
})
