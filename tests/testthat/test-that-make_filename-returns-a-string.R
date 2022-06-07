test_that("make_filename returns a string", {
  expect_true(grepl(make_filename("2013"),"accident_2013.csv"))
})
