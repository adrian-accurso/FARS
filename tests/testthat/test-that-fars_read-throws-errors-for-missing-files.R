test_that("fars_read throws errors", {
  expect_error(fars_read("dude.csv"),"file 'dude.csv' does not exist")
})
