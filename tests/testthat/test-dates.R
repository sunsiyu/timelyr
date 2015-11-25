context("Test Dates Creation")

test_that("create dates works", {
  expect_equal(create_dates("2015-01-01", "2015-02-01", by="1 day"),
               seq(as.Date("2015-01-01"), as.Date("2015-02-01"), by="1 day"))
})
