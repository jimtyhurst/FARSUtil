context("Summarize FARS data by year")

test_that("valid integer year", {
  testable <- fars_summarize_years(c(2015))
  expect_equal(dim(testable), c(12, 2))
  expect_equal(names(testable), c("MONTH", "2015"))
})

test_that("multiple years", {
  testable <- fars_summarize_years(c(2013, "2015"))
  expect_equal(dim(testable), c(12, 3))
  expect_equal(names(testable), c("MONTH", "2013", "2015"))
})

test_that("unknown year throws exception", {
  expect_error(fars_summarize_years(c("1905")))
})

test_that("non-integer year throws exception", {
  expect_error(fars_summarize_years(c("zuza")))
})
