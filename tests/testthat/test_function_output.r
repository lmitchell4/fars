library(fars)
context("Function output")

test_that("make_filename returns correct string", {  
  expect_is(make_filename(2013),"character")
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})

test_that("fars_read returns correct object type", {
  expect_is(fars_read(filename="accident_2013.csv.bz2"), "tbl_df")
})

test_that("fars_read_years returns correct object type", {
  expect_is(fars_read_years(years=2013:2015), "list")
  expect_is(fars_read_years(years=2013:2015)[[1]], "tbl_df")
})

test_that("fars_summarize_years returns correct object type", {
  expect_is(fars_summarize_years(years=2013:2015), "tbl_df")
})