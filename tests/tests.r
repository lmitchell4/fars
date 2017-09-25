
library(testthat)

test_that("model fitting", {
  my_file <- make_filename(year=2013)
  expect_that(my_file, is_a("character"))
})





# expect_that(sqrt(3) * sqrt(3), equals(3))

# ## Use a strict test of equality (this test fails)
# expect_that(sqrt(3) * sqrt(3), is_identical_to(3))

# Error: sqrt(3) * sqrt(3) not identical to 3.
# Objects equal but not identical




# equals()	check for equality with numerical fuzz
# is_identical_to()	strict equality via identical()
# is_equivalent_to()	like equals() but ignores object attributes
# is_a()	checks the class of an object (using inherits())
# matches()	checks that a string matches a regular expression
# prints_text()	checks that an expression prints to the console
# shows_message()	checks for a message being generated
# gives_warning()	checks that an expression gives a warning
# throws_error()	checks that an expression (properly) throws an error
# is_true()	checks that an expression is TRUE


# test_that("model fitting", {
  # data(airquality)
  # fit <- lm(Ozone ~ Wind, data = airquality)
  # expect_that(fit, is_a("lm"))
  # expect_that(1 + 1, equals(2))
# })




# my_data <- fars_read(filename="accident_2013.csv.bz2")

# my_list <- fars_read_years(years=2013:2015)
# fars_summarize_years(years=2013:2015)
# fars_map_state(state.num=1, year=2013)


