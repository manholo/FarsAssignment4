
setwd(dirname(system.file("extdata", "accident_2013.csv.bz2", package = "FarsAssignment4")))


test_that("creating file names", {
  expect_match( make_filename(2013), "accident_2013.csv.bz2", fixed = TRUE)
})

test_that("reading by year", {
  expect_that(fars_read_years(4013), gives_warning())
})


test_that("summarize FARS", {

  expect_that( dim(fars_summarize_years(c(2013, 2014))), equals( c(12,3) ) )
})
