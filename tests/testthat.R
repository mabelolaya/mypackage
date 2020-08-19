library(testthat)
library(mypackage)


test_check("mypackage")
testthat::context("file naming and import")




test_that("FARS csv files are available", {
  expect_equal(list.files(system.file("extdata", package = "mypackage")),
               c("accident_2013.csv.bz2",
                 "accident_2014.csv.bz2",
                 "accident_2015.csv.bz2"))
}
)

test_that("make_filename function", {

  expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))

})


test_that("fars_read function", {

  expect_that(fars_read("accident_2013.csv.bz2"), is_a("data.frame"))

  expect_that(fars_read("accident_2012.csv.bz2"), throws_error("file 'accident_2012.csv.bz2' does not exist"))

})
