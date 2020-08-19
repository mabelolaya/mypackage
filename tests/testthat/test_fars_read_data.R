
testthat::context("file naming and import")



test_that("mypackage csv files are available", {
  expect_equal(list.files(system.file("textdata", package = "mypackage")),
               c("accident_2013.csv.bz2",
                 "accident_2014.csv.bz2",
                 "accident_2015.csv.bz2"))
}
)
