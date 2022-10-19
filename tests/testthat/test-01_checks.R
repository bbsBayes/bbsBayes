test_that("check_logical()", {

  expect_silent(check_logical(TRUE, FALSE, FALSE))
  expect_silent(check_logical(TRUE))
  expect_error(check_logical(1), "`1` must be logical")
  expect_error(check_logical(true = TRUE, number = 4),
               "`number` must be logical")
  expect_error(check_logical(true = TRUE, number = 4, character = "hi"),
               "`number`, `character` must be logical")

  test_fun <- function(x) check_logical(x)

  expect_silent(test_fun(x = TRUE))
  expect_error(test_fun(x = "hi"), "`x` must be logical")
  expect_error(test_fun(x = mtcars), "`x` must be logical")
  expect_error(test_fun(x = 4), "`x` must be logical")
  expect_error(test_fun(x = 1), "`x` must be logical")

})

test_that("check_numeric()", {

  expect_silent(check_numeric(1, 5, 9.1))
  expect_silent(check_numeric(1))
  expect_error(check_numeric(TRUE), "`TRUE` must be a number")
  expect_error(check_numeric(true = TRUE, number = 4),
               "`true` must be a number")
  expect_error(check_numeric(true = TRUE, number = 4, character = "hi"),
               "`true`, `character` must be a number")

  test_fun <- function(x) check_numeric(x)

  expect_silent(test_fun(x = 10))
  expect_error(test_fun(x = "hi"), "`x` must be a number")
  expect_error(test_fun(x = mtcars), "`x` must be a number")
  expect_error(test_fun(x = TRUE), "`x` must be a number")

})
