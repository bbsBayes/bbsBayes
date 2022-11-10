test_that("dep_stop()", {

  f <- function() dep_stop("2.0.0")
  expect_error(f(), "`f\\(\\)` is defunct as of bbsBayes 2.0.0")

  f <- function() dep_stop("2.0.0", replace = "`g()`")
  expect_error(f(), "Use `g\\(\\)` instead.")

  f <- function(arg) if(!missing(arg)) dep_stop("2.0.0", "arg")
  expect_error(f("hi"), "The `arg` argument for `f\\(\\)` is defunct as of ")

  f <- function(arg) if(!missing(arg)) dep_stop("2.0.0", "arg", "`arg2`")
  expect_error(f("hi"), "Use `arg2` instead")

})

test_that("dep_warn()", {

  f <- function() dep_warn("2.0.0")
  expect_warning(f(), "`f\\(\\)` is deprecated as of bbsBayes 2.0.0")

  f <- function() dep_warn("2.0.0", replace = "`g()`")
  expect_warning(f(), "Use `g\\(\\)` instead.")

  f <- function(arg) if(!missing(arg)) dep_warn("2.0.0", "arg")
  expect_warning(f("hi"),
                 "The `arg` argument for `f\\(\\)` is deprecated as of ")

  f <- function(arg) if(!missing(arg)) dep_warn("2.0.0", "arg", "`arg2`")
  expect_warning(f("hi"), "Use `arg2` instead")

})
