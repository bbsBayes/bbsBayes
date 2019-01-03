context("fetch_bbs_data")

test_that("Invalid values for the level argument raise errors", {
  error_regex <- "Invalid level argument"
  expect_error(get_counts(level = "bad input"),
               regex = error_regex)
  expect_error(get_counts(level = NA),
               regex = error_regex)
  expect_error(get_counts(level = 1),
               regex = error_regex)
})

test_that("Fetching state level counts returns expected data", {
  skip_on_cran()
  skip_on_travis()
  counts <- get_counts(level = 'state', quiet = FALSE)
  expect_is(counts, 'data.frame')
  expect_identical(names(counts),
                   c('RouteDataID', 'countrynum', 'statenum', 'Route', 'RPID',
                     'Year', 'AOU', 'Count10', 'Count20', 'Count30', 'Count40',
                     'Count50', 'StopTotal', 'SpeciesTotal'))
})

test_that("Fetching stop level counts returns expected data", {
  skip_on_cran()
  skip_on_travis()
  counts <- get_counts(level = "stop", quiet = FALSE)
  expect_is(counts, 'data.frame')
  expect_identical(names(counts),
                   c('RouteDataID', 'countrynum', 'statenum', 'Route', 'RPID',
                     'Year', 'AOU', paste0('Stop', 1:50)))
})
