
test_that("samples_to_array()", {
  skip_on_ci()

  m <- readr::read_rds(system.file("extdata",
                                   "pacific_wren_slope_fit.rds",
                                   package = "bbsBayes"))

  n <- m$model_fit$draws(variables = "n", format = "draws_matrix")

  expect_silent(n2 <- samples_to_array(n, n_strata = 19, n_years = 51))

  n1 <- n
  n1 <- unclass(n1)
  dimnames(n1) <- NULL

  expect_equal(n1[, 1:19], n2[, , 1])
  expect_equal(n1[, 20:38], n2[, , 2])
  expect_equal(n1[, 20:38], n2[, , 2])
  expect_equal(n1[, 837:855], n2[, , 45])
  expect_equal(n1[, 932:950], n2[, , 50])
})
