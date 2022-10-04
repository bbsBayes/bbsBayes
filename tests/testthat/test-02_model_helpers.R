test_that("extract_index_data", {
  skip_on_ci()
  load(system.file("model_outputs", "pacific_wren_slope_BBS_fit.RData", package = "bbsBayes"))

  expect_silent(extract_index_data(model_fit = stan_fit,
                                   alt_n = "n",
                                   model_data = stan_data)) %>%
    expect_named(c("n", "area_weights", "r_year", "original_data"))

})

test_that("samples_to_array()", {
  skip_on_ci()
  load(system.file("model_outputs", "pacific_wren_slope_BBS_fit.RData", package = "bbsBayes"))

  n <- extract_index_data(stan_fit, model_data = stan_data)$n

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
