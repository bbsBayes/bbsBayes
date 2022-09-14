test_that("extract_index_data_stan", {
  load(system.file("model_outputs", "pacific_wren_slope_BBS_Stan_fit.RData", package = "bbsBayes"))

  expect_silent(extract_index_data_stan(model_fit = stan_fit,
                                        alt_n = "n",
                                        model_data = stan_data)) %>%
    expect_named(c("n", "area_weights", "r_year", "original_data"))

})
