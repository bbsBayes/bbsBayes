test_that("prepare_data()", {

  suppressMessages(strat_samp <- stratify(by = "bbs_usgs", sample_data = TRUE))

  expect_error(prepare_data(strat_samp,
                            species = "Wren",
                            min_max_route_years = 2),
               "Invalid species specified")

  expect_silent(p <- prepare_data(strat_samp,
                                  species = "Pacific Wren",
                                   min_max_route_years = 2)) %>%
    expect_type("list") %>%
    expect_named(c("model_data", "meta_data", "raw_data")) %>%
    expect_length(3)


  # By run (i.e. single value)
  expect_equal(p$model_data$n_sites, 377)
  expect_equal(p$model_data$n_strata, 19)
  expect_equal(p$model_data$n_counts, 4869)
  expect_equal(p$model_data$n_years, 51)
  expect_equal(p$model_data$n_observers, 608)
  expect_equal(p$model_data$max_n_obs_sites_strata, 103)

  # By Observation
  expect_snapshot_value(
    p$model_data[c("count", "strat", "year", "site", "observer", "first_year")],
    style = "json2")

  # By strata
  expect_equal(
    p$model_data$n_obs_sites_strata,
    c(15, 86, 5, 103, 54, 8, 4, 80, 44, 20, 84, 47, 41, 29, 98, 6, 26, 88, 51))

  expect_equal(
    p$model_data$non_zero_weight,
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

  # Matrix
  expect_snapshot_value(p$model_data[c("ste_mat", "obs_mat")], style = "json2")

  # Meta data
  expect_equal(p$meta_data$species, "Pacific Wren")
  expect_equal(p$meta_data$stratify_by, "bbs_usgs")

  # Data
  expect_snapshot_value(p$raw_data, style = "json2")

})


