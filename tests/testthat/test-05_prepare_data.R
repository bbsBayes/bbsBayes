test_that("prepare_data() - sample", {

  strat_samp <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE)

  expect_silent(p <- prepare_data(strat_samp, min_max_route_years = 2)) %>%
    expect_type("list") %>%
    expect_named(c("model_data", "meta_data", "meta_strata", "raw_data"))


  # By run (i.e. single value)
  expect_equal(p$model_data$n_sites, 384)
  expect_equal(p$model_data$n_strata, 19)
  expect_equal(p$model_data$n_counts, 5003)
  expect_equal(p$model_data$n_years, 52)
  expect_equal(p$model_data$n_observers, 619)
  expect_equal(p$model_data$max_n_obs_sites_strata, 105)

  # By strata
  expect_equal(
    p$model_data$n_obs_sites_strata,
    c(15, 89, 5, 105, 54, 8, 4, 81, 46, 20, 85, 48, 41, 30, 100, 6, 27, 89, 51))

  expect_equal(
    p$model_data$non_zero_weight,
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

  # Meta data
  expect_equal(p$meta_data$species, "Pacific Wren")
  expect_equal(p$meta_data$stratify_by, "bbs_usgs")
  expect_equal(p$meta_data$stratify_type, "standard")



  # Snap shots (can't be run interactively)
  # By Observation
  expect_snapshot_value(
    p$model_data[c("count", "strat", "year", "site", "observer", "first_year")],
    style = "json2")

  # Matrix
  expect_snapshot_value(p$model_data[c("ste_mat", "obs_mat")], style = "json2")

  # Data
  expect_snapshot_value(p$raw_data, style = "json2")

})


test_that("prepare_data() - other", {

  strat_samp <- stratify(by = "bbs_usgs", species = "Gyrfalcon", quiet = TRUE)

  expect_silent(p <- prepare_data(strat_samp, min_max_route_years = 2)) %>%
    expect_type("list") %>%
    expect_named(c("model_data", "meta_data", "meta_strata", "raw_data"))


  # By run (i.e. single value)
  expect_equal(p$model_data$n_sites, 8)
  expect_equal(p$model_data$n_strata, 2)
  expect_equal(p$model_data$n_counts, 180)
  expect_equal(p$model_data$n_years, 40)
  expect_equal(p$model_data$n_observers, 25)
  expect_equal(p$model_data$max_n_obs_sites_strata, 22)

  # By strata
  expect_equal(p$model_data$n_obs_sites_strata, c(22, 15))

  expect_equal(p$model_data$non_zero_weight, c(0.3571, 0.0566),
               tolerance = 0.001)

  # Meta data
  expect_equal(p$meta_data$species, "Gyrfalcon")
  expect_equal(p$meta_data$stratify_by, "bbs_usgs")
  expect_equal(p$meta_data$stratify_type, "standard")


  # Snap shots (can't be run interactively)
  # By Observation
  expect_snapshot_value(
    p$model_data[c("count", "strat", "year", "site", "observer", "first_year")],
    style = "json2")

  # Matrix
  expect_snapshot_value(p$model_data[c("ste_mat", "obs_mat")], style = "json2")

  # Data
  expect_snapshot_value(p$raw_data, style = "json2")

})


