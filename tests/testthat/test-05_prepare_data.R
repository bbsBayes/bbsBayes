test_that("prepare_data()", {

  suppressMessages(strat_samp <- stratify(by = "bbs_usgs", sample_data = TRUE))

  expect_error(prepare_data(strat_samp,
                            species_to_run = "Wren",
                            model = "slope",
                            min_max_route_years = 2),
               "Invalid species specified")

  expect_message(p <- prepare_data(strat_samp,
                                   species_to_run = "Pacific Wren",
                                   model = "slope",
                                   min_max_route_years = 2),
                 "Preparing data")

  expect_type(p, "list")
  expect_length(p, 30)


  # By run (i.e. single value)
  expect_snapshot_value(p[c("model", "nsites", "nstrata", "ncounts", "nyears",
                            "nobservers", "maxnobs_sites_strata",
                            "stratify_by", "fixedyear")], style = "json2")

  # By Observation
  expect_snapshot_value(p[c("count", "strat", "year", "site",
                            "observer", "firstyr", "r_year")], style = "json2")

  # By strata
  expect_snapshot_value(p[c("nobs_sites_strata", "nonzeroweight")],
                        style = "json2")

  # Matrix
  expect_snapshot_value(p[c("ste_at", "obs_mat")], style = "json2")

  # Model
  expect_snapshot_value(p[c("calc_nu", "heavy_tailed", "use_pois",
                            "calc_log_lik", "calc_CV", "train", "test",
                            "ntrain", "ntest")], style = "json2")

  # Data
  expect_snapshot_value(p["alt_data"], style = "json2")

})

