test_that("model_params()", {

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  p <- p$model_data

  for(i in seq_len(nrow(bbs_models))) {
    model <- bbs_models$model[i]
    if(model == "gam") n_knots <- 9 else n_knots <- NULL
    if(model %in% c("gam", "gamye")) {
      basis <- c("original", "mgcv")
    } else basis <- "mgcv"

    for(b in basis) {
      expect_silent(
        m <- model_params(
          model = model,
          n_strata = p$n_strata, year = p$year, n_counts = p$n_counts,
          heavy_tailed = TRUE,
          n_knots = n_knots,
          basis = b,
          use_pois = FALSE,
          calculate_nu = FALSE,
          calculate_log_lik = FALSE,
          calculate_CV = FALSE))

      n <- c("calc_nu", "heavy_tailed", "use_pois",
             "calc_log_lik", "calc_CV", "train", "test",
             "n_train", "n_test")

      if(model %in% c("gam", "gamye")) n <- c(n, "n_knots_year", "year_basis")
      if(model == "first_diff") n <- c(n, "fixed_year", "zero_betas",
                                       "Iy1", "n_Iy1", "Iy2", "n_Iy2")
      if(model == "slope") n <- c(n, "fixed_year")
      expect_named(m, n)
    }
  }

})

test_that("create_init", {

  withr::local_seed(111)

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  p <- p$model_data


  for(i in seq_len(nrow(bbs_models))) {
    model <- bbs_models$model[i]
    if(model == "gam") n_knots <- 9 else n_knots <- NULL
    m <- append(p,
                model_params(model = model,
                             n_strata = p$n_strata, year = p$year,
                             n_counts = p$n_counts,
                             heavy_tailed = TRUE,
                             n_knots = n_knots,
                             basis = "mgcv",
                             use_pois = FALSE,
                             calculate_nu = FALSE,
                             calculate_log_lik = FALSE,
                             calculate_CV = FALSE))


    expect_silent(id <- create_init(bbs_models$model[i], bbs_models$variant[i],
                                    model_data = m, chains = 3)) %>%
      expect_type("list")

    expect_snapshot_value(id, style = "json2", tolerance = 0.0001)
  }
})


test_that("run_model() first_diff short", {

  skip_on_ci() # Seeds not respected on GitHub Actions for unknown reasons

  withr::local_seed(111)
  unlink(list.files(test_path(), "BBS_STAN_first_diff_hier_",
                    full.names = TRUE))

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  expect_message(r <- run_model(p,
                                model = "first_diff",
                                output_dir = test_path(),
                                chains = 2,
                                iter_sampling = 10, iter_warmup = 10,
                                refresh = 0,
                                seed = 111)) %>%
    # Catch all messages and notes
    suppressMessages() %>%
    suppressWarnings() %>%
    utils::capture.output()

    expect_type(r, "list")
    expect_named(r, c("model_fit", "meta_data", "meta_strata", "raw_data"))
    expect_s3_class(r$model_fit, "CmdStanMCMC")

    f <- paste0("BBS_STAN_first_diff_hier_", Sys.Date(),
                c("-1.csv", "-2.csv", "_01.rds")) %>%
      test_path()

    expect_true(all(file.exists(f)))

    # Snapshots can't be run interactively
    f <- strip_model_files(f)
    expect_snapshot_file(f[1])
    expect_snapshot_file(f[2])

    # Clean up
    list.files(test_path(),
               paste0("^BBS_STAN_first_diff_hier_(.)*"), full.names = TRUE) %>%
      unlink()
})

test_that("run_model() slope", {

  # Use example model (slope takes a while to run)
  r <- slope_test_model

  expect_type(r, "list")
  expect_named(r, c("model_fit", "meta_data", "meta_strata", "raw_data"))
  expect_s3_class(r$model_fit, "CmdStanMCMC")
})


test_that("run_model() first_diff spatial", {

  skip_on_ci() # Seeds not respected on GitHub Actions for unknown reasons

  withr::local_seed(111)
  unlink(list.files(test_path(), "BBS_STAN_first_diff_spatial_",
                    full.names = TRUE))

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  s <- prepare_spatial(load_map("bbs_usgs"), prepped_data = p)

  expect_message(r <- run_model(p,
                                model = "first_diff",
                                model_variant = "spatial",
                                spatial_data = s,
                                output_dir = test_path(),
                                chains = 2,
                                iter_sampling = 10, iter_warmup = 10,
                                refresh = 0,
                                seed = 111)) %>%
    # Catch all messages and notes
    suppressMessages() %>%
    suppressWarnings() %>%
    utils::capture.output()

  expect_type(r, "list")
  expect_named(r, c("model_fit", "meta_data", "meta_strata", "raw_data"))
  expect_s3_class(r$model_fit, "CmdStanMCMC")

  f <- paste0("BBS_STAN_first_diff_spatial_", Sys.Date(),
              c("-1.csv", "-2.csv", "_01.rds")) %>%
    test_path()

  expect_true(all(file.exists(f)))

  # Snapshots can't be run interactively
  f <- strip_model_files(f)
  expect_snapshot_file(f[1])
  expect_snapshot_file(f[2])

  # Clean up
  list.files(test_path(),
             paste0("^BBS_STAN_first_diff_spatial_(.)*"), full.names = TRUE) %>%
    unlink()
})


test_that("run_model() Full", {

 skip("long")

  withr::local_seed(111)

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  sp <- prepare_spatial(load_map("bbs_cws"), p, quiet = TRUE)

  for(i in seq_len(nrow(bbs_models))) {

    expect_message(r <- run_model(p,
                                  model = bbs_models$model[i],
                                  model_variant = bbs_models$variant[i],
                                  spatial_data = sp,
                                  output_dir = ".", chains = 2,
                                  iter_sampling = 10, iter_warmup = 10,
                                  refresh = 0,
                                  seed = 111)) %>%
      # Catch all messages and notes
      suppressMessages() %>%
      suppressWarnings() %>%
      utils::capture.output()

    expect_type(r, "list")
    expect_named(r, c("model_fit", "non_zero_weight", "meta_data",
                      "meta_strata", "raw_data"))
    expect_s3_class(r$model_fit, "CmdStanMCMC")

    f <- paste0("BBS_STAN_", bbs_models$model[i], "_", bbs_models$variant[i],
                "_", Sys.Date(), c("-1.csv", "-2.csv", ".rds"))

    expect_true(all(file.exists(f)))
  }

  # Clean up
  unlink(list.files(test_path(), paste0("^BBS_STAN_(.)*_", Sys.Date()),
                    full.names = TRUE))
})
