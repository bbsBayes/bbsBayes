
test_that("run_model() first_diff short", {

  unlink(list.files(test_path(), "BBS_STAN_first_diff_hier_",
                    full.names = TRUE))

  md <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2) %>%
    prepare_model(model = "first_diff", set_seed = 111)

  expect_message(r <- run_model(md,
                                output_dir = test_path(),
                                chains = 2,
                                iter_sampling = 10, iter_warmup = 10,
                                refresh = 0,
                                set_seed = 111)) %>%
    # Catch all messages and notes
    suppressMessages() %>%
    suppressWarnings() %>%
    utils::capture.output()

    expect_type(r, "list")
    expect_named(r, c("model_fit", "meta_data", "meta_strata", "raw_data"))

    expect_s3_class(r$model_fit, "CmdStanMCMC")
    expect_type(r[["meta_data"]], "list")
    expect_s3_class(r[["meta_strata"]], "data.frame")
    expect_s3_class(r[["raw_data"]], "data.frame")

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
  expect_type(r[["meta_data"]], "list")
  expect_s3_class(r[["meta_strata"]], "data.frame")
  expect_s3_class(r[["raw_data"]], "data.frame")
})


test_that("run_model() first_diff spatial", {

  unlink(list.files(test_path(), "BBS_STAN_first_diff_spatial_",
                    full.names = TRUE))

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  s <- prepare_spatial(load_map("bbs_usgs"), prepped_data = p, quiet = TRUE)

  md <- prepare_model(p, model = "first_diff",
                      model_variant = "spatial",
                      spatial_data = s,
                      set_seed = 111)

  expect_message(r <- run_model(md,
                                output_dir = test_path(),
                                chains = 2,
                                iter_sampling = 10, iter_warmup = 10,
                                refresh = 0,
                                set_seed = 111)) %>%
    # Catch all messages and notes
    suppressMessages() %>%
    suppressWarnings() %>%
    utils::capture.output()

  expect_type(r, "list")
  expect_named(r, c("model_fit", "meta_data", "meta_strata", "raw_data"))

  expect_s3_class(r$model_fit, "CmdStanMCMC")
  expect_type(r[["meta_data"]], "list")
  expect_s3_class(r[["meta_strata"]], "data.frame")
  expect_s3_class(r[["raw_data"]], "data.frame")

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

test_that("run_model() ... args", {

  unlink(list.files(test_path(), "BBS_STAN_first_diff_hier_",
                    full.names = TRUE))

  md <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2) %>%
    prepare_model(model = "first_diff", set_seed = 111)

  expect_message(r <- run_model(md,
                                output_dir = test_path(),
                                chains = 2,
                                iter_sampling = 5, iter_warmup = 5,
                                refresh = 0,
                                set_seed = 111,
                                save_latent_dynamics = TRUE)) %>%
    # Catch all messages and notes
    suppressMessages() %>%
    suppressWarnings() %>%
    utils::capture.output()

  paste0("first_diff_hier_bbs_CV-diagnostic-", 1:2, ".csv") %>%
    test_path() %>%
    file.exists() %>%
    all() %>%
    expect_true()

  # Clean up
  list.files(test_path(),
             paste0("first_diff_hier_(.)*"), full.names = TRUE) %>%
    unlink()
})



test_that("run_model() Full", {

  skip("long")

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  s <- prepare_spatial(load_map("bbs_cws"), p, quiet = TRUE)

  md <- prepare_model(p, model = bbs_models$model[i],
                      model_variant = bbs_models$variant[i],
                      spatial_data = sp,
                      set_seed = 111)

  for(i in seq_len(nrow(bbs_models))) {

    expect_message(r <- run_model(md,
                                  output_dir = ".", chains = 2,
                                  iter_sampling = 10, iter_warmup = 10,
                                  refresh = 0,
                                  set_seed = 111)) %>%
      # Catch all messages and notes
      suppressMessages() %>%
      suppressWarnings() %>%
      utils::capture.output()

    expect_type(r, "list")
    expect_named(r, c("model_fit", "non_zero_weight", "meta_data",
                      "meta_strata", "raw_data"))

    expect_s3_class(r$model_fit, "CmdStanMCMC")
    expect_type(r[["meta_data"]], "list")
    expect_s3_class(r[["meta_strata"]], "data.frame")
    expect_s3_class(r[["raw_data"]], "data.frame")

    f <- paste0("BBS_STAN_", bbs_models$model[i], "_", bbs_models$variant[i],
                "_", Sys.Date(), c("-1.csv", "-2.csv", ".rds"))

    expect_true(all(file.exists(f)))
  }

  # Clean up
  unlink(list.files(test_path(), paste0("^BBS_STAN_(.)*_", Sys.Date()),
                    full.names = TRUE))
})
