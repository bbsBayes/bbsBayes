test_that("create_init_def", {

  withr::local_seed(111)

  for(i in seq_len(nrow(bbs_models))) {

    p <- prepare_data(stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE),
                      species_to_run = "Pacific Wren",
                      model = bbs_models$model[i],
                      model_variant = bbs_models$variant[i],
                      min_max_route_years = 2) %>%
      suppressWarnings()

    create_init_def(c(bbs_models$model[i], bbs_models$variant[i]),
                    model_data = p, n_chains = 3) %>%
      expect_silent() %>%
      expect_type("list") %>%
      expect_snapshot_value()
  }
})

test_that("run_model", {

  withr::local_seed(111)


  p <- prepare_data(stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE),
                    species_to_run = "Pacific Wren",
                    min_max_route_years = 2)

  map <- load_map(stratify_by = "bbs_cws") %>%
    dplyr::filter(.data$ST_12 %in% p$alt_data$strat_name)

  n <- spatial_neighbours(map, strata_col = "ST_12")

  for(i in seq_len(nrow(bbs_models))) {

    expect_message(r <- run_model(model_data = p,
                                  model = bbs_models$model[i],
                                  model_variant = bbs_models$variant[i],
                                  spatial_neighbours = n,
                                  out_dir = ".", n_chains = 2,
                                  iter_sampling = 10, iter_warmup = 10))

    expect_type(r, "list")
    expect_named(r, c("model_fit", "meta_data"))
    expect_s3_class(r$model_fit, "CmdStanMCMC")

    f <- paste0("BBS_STAN_", bbs_models$model[i], "_", bbs_models$variant[i],
                "_", Sys.Date(), c("-1", "-2"), ".csv")

    expect_true(all(file.exists(f)))
    expect_snapshot_file(f[1])
    expect_snapshot_file(f[2])
  }

})
