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


    withr::with_seed(111, {
      expect_silent(id <- create_init(bbs_models$model[i], bbs_models$variant[i],
                                      model_data = m)) %>%
        expect_type("list")
    })

    expect_snapshot_value(id, style = "json2", tolerance = 0.0001)
  }
})


test_that("prepare_model() first_diff / slope", {

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  s <- prepare_spatial(load_map("bbs_usgs"), prepped_data = p, quiet = TRUE)

  m <- dplyr::filter(bbs_models, model %in% c("first_diff", "slope"))

  for(i in seq_len(nrow(m))) {

    if(m$variant[i] == "spatial") sp <- s else sp <- NULL

    if(m$variant[i] == "nonhier") ex <- expect_warning else ex <- expect_silent
    ex(md <- prepare_model(p,
                           model = m$model[!!i],
                           model_variant = m$variant[!!i],
                           spatial_data = sp,
                           set_seed = 111))

    md %>%
      expect_type("list") %>%
      expect_named(c("model_data", "init_values", "meta_data", "meta_strata",
                     "raw_data"))

    expect_type(md[["model_data"]], "list")
    expect_type(md[["init_values"]], "list")
    expect_type(md[["meta_data"]], "list")
    expect_s3_class(md[["meta_strata"]], "data.frame")
    expect_s3_class(md[["raw_data"]], "data.frame")

    # Snapshots can't be run interactively
    expect_snapshot_value(md[["model_data"]], style = "json2",
                          tolerance = 0.001)
  }
})



test_that("prepare_model() gam / gamye", {

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  s <- prepare_spatial(load_map("bbs_usgs"), prepped_data = p, quiet = TRUE)

  m <- dplyr::filter(bbs_models, stringr::str_detect(model, "gam"))

  for(i in seq_len(nrow(m))) {
    for(n_knots in c(3, 8)) {
      for(basis in c("original", "mgcv")) {
        if(m$variant[i] == "spatial") sp <- s else sp <- NULL

        expect_silent(md <- prepare_model(p,
                                          model = m$model[!!i],
                                          model_variant = m$variant[!!i],
                                          spatial_data = sp,
                                          n_knots = n_knots,
                                          basis = basis,
                                          set_seed = 111))

        md %>%
          expect_type("list") %>%
          expect_named(c("model_data", "init_values", "meta_data",
                         "meta_strata", "raw_data"))

        expect_type(md[["model_data"]], "list")

        # Snapshots can't be run interactively
        expect_snapshot_value(md[["model_data"]], style = "json2",
                              tolerance = 0.001)
      }
    }
  }
})

test_that("prepare_model() heavy_tailed / use_pois", {

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  s <- prepare_spatial(load_map("bbs_usgs"), prepped_data = p, quiet = TRUE)

  m <- dplyr::filter(bbs_models, variant != "nonhier")

  for(i in seq_len(nrow(m))) {
    if(m$variant[i] == "spatial") sp <- s else sp <- NULL

    expect_message(md <- prepare_model(p,
                                     model = m$model[i],
                                     model_variant = m$variant[i],
                                     spatial_data = sp,
                                     heavy_tailed = FALSE,
                                     use_pois = FALSE,
                                     set_seed = 111),
                   "Negative Binomial models")

    for(heavy_tailed in c(TRUE, FALSE)) {
      expect_silent(md <- prepare_model(p,
                                        model = m$model[!!i],
                                        model_variant = m$variant[!!i],
                                        spatial_data = sp,
                                        heavy_tailed = heavy_tailed,
                                        use_pois = TRUE,
                                        set_seed = 111))

      md %>%
        expect_type("list") %>%
        expect_named(c("model_data", "init_values", "meta_data",
                       "meta_strata", "raw_data"))

      expect_type(md[["model_data"]], "list")

      # Snapshots can't be run interactively
      expect_snapshot_value(md[["model_data"]], style = "json2",
                            tolerance = 0.001)
    }
  }
})

