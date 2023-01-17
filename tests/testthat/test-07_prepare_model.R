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
          calculate_log_lik = FALSE))

      n <- c("calc_nu", "heavy_tailed", "use_pois",
             "calc_log_lik")

      if(model %in% c("gam", "gamye")) n <- c(n, "n_knots_year", "year_basis")
      if(model == "first_diff") n <- c(n, "fixed_year", "zero_betas",
                                       "Iy1", "n_Iy1", "Iy2", "n_Iy2")
      if(model == "slope") n <- c(n, "fixed_year")
      expect_named(m, n)
    }
  }

})

test_that("create_init()", {

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
                             calculate_log_lik = FALSE))


    withr::with_seed(111, {
      expect_silent(id <- create_init(bbs_models$model[i], bbs_models$variant[i],
                                      model_data = m)) %>%
        expect_type("list")
    })

    expect_snapshot_value_safe(id, style = "json2", tolerance = 0.0001)
  }
})

test_that("cv_folds()", {
  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2)

  expect_message(cv_folds(p[["raw_data"]]), "experimental")
  expect_silent(f1 <- cv_folds(p[["raw_data"]], quiet = TRUE))
  expect_type(f1, "double")
  expect_length(f1, nrow(p[["raw_data"]]))
  expect_equal(max(f1, na.rm = TRUE), 10)
  expect_true(any(is.na(f1)))

  expect_silent(f2 <- cv_folds(p[["raw_data"]], k = 3,
                               omit_singles = FALSE, quiet = TRUE))
  expect_equal(max(f2, na.rm = TRUE), 3)
  expect_false(any(is.na(f2)))

  expect_silent(f3 <- cv_folds(p[["raw_data"]],
                               fold_groups = "route", quiet = TRUE))
  expect_type(f3, "double")
  expect_length(f3, nrow(p[["raw_data"]]))
  expect_equal(max(f3, na.rm = TRUE), 10)
  expect_true(any(is.na(f3)))

  expect_snapshot_value_safe(f1, style = "json2")
  expect_snapshot_value_safe(f2, style = "json2")
  expect_snapshot_value_safe(f3, style = "json2")
})


test_that("prepare_model() first_diff / slope", {

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2) %>%
    prepare_spatial(load_map("bbs_usgs"), quiet = TRUE)

  m <- dplyr::filter(bbs_models, model %in% c("first_diff", "slope"))

  for(i in seq_len(nrow(m))) {

    if(m$variant[i] == "nonhier") ex <- expect_warning else ex <- expect_silent
    ex(md <- prepare_model(p,
                           model = m$model[!!i],
                           model_variant = m$variant[!!i],
                           set_seed = 111))

    md %>%
      expect_type("list") %>%
      expect_named(c("model_data", "init_values", "folds",
                     "meta_data", "meta_strata", "raw_data"))

    expect_type(md[["model_data"]], "list")
    expect_type(md[["init_values"]], "list")
    expect_type(md[["folds"]], "NULL")
    expect_type(md[["meta_data"]], "list")
    expect_s3_class(md[["meta_strata"]], "data.frame")
    expect_s3_class(md[["raw_data"]], "data.frame")

    if(m$variant[i] == "spatial") {
      expect_true(all(c("n_edges", "node1", "node2") %in%
                        names(md[["model_data"]])))
    } else {
      expect_false(any(c("n_edges", "node1", "node2") %in%
                         names(md[["model_data"]])))
    }

    # Snapshots can't be run interactively
    expect_snapshot_value_safe(md[["model_data"]], style = "json2",
                               tolerance = 0.01)
  }
})



test_that("prepare_model() gam / gamye", {

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2) %>%
    prepare_spatial(load_map("bbs_usgs"), quiet = TRUE)

  m <- dplyr::filter(bbs_models, stringr::str_detect(model, "gam"))

  for(i in seq_len(nrow(m))) {
    for(n_knots in c(3, 8)) {
      for(basis in c("original", "mgcv")) {

        expect_silent(md <- prepare_model(p,
                                          model = m$model[!!i],
                                          model_variant = m$variant[!!i],
                                          n_knots = n_knots,
                                          basis = basis,
                                          set_seed = 111))

        md %>%
          expect_type("list") %>%
          expect_named(c("model_data", "init_values", "folds", "meta_data",
                         "meta_strata", "raw_data"))

        expect_type(md[["model_data"]], "list")

        if(m$variant[i] == "spatial") {
          expect_true(all(c("n_edges", "node1", "node2") %in%
                            names(md[["model_data"]])))
        } else {
          expect_false(any(c("n_edges", "node1", "node2") %in%
                             names(md[["model_data"]])))
        }

        # Snapshots can't be run interactively

        # For reasons (?) year_basis sometimes switches signs when testing on CI
        x <- md[["model_data"]]
        x <- x[names(x) != "year_basis"]
        expect_snapshot_value_safe(x, style = "json2", tolerance = 0.01)
      }
    }
  }
})

test_that("prepare_model() heavy_tailed / use_pois", {

  p <- stratify(by = "bbs_usgs", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data(min_max_route_years = 2) %>%
    prepare_spatial(load_map("bbs_usgs"), quiet = TRUE)

  m <- dplyr::filter(bbs_models, variant != "nonhier")

  for(i in seq_len(nrow(m))) {
    expect_message(md <- prepare_model(p,
                                     model = m$model[i],
                                     model_variant = m$variant[i],
                                     heavy_tailed = FALSE,
                                     use_pois = FALSE,
                                     set_seed = 111),
                   "Negative Binomial models")

    for(heavy_tailed in c(TRUE, FALSE)) {
      expect_silent(md <- prepare_model(p,
                                        model = m$model[!!i],
                                        model_variant = m$variant[!!i],
                                        heavy_tailed = heavy_tailed,
                                        use_pois = TRUE,
                                        set_seed = 111))

      md %>%
        expect_type("list") %>%
        expect_named(c("model_data", "init_values", "folds", "meta_data",
                       "meta_strata", "raw_data"))

      expect_type(md[["model_data"]], "list")

      if(m$variant[i] == "spatial") {
        expect_true(all(c("n_edges", "node1", "node2") %in%
                          names(md[["model_data"]])))
      } else {
        expect_false(any(c("n_edges", "node1", "node2") %in%
                           names(md[["model_data"]])))
      }

      # Snapshots can't be run interactively

      # For reasons (?) year_basis sometimes switches signs when testing on CI
      x <- md[["model_data"]]
      x <- x[names(x) != "year_basis"]
      expect_snapshot_value_safe(x, style = "json2", tolerance = 0.01)
    }
  }
})

