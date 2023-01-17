expect_silent({
  r <- pacific_wren_model
  #20 iterations x 2 chains = 40
  n_iter <- r$model_fit$metadata()$iter_sampling * r$model_fit$num_chains()

  n_yrs <- 54
  n_strata <- dplyr::n_distinct(r$raw_data$strata_name)
})

test_that("samples_to_array()", {

  n <- r$model_fit$draws(variables = "n", format = "draws_matrix")

  yrs <- sort(unique(r$raw_data$year))
  expect_silent(n2 <- samples_to_array(
    model_output = r,
    alternate_n = "n",
    years_to_keep = yrs))

  expect_equal(dimnames(n2),
               list("iter" = as.character(seq_len(n_iter)),
                    "strata_name" = unique(r$raw_data$strata_name),
                    "year" = as.character(yrs)))

  n1 <- unclass(n)

  expect_equal(n1[, 1:19], n2[, , 1], ignore_attr = TRUE)
  expect_equal(n1[, 20:38], n2[, , 2], ignore_attr = TRUE)
  expect_equal(n1[, 39:57], n2[, , 3], ignore_attr = TRUE)
  expect_equal(n1[, 96:114], n2[, , 6], ignore_attr = TRUE)

  # Check year range
  yrs <- 1995:2018

  expect_silent(n3 <- samples_to_array(
    model_output = r,
    alternate_n = "n",
    years_to_keep = yrs))

  expect_equal(dimnames(n3),
               list("iter" = as.character(seq_len(n_iter)),
                    "strata_name" = unique(r$raw_data$strata_name),
                    "year" = as.character(yrs)))

  expect_equal(n2[ , , as.character(yrs)], n3)
})


test_that("generate_indices()", {

  # Basic run
  expect_message(i <- generate_indices(r, regions = "stratum"),
                 "Processing region stratum")
  expect_named(i, c("indices", "samples", "meta_data",
                    "meta_strata", "raw_data"))

  ix <- i[["indices"]]
  s <- i[["samples"]]

  expect_s3_class(ix, "data.frame")
  expect_equal(nrow(ix), n_yrs * n_strata)
  expect_true(all(i[["raw_data"]]$year %in% ix$year)) # Can be missing from raw
  expect_true(all(ix$region %in% i[["raw_data"]]$strata_name))
  expect_true(all(ix$region == ix$strata_included))
  expect_true(all(ix$strata_excluded == ""))

  expect_named(s, paste0("stratum_", unique(i[["raw_data"]]$strata_name)))

  # Samples for all iterations x all years
  expect_true(all(vapply(s, FUN = dim, FUN.VALUE = c(1, 1)) == c(n_iter, n_yrs)))

  # Missing years have NA obs_mean (only missing 2020, no others)
  expect_true(all(is.na(ix$obs_mean[ix$year == 2020])))
  expect_true(all(!is.na(ix$obs_mean[ix$year != 2020])))

  # Expect quantiles based on samples: Check a bunch of combinations
  year <- c(1, 20, 50)
  strat <- c("CA-AB-10", "US-AK-5", "US-WA-9")
  quant <- c(0.025, 0.75, 0.975)
  for(y in year) {
    for(st in strat) {
      for(q in quant) {
      expect_equal(stats::quantile(s[[paste0("stratum_", st)]][, y], q),
                   ix[ix$region == st, ][[paste0("index_q_", q)]][y],
                   ignore_attr = TRUE)
      }
    }
  }

  # Snapshots can't be run interactively
  snp <- dplyr::select(ix, "year", "region", "obs_mean", "n_routes",
                       "n_routes_total", "n_non_zero", "backcast_flag")
  expect_snapshot_value_safe(snp, style = "json2")
})

test_that("generate_indices(start_year)", {

  # Diff start year
  expect_silent(i1 <- generate_indices(r, quiet = TRUE))
  expect_silent(i2 <- generate_indices(r, start_year = 1995, quiet = TRUE))
  ix <- i2[["indices"]]
  s <- i2[["samples"]]

  expect_false(all(i1[["raw_data"]]$year %in% ix$year))
  expect_equal(min(ix$year), 1995)

  # Samples for all samples x all years (fewer now)
  expect_true(all(vapply(s, FUN = dim, FUN.VALUE = c(1, 1)) == c(n_iter, 27)))
  expect_true(all(ix$year %in% i1[["indices"]]$year))

  # Expect indices same for years which overlap (except n_routes_total)
  expect_equal(dplyr::filter(i1[["indices"]], year >= 1995) %>%
                 dplyr::select(-"n_routes_total"),
               dplyr::select(ix, -"n_routes_total"))

  # Snapshots can't be run interactively
  snp <- dplyr::select(ix, "year", "region", "obs_mean", "n_routes",
                       "n_routes_total", "n_non_zero", "backcast_flag")
  expect_snapshot_value_safe(snp, style = "json2")
})

test_that("generate_indices(quantiles)", {

  # Diff quantiles
  expect_silent(i <- generate_indices(r, quantiles = c(0.3, 0.6), quiet = TRUE))
  ix <- i[["indices"]]
  s <- i[["samples"]]

  expect_true(all(c("index_q_0.3", "index_q_0.6") %in% names(ix)))

  # No change in other values
  expect_silent(i1 <- generate_indices(r, quiet = TRUE))
  expect_silent(i2 <- generate_indices(r, quantiles = c(0.3, 0.6),
                                       quiet = TRUE))
  expect_equal(i1$samples, i2$samples)
  expect_equal(dplyr::select(i1$indices, -dplyr::contains("index_q")),
               dplyr::select(i2$indices, -dplyr::contains("index_q")))
})

test_that("generate_indices(regions)", {

  r <- pacific_wren_model

  # Diff quantiles
  expect_silent(i <- generate_indices(r,
                                      regions = c("continent", "country"),
                                      quiet = TRUE))
  ix <- i[["indices"]]
  s <- i[["samples"]]

  expect_true(all(ix$region %in%
                    c("continent", "Canada", "United States of America")))
  expect_named(s, c("continent_continent", "country_Canada",
                    "country_United States of America"))
  expect_true(all(ix$strata_included != ""))
  expect_true(all(ix$strata_excluded == ""))

  # Samples for all strata x all years
  expect_true(all(vapply(s, FUN = dim, FUN.VALUE = c(1, 1)) == c(n_iter, n_yrs)))

  # Expect quantiles based on samples: Check a bunch of combinations
  year <- c(1, 20, 50)
  strat <- c("United States of America", "Canada", "continent")
  type <- c("country_", "country_", "continent_")
  quant <- c(0.025, 0.75, 0.975)
  for(y in year) {
    for(st in strat) {
      for(q in quant) {
        t <- type[which(st == strat)]
        expect_equal(stats::quantile(s[[paste0(t, st)]][, y], q),
                     ix[ix$region == st, ][[paste0("index_q_", q)]][y],
                     ignore_attr = TRUE)
      }
    }
  }
})


test_that("generate_indices(regions_index)", {

  ri <- bbs_strata[[r$meta_data$stratify_by]] %>%
    dplyr::mutate(east_west = dplyr::if_else(
      prov_state %in% c("MB", "BC", "SK", "AB", "YT", "NT"), "west", "east"))


  expect_silent(i <- generate_indices(r,
                                      regions = c("stratum", "east_west"),
                                      regions_index = ri, quiet = TRUE))
  ix <- i[["indices"]]
  s <- i[["samples"]]

  expect_equal(nrow(ix), n_strata * n_yrs + 2 * n_yrs)
  expect_true(all(i[["raw_data"]]$year %in% ix$year)) # Can have missing in raw_data
  expect_true(
    all(ix$region %in% c(i[["raw_data"]]$strata_name, "east", "west")))
  expect_true(all(ix$strata_excluded == ""))

  ix_cust <- dplyr::filter(ix, region %in% c("east", "west"))
  inc <- dplyr::filter(ri, strata_name %in% ix$region)
  expect_true(all(ix_cust$strata_included != ""))
  expect_equal(
    sort(unique(ix_cust$strata_included)),
    c(paste0(sort(inc$strata_name[inc$east_west == "west"]), collapse = " ; "),
      paste0(sort(inc$strata_name[inc$east_west == "east"]), collapse = " ; ")))

  expect_named(s, c(paste0("stratum_", unique(i[["raw_data"]]$strata_name)),
                    "east_west_east", "east_west_west"))
})


  # Alternate n
test_that("generate_indices(alternate_n)", {

  r <- slope_test_model

  # Diff annual index parameter
  expect_silent(i1 <- generate_indices(r, regions = "stratum",
                                       alternate_n = "n", quiet = TRUE))
  expect_silent(i <- generate_indices(r, regions = "stratum",
                                      alternate_n = "n_slope", quiet = TRUE))

  # Differences based on alternate_n chosen
  expect_false(all(i1$indices == i$indices))
  expect_false(all(i1$samples[[1]] == i$samples[[1]]))

  # Otherwise similar way of being created
  ix <- i[["indices"]]
  s <- i[["samples"]]

  expect_s3_class(ix, "data.frame")
  expect_equal(nrow(ix), n_strata * n_yrs)
  expect_true(all(i[["raw_data"]]$year %in% ix$year)) # Can have missing in raw_data
  expect_true(all(ix$region %in% i[["raw_data"]]$strata_name))
  expect_true(all(ix$region == ix$strata_included))
  expect_true(all(ix$strata_excluded == ""))

  expect_named(s, paste0("stratum_", unique(i[["raw_data"]]$strata_name)))

  # Samples for all iterations x all years  (20 iterations x 2 chains = 40)
  expect_true(all(vapply(s, FUN = dim, FUN.VALUE = c(1, 1)) == c(40, n_yrs)))

  # Expect quantiles based on samples: Check a bunch of combinations
  year <- c(1, 20, 50)
  strat <- c("CA-AB-10", "US-AK-5", "US-WA-9")
  quant <- c(0.025, 0.75, 0.975)
  for(y in year) {
    for(st in strat) {
      for(q in quant) {
        expect_equal(stats::quantile(s[[paste0("stratum_", st)]][, y], q),
                     ix[ix$region == st, ][[paste0("index_q_", q)]][y],
                     ignore_attr = TRUE)
      }
    }
  }

  # Snapshots can't be run interactively
  snp <- dplyr::select(ix, "year", "region", "obs_mean", "n_routes",
                       "n_routes_total", "n_non_zero", "backcast_flag")
  expect_snapshot_value_safe(snp, style = "json2")
})


test_that("generate_indices(max_backcast)", {

  # Diff backcast
  expect_silent(i1 <- generate_indices(r, regions = "prov_state", quiet = TRUE))
  expect_silent(i2 <- generate_indices(r, regions = "prov_state",
                                       max_backcast = 1, quiet = TRUE))

  # Expect some cols to differ
  chng <- c("backcast_flag", "strata_included", "strata_excluded")
  for(c in chng) expect_false(all(i1[["indices"]][[c]] == i2[["indices"]][[c]]))
  expect_true(all(i1[["indices"]][["strata_excluded"]] == ""))
  expect_false(all(i2[["indices"]][["strata_excluded"]] == ""))
  expect_true(all(i1[["indices"]][["backcast_flag"]] == 1))
  expect_false(all(i2[["indices"]][["backcast_flag"]] == 1))

  # No change in other values
  expect_equal(i1$samples, i2$samples)
  expect_equal(dplyr::select(i1[["indices"]], -dplyr::any_of(chng)),
               dplyr::select(i2[["indices"]], -dplyr::any_of(chng)))


  # Snapshots can't be run interactively
  snp <- dplyr::select(i2[["indices"]],
                       "year", "region", "n_non_zero", "backcast_flag",
                       "strata_included", "strata_excluded")
  expect_snapshot_value_safe(snp, style = "json2")
})


test_that("generate_indices(max_backcast, drop_excluded)", {

  # Diff backcast
  expect_silent(i1 <- generate_indices(r, regions = "prov_state",
                                       max_backcast = 1, quiet = TRUE))
  expect_silent(i2 <- generate_indices(r, regions = "prov_state",
                                       max_backcast = 1, drop_exclude = TRUE,
                                       quiet = TRUE))

  # Expect differences where strata excluded
  i2_diff <- dplyr::filter(i2[["indices"]], .data$strata_excluded != "")
  i1_sub <- dplyr::semi_join(i1[["indices"]], i2_diff, by = c("year", "region"))
  expect_gt(nrow(i1[["indices"]]), nrow(i2[["indices"]]))
  expect_false(any(dplyr::select(i1_sub, dplyr::contains("Index")) ==
                     dplyr::select(i2_diff, dplyr::contains("Index"))))

  # number of routes / total number of routes might be less (never more)
  # There should be at least some less
  expect_true(all(i2_diff$n_routes <= i1_sub$n_routes))
  expect_true(any(i2_diff$n_routes <  i1_sub$n_routes))
  expect_true(all(i2_diff$n_routes_total <= i1_sub$n_routes_total))
  expect_true(any(i2_diff$n_routes_total <  i1_sub$n_routes_total))

  # Expect the same where no exclusion
  i2_same <- dplyr::filter(i2[["indices"]], .data$strata_excluded == "")
  i1_sub <- dplyr::semi_join(i1[["indices"]], i2_same, by = c("year", "region"))
  expect_true(all(dplyr::select(i1_sub, dplyr::contains("Index")) ==
                    dplyr::select(i2_same, dplyr::contains("Index"))))

  # Expect new indices based on exclusion
  prov <- unique(i2_diff$region)
  prov_samp <- paste0("prov_state_", prov)
  q <- 0.25
  y <- 10
  for(p in seq_along(prov)) {
    expect_equal(
      stats::quantile(i2[["samples"]][[prov_samp[p]]][, y], q),
      i2_diff[i2_diff$region == prov[p], ][[paste0("index_q_", q)]][y],
      ignore_attr = TRUE)
  }


  # Snapshots can't be run interactively
  snp <- dplyr::select(i2[["indices"]],
                       "year", "region", "n_non_zero", "backcast_flag",
                       "strata_included", "strata_excluded")
  expect_snapshot_value_safe(snp, style = "json2")
})
