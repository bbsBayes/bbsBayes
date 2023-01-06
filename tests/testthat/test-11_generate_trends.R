
# Setup first set of indices

expect_silent({
  i <- pacific_wren_model %>%
    generate_indices(quiet = TRUE)
  ix <- i[["indices"]] %>% dplyr::arrange(region_type, region, year)
})


test_that("bsl()", {
  expect_equal(bsl(1:10, 1:10), 1)
  expect_equal(bsl(21:40, 1:20), 1)
  expect_equal(bsl(seq(21, 60, length.out = 20), 2:21),
               2.0526316, tolerance = 0.0001)
})

test_that("calc_slope()", {

  expect_silent(sl <- calc_slope(i[["samples"]][["continent_continent"]],
                                 1, dim(i[["samples"]][[1]])[2]))
  expect_equal(sl[1], 0.6569389, tolerance = 0.0001)
  expect_equal(sl[10], 0.5405611, tolerance = 0.0001)
  expect_equal(sl[20], 0.6839494, tolerance = 0.0001)

})

test_that("calc_prob_crease", {

  x <- c(-10, -20, -5, 0, -20)
  expect_equal(calc_prob_crease(x, 1),
               setNames(0.8, "prob_decrease_1_percent"))
  expect_equal(calc_prob_crease(x, 1, "increase"),
               setNames(0, "prob_increase_1_percent"))

  expect_equal(calc_prob_crease(x, c(10, 15)),
               setNames(c(0.4, 0.4), c("prob_decrease_10_percent",
                                       "prob_decrease_15_percent")))
  expect_equal(calc_prob_crease(x, c(10, 15), "increase"),
               setNames(c(0, 0), c("prob_increase_10_percent",
                                   "prob_increase_15_percent")))
})

test_that("generate_trends() basics", {

  expect_silent(t <- generate_trends(i))
  expect_named(t, c("trends", "meta_data", "meta_strata", "raw_data"))
  expect_s3_class(trnd <- t[["trends"]], "data.frame")
  expect_equal(nrow(trnd), length(unique(ix$region)))
  expect_equal(trnd$start_year[1], min(ix$year))
  expect_equal(trnd$end_year[1], max(ix$year))
  expect_true(all(trnd$strata_included %in% unique(ix$strata_included)))
  expect_true(all(trnd$strata_excluded %in% unique(ix$strata_excluded)))

  # See if trend calc is as it should be
  ch <-
    i[["samples"]][["continent_continent"]][, as.character(trnd$end_year[1])] /
    i[["samples"]][["continent_continent"]][, as.character(trnd$start_year[1])]
  tr <- 100 * ((ch ^ (1 / (trnd$end_year[1] - trnd$start_year[1]))) - 1)

  expect_equal(trnd$trend[1], median(tr))
  expect_equal(trnd$trend_q_0.025[1], quantile(tr, 0.025), ignore_attr = TRUE)
  expect_equal(trnd$percent_change[1], 100 * (median(ch) - 1))
  expect_equal(trnd$percent_change_q_0.025[1], 100 * (quantile(ch, 0.025) - 1),
               ignore_attr = TRUE)
  expect_equal(trnd$rel_abundance[1], mean(ix$index[ix$region == "continent"]))
  expect_equal(trnd$obs_rel_abundance[1],
               mean(ix$obs_mean[ix$region == "continent"]))
  expect_equal(trnd$n_routes[1],
               mean(ix$n_routes_total[ix$region == "continent"]))
  expect_equal(trnd$mean_n_routes[1],
               mean(ix$n_routes[ix$region == "continent"]))
  expect_equal(trnd$backcast_flag[1],
               mean(ix$backcast_flag[ix$region == "continent"]))
  expect_equal(trnd$n_strata_included,
               stringr::str_count(trnd$strata_included, ";") + 1)
  expect_equal(trnd$width_of_95_percent_credible_interval,
               trnd$trend_q_0.975 - trnd$trend_q_0.025)
})

test_that("generate_trends() slopes", {

  expect_silent(trnd <- generate_trends(i, slope = TRUE)[["trends"]])

  expect_silent(s <- calc_slope(i[["samples"]][["continent_continent"]],
                                1, dim(i[["samples"]][[1]])[2]))

  expect_equal(trnd$slope_trend[1], median(s))
  expect_equal(trnd$slope_trend_q_0.025[1], quantile(s, 0.025),
               ignore_attr = TRUE)

  expect_equal(trnd$width_of_95_percent_credible_interval_slope,
               trnd$slope_trend_q_0.975 - trnd$slope_trend_q_0.025)
})

test_that("generate_trends() min_year/max_year", {

  # Error/message
  expect_message(generate_trends(i, min_year = 1900), "`min_year` is before")
  expect_message(generate_trends(i, max_year = 9999), "`max_year` is beyond")

  # min_year
  expect_silent(trnd <- generate_trends(i, min_year = 2007)[["trends"]])
  expect_true(all(trnd$start_year == 2007))
  expect_false(all(trnd$start_year == min(ix$year)))
  expect_true(all(trnd$end_year == max(ix$year)))

  # max_year
  expect_silent(trnd <- generate_trends(i, max_year = 2007)[["trends"]])
  expect_true(all(trnd$end_year == 2007))
  expect_false(all(trnd$end_year == max(ix$year)))
  expect_true(all(trnd$start_year == min(ix$year)))

  # both
  expect_silent(trnd <- generate_trends(i, min_year = 2000,
                                         max_year = 2007)[["trends"]])
  expect_true(all(trnd$start_year == 2000))
  expect_true(all(trnd$end_year == 2007))
  expect_false(all(trnd$end_year == max(ix$year)))
  expect_false(all(trnd$start_year == min(ix$year)))

  # Expect samples truncated
  ch <-
    i[["samples"]][["continent_continent"]][, as.character(trnd$end_year[1])] /
    i[["samples"]][["continent_continent"]][, as.character(trnd$start_year[1])]
  tr <- 100 * ((ch ^ (1 / (trnd$end_year[1] - trnd$start_year[1]))) - 1)
  expect_equal(trnd$trend[1], median(tr))
})


test_that("generate_trends() quantiles", {

  # Error/message
  expect_error(generate_trends(i, quantiles = 2), "range between 0 and 1")

  expect_silent(trnd <- generate_trends(i, quantiles = c(0.33, 0.67)))
  trnd <- trnd[["trends"]]

  # Expect samples summarized with diff quantiles
  ch <-
    i[["samples"]][["continent_continent"]][, as.character(trnd$end_year[1])] /
    i[["samples"]][["continent_continent"]][, as.character(trnd$start_year[1])]
  tr <- 100 * ((ch ^ (1 / (trnd$end_year[1] - trnd$start_year[1]))) - 1)

  expect_equal(trnd$trend[1], median(tr))
  expect_equal(trnd$trend_q_0.33[1], quantile(tr, 0.33), ignore_attr = TRUE)
  expect_equal(trnd$trend_q_0.67[1], quantile(tr, 0.67), ignore_attr = TRUE)

  nm <- paste0("width_of_", 100 * (0.67-0.33), "_percent_credible_interval")
  expect_equal(trnd[[nm]], trnd$trend_q_0.67 - trnd$trend_q_0.33)

})

test_that("generate_trends() prob_decrease/prob_increase", {

  # Error/message
  expect_error(generate_trends(i, prob_decrease = 0.2),
               "range between 1 and 100")
  expect_error(generate_trends(i, prob_increase = 0.2),
               "range between 1 and 100")

  expect_silent(trnd <- generate_trends(i, prob_decrease = 10)[["trends"]])
  expect_true("prob_decrease_10_percent" %in% names(trnd))
  expect_equal(trnd$prob_decrease_10_percent[1:4], c(0, 0.15, 0.0, 0.65))

  expect_silent(trnd <- generate_trends(i, prob_increase = 10)[["trends"]])
  expect_true("prob_increase_10_percent" %in% names(trnd))
  expect_equal(trnd$prob_increase_10_percent[1:4], c(0.975, 0.725, 0.975, 0.25))

  expect_silent(
    trnd <- generate_trends(i, prob_decrease = c(10, 20))[["trends"]])
  expect_true(all(c("prob_decrease_10_percent", "prob_decrease_20_percent")
                   %in% names(trnd)))

  expect_silent(
    trnd <- generate_trends(i, prob_increase = c(10, 20))[["trends"]])
  expect_true(all(c("prob_increase_10_percent", "prob_increase_20_percent")
                  %in% names(trnd)))

  expect_silent(trnd <- generate_trends(i, prob_increase = c(10, 20),
                                        prob_decrease = c(15, 30))[["trends"]])
  expect_true(all(c("prob_increase_10_percent", "prob_increase_20_percent",
                    "prob_decrease_15_percent", "prob_decrease_30_percent")
                  %in% names(trnd)))

})



test_that("generate_trends() indices diff start_year", {
  i <- pacific_wren_model %>%
    generate_indices(start_year = 2007, quiet = TRUE)
  ix <- i[["indices"]] %>% dplyr::arrange(region_type, region, year)

  expect_silent(trnd <- generate_trends(i)[["trends"]])
  expect_true(all(trnd$start_year == 2007))
  expect_true(all(trnd$start_year == min(ix$year)))
  expect_true(all(trnd$end_year == max(ix$year)))

  # Expect samples truncated
  ch <-
    i[["samples"]][["continent_continent"]][, as.character(trnd$end_year[1])] /
    i[["samples"]][["continent_continent"]][, as.character(trnd$start_year[1])]
  tr <- 100 * ((ch ^ (1 / (trnd$end_year[1] - trnd$start_year[1]))) - 1)
  expect_equal(trnd$trend[1], median(tr))
})

