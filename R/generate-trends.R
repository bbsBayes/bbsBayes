#' Generate regional trends
#'
#' Generates trends for continent and strata and optionally for countries,
#' states/provinces, or BCRs from analyses run on the stratifications that
#' support these composite regions. Calculates the geometric mean annual changes
#' in population size for composite regions.
#'
#' @param quantiles Numeric vector. Quantiles to be sampled from the posterior
#'   distribution. Defaults to `c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)`
#' @param slope Logical. Whether to calculate an alternative trend metric, the
#'   slope of a log-linear regression through the annual indices. Default
#' `FALSE`.
#' @param prob_decrease Numeric vector. Percent-decrease values for which to
#'   optionally calculate the posterior probabilities (see Details). Default is
#'   `NULL` (do not calculate).
#' @param prob_increase Numeric vector. Percent-increase values for which to
#'   optionally calculate the posterior probabilities (see Details). Default is
#'   `NULL` (do not calculate).
#' @param Min_year Deprecated. Use `min_year` instead
#' @param Max_year Deprecated. Use `max_year` instead
#'
#' @inheritParams common_docs
#'
#' @details
#'   The posterior probabilities can be calculated for a percent-decrease
#'   (`prob_decrease`) and/or percent-increase (`prob_increase`) if desired.
#'   These calculate the probability that the populationhas decreased/increased
#'   by at least the amount specified.
#'
#'   For example, a `prob_increase = 100` would result in the calculation of the
#'   probability that the population has increased by more than 100% (i.e.,
#'   doubled) over the period of the trend.
#'
#'   Alternatively, a `prob_decrease = 50` would result in the calculation of
#'   the probability that the population has decreased by more than 50% (i.e.,
#'   less than half of the population remains) over the period of the trend.
#'
#' @return A list containing the trends (`trends`), meta data for the analysis
#'   (`meta_data`), meta data for the strata (`meta_strata`) and prepared data
#'   counts from `prepare_data()` (`raw_data`).
#'
#'   `trends` is a data frame with one row for each region in the input
#'   `indices`. It has the following columns:
#'
#'   - `start_year` - First year of the trend
#'   - `end_year` - Last year of the trend
#'   - `region` - Region name
#'   - `region_type` - Type of region
#'   - `strata_included` - Strata *potentially* included in the annual index
#'   calculations
#'   - `strata_excluded` - Strata *potentially* excluded from the annual index
#'   calculations because they have no observations of the species in the first
#'   part of the time series, see arguments `max_backcast` and `start_year`
#'   - `trend` - Estimated median annual percent change over the trend
#'   time-period according to end point comparison of annual indices for the
#'   `start_year` and the `end_year`
#'   - `trend_q_XXX` - Trend estimates by different quantiles
#'   - `percent_change` - Median overall estimate percent change over the trend
#'   time-period
#'   - `percent_change_q_XXX` - Percent change by different quantiles
#'   - `slope_trend` - Estimated median annual percent change over the trend
#'   time-period, according to the slope of a linear regression through the
#'   log-transformed annual indices. (Only if `slope = TRUE`)
#'   - `slope_trend_q_XXX` - Slope-based trend estimates by different quantiles.
#'   (Only if `slope = TRUE`)
#'   - `width_of_95_percent_credible_interval` - Width (in percent/year) of the
#'   credible interval on the trend calculation. Calculated for the widest
#'   credible interval requested in via `quantiles`. Default is 95 percent CI
#'   (i.e., `trend_q_0.975` - `trend_q_0.025`)
#'   - `width_of_95_percent_credible_interval_slope` - Width (in percent/year)
#'   of the credible interval on the slope-based trend calculation. Calculated
#'   for the widest credible interval requested in via `quantiles`. Default is
#'   95 percent CI (i.e., `slope_trend_q_0.975` - `slope_trend_q_0.025`). (Only
#'   if `slope = TRUE`)
#'   - `prob_decrease_XX_percent` - Proportion of the posterior distribution of
#'   `percent_change` that is below the percentage values in
#'   `prob_decrease` (if non-`Null`)
#'   - `prob_increase_XX_percent` - Proportion of the posterior distribution of
#'   `percent_change` that is above tthe percentage values in
#'   `prob_increase` (if non-`Null`)
#'   - `rel_abundance` - Mean annual index value across all years. An estimate
#'   of the average relative abundance of the species in the region. Can be
#'   interpreted as the predicted average count of the species in an average
#'   year on an average route by an average observer, for the years, routes, and
#'   observers in the existing data
#'   - `obs_rel_abundance` - Mean observed annual count of birds across all
#'   routes and all years. An alternative estimate of the average relative
#'   abundance of the species in the region. For composite regions (i.e.,
#'   anything other than stratum-level estimates) this average count is
#'   calculated as an area-weighted average across all strata included.
#'   - `n_routes` - Number of BBS routes that contributed data for this
#'   species and region for all years in the selected time-series, i.e., all
#'   years since `start_year`
#'   - `mean_n_routes` - Mean number of BBS routes that contributed data for
#'   this species, region, and year
#'   - `n_strata_included` - The number of strata included in the region
#'   - `backcast_flag` - Approximate annual average proportion of the covered
#'   species range that is free of extrapolated population trajectories. e.g.,
#'   if 1.0, data cover full time-series; if 0.75, data cover 75 percent of
#'   time-series. Only calculated if `max_backcast != NULL`.
#'
#'
#' @examples
#'
#' # Using the example model for Pacific Wrens...
#'
#' # Generate the continental and stratum indices#'
#' i <- generate_indices(pacific_wren_model)
#'
#' # Now, generate the trends
#' t <- generate_trends(i)
#'
#'
#' @export
#'

generate_trends <- function(indices,
                            min_year = NULL,
                            max_year = NULL,
                            quantiles = c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975),
                            slope = FALSE,
                            prob_decrease = NULL,
                            prob_increase = NULL,
                            Min_year, Max_year) {

  # Deprecated/Defunct args
  if(!missing(Min_year)) {
    dep_warn("3.0.0", "Min_year", "`min_year`")
    min_year <- Min_year
  }
  if(!missing(Max_year)) {
    dep_warn("3.0.0", "Max_year", "`max_year`")
    max_year <- Max_year
  }

  # Checks
  check_data(indices)
  check_logical(slope)
  check_numeric(quantiles)
  check_numeric(min_year, max_year, quantiles, prob_decrease, prob_increase,
                allow_null = TRUE)
  check_range(quantiles, c(0, 1))
  check_range(prob_decrease, c(1, 100))
  check_range(prob_increase, c(1, 100))

  start_year <- indices[["meta_data"]]$start_year
  n_years <- indices[["meta_data"]]$n_years
  indx <- indices[["indices"]]

  if(is.null(min_year)) {
    min_year <- start_year
  } else {

    if(min_year < start_year) {
      message("`min_year` is before the date range, using minimum year of ",
              "the data (", min_year <- start_year, ") instead.")
    }
  }

  if (is.null(max_year)) {
    max_year <- max(indx$year)
  } else if(max_year > max(indx$year)) {
    message("`max_year` is beyond the date range, using maximum year of ",
            "the data (", max_year <- max(indx$year), ") instead.")
  }

  # For indexing
  min_year_num <- min_year - start_year + 1
  max_year_num <- max_year - start_year + 1


  trends <- indx %>%
    dplyr::filter(.data$year %in% min_year:max_year) %>%
    dplyr::group_by(.data$region, .data$region_type,
                    .data$strata_included, .data$strata_excluded) %>%
    dplyr::summarize(
      # Add in samples
      n = purrr::map2(.data$region_type, .data$region,
                      ~indices$samples[[paste0(.x, "_", .y)]]),
      # Calculate change start to end for each iteration
      ch = purrr::map(.data$n,
                      ~.x[, .env$max_year_num] / .x[, .env$min_year_num]),
      # Calculate change as trend for each iteration
      tr = purrr::map(
        .data$ch,
        ~100 * ((.x^(1/(.env$max_year_num - .env$min_year_num))) - 1)),

      # Median and percentiles of trend per region
      trend = purrr::map_dbl(.data$tr, stats::median),
      trend_q = purrr::map_df(
        .data$tr,
        ~stats::setNames(stats::quantile(.x, quantiles, names = FALSE),
                         paste0("trend_q_", quantiles))),

      # Percent change and quantiles thereof per region
      percent_change = purrr::map_dbl(.data$ch, ~100 * (stats::median(.x) - 1)),
      pc_q = purrr::map_df(
        .data$ch, ~stats::setNames(
          100 * (stats::quantile(.x, quantiles, names = FALSE) - 1),
          paste0("percent_change_q_", quantiles))),

      # Other statistics
      rel_abundance = mean(.data$index),
      obs_rel_abundance = mean(.data$obs_mean),
      mean_n_routes = mean(.data$n_routes),
      n_routes = mean(.data$n_routes_total),
      backcast_flag = mean(.data$backcast_flag),

      # Metadata
      start_year = .env$min_year,
      end_year = .env$max_year,
      n_strata_included = purrr::map_dbl(
        .data$strata_included, ~length(unlist(stringr::str_split(.x, " ; ")))),
      .groups = "drop") %>%

    dplyr::distinct() %>%
    tidyr::unnest(cols = c("trend_q", "pc_q")) %>%
    dplyr::arrange(.data$region_type, .data$region)



  # Reliability Criteria
  q1 <- quantiles[1]
  q2 <- quantiles[length(quantiles)]
  q <- (q2 - q1) * 100

  trends <- trends %>%
    dplyr::mutate(
      "width_of_{{q}}_percent_credible_interval" :=
        .data[[paste0("trend_q_", q2)]] - .data[[paste0("trend_q_", q1)]])

  # Optional slope based trends
  if(slope) {
    trends <- trends %>%
      dplyr::mutate(
        sl_t = purrr::map(.data$n, calc_slope,
                          .env$min_year_num, .env$max_year_num),
        slope_trend = purrr::map_dbl(.data$sl_t, stats::median),
        slope_trend_q = purrr::map_df(
          .data$sl_t, ~stats::setNames(
            stats::quantile(.x, quantiles, names = FALSE),
            paste0("slope_trend_q_", quantiles)))) %>%
      tidyr::unnest("slope_trend_q") %>%
      dplyr::mutate(
        "width_of_{q}_percent_credible_interval_slope" :=
          .data[[paste0("slope_trend_q_", q2)]] -
          .data[[paste0("slope_trend_q_", q1)]])
  }

  # Model conditional probabilities of population change during trends period
  if(!is.null(prob_decrease)) {
    trends <- trends %>%
      dplyr::mutate(
        pch = purrr::map(.data$ch, ~100 * (.x - 1)),
        pch_pp = purrr::map_df(.data$pch, calc_prob_crease,
                               .env$prob_decrease, type = "decrease")) %>%
      tidyr::unnest("pch_pp") %>%
      dplyr::select(-"pch")
  }

  if(!is.null(prob_increase)){
    trends <- trends %>%
      dplyr::mutate(
        pch = purrr::map(.data$ch, ~100 * (.x - 1)),
        pch_pp = purrr::map_df(.data$pch, calc_prob_crease,
                               .env$prob_increase, type = "increase")) %>%
      tidyr::unnest("pch_pp") %>%
      dplyr::select(-"pch")
  }

  trends <- trends %>%
    dplyr::select(-"n", -"ch", -"tr") %>%
    dplyr::select(
      "start_year", "end_year", "region", "region_type",
      "strata_included", "strata_excluded",
      dplyr::starts_with("trend"),
      dplyr::starts_with("percent_change"),
      dplyr::starts_with("slope"),
      dplyr::starts_with("width"),
      dplyr::starts_with("prob"),
      "rel_abundance", "obs_rel_abundance",
      "n_routes", "mean_n_routes",
      "n_strata_included", "backcast_flag")


  list("trends" = trends,
       "meta_data" = indices[["meta_data"]],
       "meta_strata" = indices[["meta_strata"]],
       "raw_data" = indices[["raw_data"]])
}

bsl <- function(i, wy) {
  n <- length(wy)
  sy <- sum(i)
  sx <- sum(wy)
  ssx <- sum(wy^2)
  sxy <- sum(i*wy)

  (n * sxy - sx * sy) / (n * ssx - sx^2)
}

calc_slope <- function(n, min_year_num, max_year_num) {
  wy <- min_year_num:max_year_num

  ne <- log(n[, wy])
  m <-  t(apply(ne, 1, FUN = bsl, wy))

  as.vector((exp(m) - 1) * 100)
}

calc_prob_crease <- function(x, p, type = "decrease") {
  if(type == "decrease") f <- function(p) length(x[x < (-1 * p)]) / length(x)
  if(type == "increase") f <- function(p) length(x[x > p]) / length(x)

  vapply(p, FUN = f, FUN.VALUE = 1.1) %>%
    stats::setNames(paste0("prob_", type, "_", p, "_percent"))
}
