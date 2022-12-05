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
#' @return Data frame with one row for each region included in the starting `indices` object. Contains `start_year`, `end_year`, `region`, `region-alt`, `region_type`,
#' `strata_included`, `strata_excluded`
#'   \item{Start_year}{first year of the trend}
#'   \item{End_year}{last year of the trend}
#'   \item{Region}{short name of the region}
#'   \item{Region_alt}{Long name for region}
#'   \item{Region_type}{Type of region including continental, national,Province_State,BCR, bcr_by_national, or stratum}
#'   \item{Strata_included}{Strata included in the trend and annual index calculations}
#'   \item{Strata_excluded}{Strata potentially excluded from the trend and annual index calculations because they have no observations of the species in the first part of the time series}
#'   \item{Trend}{Estimated mean annual percent change over the trend time-period (i.e., Start_year - End_year), according to an endpoint comparison of annual index in Start_year and the annual index in End_year}
#'   \item{Trend_Q_quantiles}{quantiles of the posterior distribution of Trend estimates, matching levels included in the quantiles argument}
#'   \item{Percent_Change}{Estimated total percent change over the trend time-period}
#'   \item{Percent_Change_Q_quantiles}{quantiles of the posterior distribution of Percent Change estimates, matching levels included in the quantiles argument}
#'   \item{Slope_Trend}{Estimated mean annual percent change over the trend time-period, according to the slope of a linear regression through the log-transformed annual indices}
#'   \item{Slope_Trend_Q_quantiles}{quantiles of the posterior distribution of Percent Change estimates, matching levels included in the quantiles argument}
#'   \item{prob_decrease_X_percent}{proportion of the posterior distribution of Percent_Change that is below the percentage values supplied in prob_decrease}
#'   \item{prob_increase_X_percent}{proportion of the posterior distribution of Percent_Change that is above the percentage values supplied in prob_increase}
#'   \item{Relative_Abundance}{Mean of the annual index values across all years. An estimate of the average relative abundance of the species in the region. Can be interepreted as the predicted average count of the species in an average year on an average route by an average observer, for the years, routes, and observers in the existing data}
#'   \item{Observed_Relative_Abundance}{Mean of the observed annual counts of birds across all routes and all years. An alternative estimate of the average relative abundance of the species in the region. For composite regions (i.e., anything other than stratum-level estimates) this average count is calculated as an area-weighted average across all strata included}
#'   \item{Number_of_Strata}{The number of strata included in the region}
#'   \item{Width_of_X_percent_Credible_Interval}{Width (in percent/year) of the credible interval on the Trend calculation. Calculated for the widest credible interval requested in quantiles argument. Default is 95 percent CI (i.e., Trend_Q0.975 - Trend_Q0.025)}
#'   \item{Width_of_X_percent_Credible_Interval_Slope}{Width (in percent/year) of the credible interval on the Trend calculation for the slope-based trend. Calculated for the widest credible interval requested in quantiles argument. Default is 95 percent CI (i.e., Slope_Trend_Q0.975 - Slope_Trend_Q0.025)}
#'   \item{Number_of_Routes}{The number of unique BBS routes included in the annual indices for this region and species, i.e., number of routes for this region and species for the years since \code{generate_indices(startyear)}}
#'   \item{Mean_Number_of_Routes}{The average number of BBS routes across years contributing data for this region and species}
#'   \item{backcast_flag}{approximate proportion of the included species range*years that are supported by data in a given region and year, e.g., 1.0 = data cover full time-series, 0.75 = data cover 75 percent of time-series. Only calculated if max_backcast != NULL}
#'
#' @examples
#'
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in a JAGS model.
#' d <- prepare_data(s, min_year = 2009, max_year = 2018)
#'
#' # Now run the model (fast but not good, just for illustration)
#' m <- run_model(d, model = "first_diff",
#'                iter_sampling = 5, iter_warmup = 5, chains = 2)
#'
#' # Generate the continental and stratum indices
#' i <- generate_indices(model_output = m)
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

  # For indexing by name in samples
  min_year_chr <- as.character(min_year)
  max_year_chr <- as.character(max_year)


  trends <- indx %>%
    dplyr::filter(.data$year %in% min_year:max_year) %>%
    dplyr::group_by(.data$region, .data$region_type,
                    .data$strata_included, .data$strata_excluded) %>%
    dplyr::summarize(
      # Add in samples
      n = purrr::map2(.data$region_type, .data$region,
                      ~indices$samples[[paste0(.x, "_", .y)]]),
      # Calculate change start to end for each iteration
      ch = purrr::map(n, ~.x[, .env$max_year_chr] / .x[, .env$min_year_chr]),
      # Calculate change as trend for each iteration
      tr = purrr::map(ch, ~100 * ((.x^(1/(.env$max_year - .env$min_year))) - 1)),

      # Median and percentiles of trend per region
      trend = purrr::map_dbl(tr, median),
      trend_q = purrr::map_df(
        tr, ~setNames(stats::quantile(.x, quantiles, names = FALSE),
                      paste0("trend_q", quantiles))),

      # Percent change and quantiles thereof per region
      percent_change = purrr::map_dbl(ch, ~100 * (median(.x) - 1)),
      pc_q = purrr::map_df(
        ch, ~setNames(100 * (stats::quantile(.x, quantiles, names = FALSE) - 1),
                      paste0("percent_change_q", quantiles))),

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
        strata_included, ~length(unlist(stringr::str_split(.x, " ; ")))),
      .groups = "drop") %>%

    dplyr::distinct() %>%
    tidyr::unnest(cols = c(.data$trend_q, .data$pc_q)) %>%
    dplyr::arrange(.data$region_type, .data$region)



  # Reliability Criteria
  q1 <- quantiles[1]
  q2 <- quantiles[length(quantiles)]
  q <- (q2 - q1) * 100

  trends <- trends %>%
    dplyr::mutate(
      "width_of_{{q}}_percent_credible_interval" :=
        .data[[paste0("trend_q", q2)]] - .data[[paste0("trend_q", q1)]])

  # Optional slope based trends
  if(slope) {
    trends <- trends %>%
      dplyr::mutate(
        sl_t = purrr::map(.data$n, calc_slope,
                          .env$min_year, .env$max_year),
        slope_trend = purrr::map_dbl(.data$sl_t, stats::median),
        slope_trend_q = purrr::map_df(
          sl_t, ~setNames(stats::quantile(.x, quantiles, names = FALSE),
                          paste0("slope_trend_q", quantiles)))) %>%
      tidyr::unnest(slope_trend_q) %>%
      dplyr::mutate(
        "width_of_{q}_percent_credible_interval_slope" :=
          .data[[paste0("slope_trend_q", q2)]] -
          .data[[paste0("slope_trend_q", q1)]])
  }

  # Model conditional probabilities of population change during trends period
  if(!is.null(prob_decrease)) {
    trends <- trends %>%
      dplyr::mutate(
        pch = purrr::map(.data$ch, ~100 * (.x - 1)),
        pch_pp = purrr::map_df(.data$pch, calc_prob_crease,
                               .env$prob_decrease, type = "decrease")) %>%
      tidyr::unnest(.data$pch_pp) %>%
      dplyr::select(-"pch")
  }

  if(!is.null(prob_increase)){
    trends <- trends %>%
      dplyr::mutate(
        pch = purrr::map(.data$ch, ~100 * (.x - 1)),
        pch_pp = purrr::map_df(.data$pch, calc_prob_crease,
                               .env$prob_increase, type = "increase")) %>%
      tidyr::unnest(.data$pch_pp) %>%
      dplyr::select(-"pch")
  }

  trends <- trends %>%
    dplyr::select(-"n", -"ch", -"tr") %>%
    dplyr::relocate("start_year", "end_year")

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

calc_slope <- function(n, min_year, max_year) {
  browser()
  wy <- 1:(1 + max_year - min_year) # Convert to numeric year (0, 1, 2, etc.)
  ne <- log(n[, as.character(min_year:max_year)]) # Extract from samples by name
  m <-  t(apply(ne, 1, FUN = bsl, wy))

  as.vector((exp(m) - 1) * 100)
}

calc_prob_crease <- function(x, p, type = "decrease") {
  if(type == "decrease") f <- function(p) length(x[x < (-1 * p)]) / length(x)
  if(type == "increase") f <- function(p) length(x[x > p]) / length(x)

  vapply(p, FUN = f, FUN.VALUE = 1.1) %>%
    setNames(paste0("prob_", type, "_", p, "_percent"))
}
