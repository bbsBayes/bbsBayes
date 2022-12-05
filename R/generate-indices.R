#' Regional annual indices of abundance
#'
#' Calculate annual indices of relative abundance by year for different regions.
#' These indices can then be used to plot population trajectories for the
#' species, and to estimate trends.
#'
#' @param quantiles Numeric. Vector of quantiles to be sampled from the
#'   posterior distribution. Default is
#'   `c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)`. Note that these quantiles
#'   will be used to create confidence interval bands in `plot_indices()` and
#'   by quantiles in `generate_trends()`, so make sure you specify the ones you
#'   want to use later in this step.
#' @param regions Character. Which region(s) to summarize and calculate indices
#'   for. Default is "continent" and "stratum". Options also include "country",
#'   "prov_state", "bcr", and "bcr_by_country". Note that some regions only
#'   apply to specific stratifications. You can also supply a custom region that
#'   exists as a column in the `regions_index` data frame (see
#'   examples for more details).
#' @param regions_index Data frame. Custom regions to summarize. Data frame must
#'   include all strata in the original data in one column (`strata_name`), and
#'   any custom regions defined as categories in other columns.
#' @param alternate_n Character. Indicating the name of the alternative annual
#'   index parameter in a model, Default is "n", alternatives are "n2" which
#'   involves a different way of scaling the annual indices, "n_smooth" for the
#'   gam and gamye models which show only the smooth component of the
#'   trajectory, and "n_slope" for the slope models which track only the linear
#'   slope component of the model.
#' @param max_backcast Numeric. The number of years to back cast stratum-level
#'   estimates before the first year that species was observed on any route in
#'   that stratum. Default is `NULL`, which generates annual indices for the
#'   entire time series and ignores back-casting. CWS national estimates use a
#'   back cast of 5. Note that unless `drop_exclude = TRUE`, problematic years
#'   are only flagged, not omitted. See Details for more specifics.
#' @param drop_exclude Logical. Whether or not strata that exceed the
#'   `max_backcast` threshold should be excluded from the calculations. Default
#'   is `FALSE` (regions are flagged and listed but not dropped).
#' @param start_year Numeric. Trim the data record before calculating annual
#'   indices.
#' @param jags_mod Defunct. Use `model_output` instead
#' @param jags_data Defunct.
#' @param alt_region_names Defunct. Use `regions_index` instead
#' @param startyear Deprecated. Use `start_year` instead
#'
#' @inheritParams common_docs
#'
#' @details
#'   `max_backcast` is a way to deal with the fact that the species of interest
#'   may not appear in the data until several years after the start of the record.
#'   `max_backcast` specifies how many years can occur before the stratum is flagged.
#'   A `max_backcast` of 5 will flag any stratum without a non-zero (or non-NA)
#'   observation within the first 5 years of the data record. Note that records
#'   are *only* flagged unless `drop_exclude = TRUE`.
#'   If you find that the early data record is sparse and results in the
#'   exclusion of many strata, consider trimming the early years by specifying a
#'   `start_year`.
#'
#'
#' @return List of 6 objects
#'   \item{data_summary}{dataframe with the following columns}
#'   \item{Year}{Year of particular index}
#'   \item{Region}{Region name}
#'   \item{Region_alt}{Long name for region}
#'   \item{Region_type}{Type of region including continental, national,Province_State,BCR, bcr_by_country, or stratum}
#'   \item{Strata_included}{Strata included in the annual index calculations}
#'   \item{Strata_excluded}{Strata potentially excluded from the annual index calculations because they have no observations of the species in the first part of the time series, see arguments max_backcast and start_year}
#'   \item{Index}{Strata-weighted count index}
#'   \item{additional columns for each of the values in quantiles}{quantiles of the posterior distribution}
#'   \item{obs_mean}{Mean of the observed annual counts of birds across all routes and all years. An alternative estimate of the average relative abundance of the species in the region and year. Differences between this and the annual indices are a function of the model. For composite regions (i.e., anything other than stratum-level estimates) this average count is calculated as an area-weighted average across all strata included}
#'   \item{nrts}{Number of BBS routes that contributed data for this species, region, and year}
#'   \item{nrts_total}{Number of BBS routes that contributed data for this species and region for all years in the selected time-series, i.e., all years since \code{start_year}}
#'   \item{nnzero}{Number of BBS routes on which this species was observed (i.e., count is > 0) in this region and year}
#'   \item{backcast_flag}{approximate annual average proportion of the covered species range that is free of extrapolated population trajectories. e.g., 1.0 = data cover full time-series, 0.75 = data cover 75 percent of time-series. Only calculated if max_backcast != NULL}
#'
#'   \item{samples}{array of all posterior draws}
#'   \item{area-weights}{data frame of the strata names and area weights used to calculate the continental estimates}
#'   \item{y_min}{first year used in the summary, scale 1:length of time-series}
#'   \item{y_max}{last year used in the summary, scale 1:length of time-series}
#'   \item{start_year}{first year used in the summary, scale 1966:2018}
#'
#' @examples
#'
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in modelling
#' d <- prepare_data(s,
#'                   min_year = 2009,
#'                   max_year = 2018)
#'
#' # Now run the model (fast but not good, just for illustration)
#' m <- run_model(d, model = "first_diff",
#'                iter_sampling = 20, iter_warmup = 20, chains = 2)
#'
#' # Generate the continental and stratum indices
#' i <- generate_indices(model_output = m)
#'
#' # Generate only country indices
#' i_nat <- generate_indices(model_output = m, regions = "country")
#'
#' # Use a custom region specification (dummy example)
#' library(dplyr)
#' ri <- bbs_strata[["bbs_cws"]] %>%
#'   mutate(my_region = if_else(prov_state %in% "ON", "Ontario", "Rest"))
#' i_custom <- generate_indices(model_output = m,
#'                              regions = c("country", "prov_state", "my_region"),
#'                              regions_index = ri)
#'
#' @export
#'

generate_indices <- function(model_output = NULL,
                             quantiles = c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975),
                             regions = c("stratum", "continent"),
                             regions_index = NULL,
                             alternate_n = "n",
                             start_year = NULL,
                             drop_exclude = FALSE,
                             max_backcast = NULL,
                             quiet = FALSE,
                             jags_mod, jags_data, alt_region_names, startyear) {

  # Deprecated/Defunct args
  if(!missing(jags_mod)) dep_stop("3.0.0", "jags_mod", "`model_output`")
  if(!missing(jags_data)) dep_stop("3.0.0", "jags_data")
  if(!missing(alt_region_names)) dep_stop("3.0.0", "alt_region_names", "`regions_index`")
  if(!missing(startyear)) {
    start_year <- startyear
    dep_warn("3.0.0", "startyear", "`start_year`")
  }

  # Checks
  check_data(model_output)

  # Get data
  stratify_by <- model_output$meta_data$stratify_by
  stratify_type <- model_output$meta_data$stratify_type
  raw_data <- model_output$raw_data
  meta_strata <- model_output$meta_strata

  check_regions(regions, stratify_by, stratify_type, regions_index)
  check_numeric(quantiles)
  check_numeric(start_year, max_backcast, allow_null = TRUE)
  check_logical(drop_exclude, quiet)

  # Start years
  if(!is.null(start_year)){
    inity <- min(raw_data$year)-1

    if(inity > start_year){
      warning(
        "Value of ", start_year, " for `start_year` is earlier than the ",
        "earliest year of the data, using ",
        start_year <- min(raw_data$year),
        " instead", call. = FALSE)
    }

  } else{
    start_year <- min(raw_data$year)
  }
  end_year <- max(raw_data$year)

  raw_data <- raw_data %>%
    # Set start year
    dplyr::group_by(.data[["strata"]]) %>%
    dplyr::mutate(first_year = min(.data$year[.data$count > 0], na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    # Trim year range
    dplyr::filter(.data$year >= .env$start_year)

  # After trimming data
  n_years <- max(raw_data$year_num) - min(raw_data$year_num) + 1

  # Backcast
  if(is.null(max_backcast)) max_backcast <- n_years

  # Posterior draws
  n <- samples_to_array(model_output, alternate_n,
                        years_to_keep = start_year:end_year)

  # Meta strata data
  meta_strata <- raw_data %>%
    dplyr::group_by(.data$strata) %>%
    dplyr::summarize(n_routes_total = dplyr::n_distinct(.data$route),
                     non_zero_weight = unique(.data$non_zero_weight)) %>%
    dplyr::left_join(meta_strata, by = "strata") %>%
    dplyr::mutate(stratum = .data$strata_name,
                  continent = "continent")

  # Adding extra regions
  if(!is.null(regions_index)) {

    # Check if strata_names don't match
    if(!all(meta_strata$strata_name %in% regions_index$strata_name)){
      stop("'strata_name's in the `regions_index` don't match 'strata_name's ",
           "in the data. ",
           "See `model_output$meta_strata` for the strata to match",
           call. = FALSE)
    }

    # Keep only relevant regions
    r <- regions[!regions %in% c("continent", "stratum")]
    regions_index <- regions_index %>%
      dplyr::select("strata_name", dplyr::all_of(r)) %>%
      dplyr::arrange(.data$strata_name) %>%
      dplyr::distinct() %>%
      dplyr::mutate(strata_name = as.character(strata_name))

    # Add new regional definitions to existing meta_strata
    meta_strata <- meta_strata %>%
      dplyr::select(-dplyr::any_of(r)) %>% # Remove any existing regions
      dplyr::left_join(regions_index, by = "strata_name") # Join in new
  }

  # Calculate strata/year-level observation statistics
  obs_strata <- raw_data %>%
    dplyr::select("strata", "year", "first_year", "count") %>%
    dplyr::group_by(.data$strata) %>%
    tidyr::complete(year = seq(.env$start_year, .env$end_year), first_year) %>%
    dplyr::arrange(.data$strata, .data$year, .data$count) %>%
    dplyr::group_by(.data$strata, .data$year, .data$first_year) %>%
    dplyr::summarize(obs_mean = mean(.data$count, na.rm = TRUE),
                     n_routes = sum(!is.na(.data$count)),
                     n_non_zero = sum(.data$count > 0, na.rm = TRUE),
                     strata_remove_flag = 0, .groups = "drop")

  indices <- dplyr::tibble()
  N_all <- list()

  for(rr in regions) { #selecting the type of composite region

    if(!quiet) message("Processing region ", rr)

    # Calculate strata-level information for sub-regions in this composite region
    meta_strata_sub <- meta_strata %>%
      # Ensure region columns are character
      dplyr::mutate("{rr}" := as.character(.data[[rr]])) %>%
      dplyr::group_by(.data[[rr]]) %>%
      dplyr::mutate(
        pz_area = .data$area_sq_km * .data$non_zero_weight,
        strata_p = .data$pz_area / sum(.data$pz_area),
        area_weight = .data$area_sq_km / sum(.data$area_sq_km),
        area_weight_non_zero = .data$area_weight * .data$non_zero_weight) %>%
      dplyr::ungroup()

    # Calculate observation statistics for this composite region
    obs_region <- obs_strata %>%
      dplyr::inner_join(meta_strata_sub, by = "strata") %>%
      dplyr::mutate(obs_mean = obs_mean * area_weight_non_zero) %>%
      dplyr::group_by(.data[[rr]], .data$strata)

    # Flag strata to remove due to max_backcast
    # - Flag first max_backcast no. years IF:
    #    - If no obs in those years, AND
    #    - first_year is AFTER the current start of the data range
    #      (i.e. flag data that has no true counts in it.)

    obs_region <- obs_region %>%
      dplyr::mutate(
        flag_remove = sum(.data$n_non_zero[seq_len(.env$max_backcast)]) < 1 &
          .data$first_year > .env$start_year,
        flag_year = dplyr::if_else(.data$flag_remove &
                                     .data$year <= .data$first_year, # should be <?
                                   .data$strata_p, 0))

    # Mark strata included/excluded
    obs_region <- obs_region %>%
      dplyr::group_by(.data[[rr]], .data$year) %>%
      dplyr::mutate(
        strata_included = paste0(.data$strata_name[!.data$flag_remove],
                                 collapse = " ; "),
        strata_excluded = paste0(.data$strata_name[.data$flag_remove],
                                 collapse = " ; "))

    # Exclude if requested
    if(drop_exclude) {
      rm <- unique(obs_region$strata_name[obs_region$flag_remove])

      obs_region <- dplyr::filter(obs_region, !.data$flag_remove)
      meta_strata_sub <- dplyr::filter(meta_strata_sub, !strata_name %in% rm)
      n_sub <- n[, unique(obs_region$strata_name), ] # Keep only good
    } else n_sub <- n

    obs_region <- obs_region %>%
      dplyr::group_by(.data$strata_included, .data$strata_excluded,
                      .add = TRUE) %>%
      dplyr::summarize(
        dplyr::across(.cols = c(.data$obs_mean, .data$n_routes,
                                .data$n_routes_total,
                                .data$n_non_zero, .data$flag_year),
                      sum, na.rm = TRUE),
        .groups = "drop")

    # Calculate sample statistics for this composite region
    samples <- meta_strata_sub %>%
      # Create back up col for use in calculations
      tidyr::nest(data = -.data[[rr]]) %>%
      dplyr::group_by(.data[[rr]]) %>%
      dplyr::summarize(N = purrr::map(.data$data, calc_weights, .env$n_sub),
                       N_names = paste0(rr, "_", .data[[rr]]),
                       Q = purrr::map(.data$N, calc_quantiles,
                                      .env$quantiles)) %>%
      dplyr::mutate(r = .env$rr)

    # Save sample stats for output
    N_all <- append(N_all, setNames(samples$N, samples$N_names))

    # Calculate data summaries for output
    indices <- obs_region %>%
      #dplyr::left_join(calc_alt_names(rr, meta_strata), by = rr) %>%
      dplyr::mutate(backcast_flag = 1 - .data$flag_year,
                    region_type = .env$rr) %>%
      # Add in quantiles
      dplyr::left_join(tidyr::unnest(samples, "Q"), by = c(rr, "year")) %>%
      # Clean up
      dplyr::rename(region = .data[[rr]]) %>%
      dplyr::select("year", "region", "region_type",
                    "strata_included", "strata_excluded",
                    "index", dplyr::contains("index_q"),
                    "obs_mean", "n_routes", "n_routes_total", "n_non_zero",
                    "backcast_flag") %>%
      dplyr::bind_rows(indices, .)
  }

  meta_strata <- dplyr::select(meta_strata,
                               "strata_name", "strata", "area_sq_km",
                               dplyr::all_of(.env$regions))

  list("indices" = indices,
       "samples" = N_all,
       "meta_data" = append(model_output$meta_data,
                            list("regions" = regions,
                                 "start_year" = start_year,
                                 "n_years" = n_years)),
       "meta_strata" = meta_strata,
       "raw_data" = model_output$raw_data # Original data before trimming
       )
}







calc_weights <- function(data, n) {

  # Weight each sampled n
  n_weight <- n[, data$strata_name, , drop = FALSE]

  # Use numbers for indexing as is slightly faster
  for (i in seq_len(dim(n_weight)[1])) {       # iter
    for (j in seq_len(dim(n_weight)[2])) {     # strata_name
      n_weight[i, j, ] <- n_weight[i, j, ] * data$area_weight[j]

    }
  }

  # Sum over strata
  apply(n_weight, c(1, 3), sum)
}

calc_quantiles <- function(N, quantiles) {
  apply(N, 2, stats::quantile, probs = c(quantiles, 0.5)) %>%
    t() %>%
    as.data.frame() %>%
    setNames(c(paste0("index_q_", quantiles), "index")) %>%
    dplyr::bind_cols(year = as.numeric(dimnames(N)$year))
}

calc_alt_names <- function(r, region_names) {
  col_region_name <- dplyr::case_when(r == "prov_state" ~ "province_state",
                                      TRUE ~ r)

  region_alt_name <- dplyr::bind_cols(
    {{r}} := region_names[[r]],
    region_alt = region_names[[col_region_name]]) %>%
    dplyr::distinct()

  if(r == "bcr") {
    region_alt_name <- dplyr::mutate(region_alt_name,
                                     region_alt = paste0("BCR_", .data$region_alt))
  }

  region_alt_name
}



#' Convert Stan samples matrix to array
#'
#' Stan is all matrix [samples, strata_x_years]. Here we convert it to an
#' array with by splitting strata and years into separate dimensions.
#'
#' Looks like the order of strata_x_years is S1Y1 S2Y1 S3Y1, etc.
#'
#' @param model_output Model output from `run_model()`
#' @param alternative_n Variable to extract draws for
#' @param years_to_keep Numeric vector. All the years (1995, 1996, etc.) to
#'   retain in the samples array
#'
#' @return Three dimensional array, samples x strata x years
#' @noRd

samples_to_array <- function(model_output, alternate_n, years_to_keep) {

  # Extract samples
  n <- model_output$model_fit$draws(variables = alternate_n,
                                    format = "draws_matrix")
  # Determine dim names
  strata_name <- unique(model_output$raw_data$strata_name)
  year <- sort(unique(model_output$raw_data$year))

  # Transform samples to array with appropriate dimnames
  n <- array(as.vector(n),
             dim = c(posterior::ndraws(n), length(strata_name), length(year)),
             dimnames = list("iter" = 1:posterior::ndraws(n),
                             "strata_name" = strata_name,
                             "year" = year))

  # Filter to years selected
  years_to_keep <- years_to_keep[years_to_keep %in% year]
  n[ , , as.character(years_to_keep)]
}
