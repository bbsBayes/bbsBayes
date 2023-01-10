#' Filter for data quality
#'
#' Check and filter the stratified data by minimum required samples for
#' modelling, and prepare data format for use by models.
#'
#' @param min_n_routes Numeric. Required minimum routes per strata where species
#'   has been observed. Default 3.
#' @param min_max_route_years Numeric. Required minimum number of years with
#'   non-zero observations of species on at least 1 route. Default 3. Only
#'   retain strata with at least one route where the species was observed at
#'   least once in this many years.
#' @param min_mean_route_years Numeric. Required minimum average of years per
#'   route with the species observed. Default 1. Only retain strata where the
#'   average number of years the species was observed per route is greater than
#'   this value.
#' @param species Defunct. Use `species` in `stratify()` instead
#' @param model Defunct. Use `model` in `prepare_model()` instead
#' @param heavy_tailed Defunct. Use `heavy_tailed` in `prepare_model()` instead
#' @param n_knots Defunct. Use `n_knots` in `prepare_model()` instead
#' @param basis Defunct. Use `basis` in `prepare_model()` instead
#' @param sampler Defunct.
#' @param strat_data Defunct. Use `strata_data` instead
#' @param strata_rem Defunct. Use `custom_strata` in `stratify()` instead
#'
#' @inheritParams common_docs
#'
#' @return List of prepared (meta) data to be used for modelling and further
#'   steps.
#'   - `model_data` list of data formatted for use in Stan modelling
#'   - `meta_data` meta data defining the analysis
#'   - `meta_strata` data frame listing strata meta data
#'   - `raw_data` contains a data frame of summarized data used to create
#'     `model_data` (just formatted more nicely)
#'
#' @export
#'
#' @examples
#' # Toy example with Pacific Wren sample data
#'
#' # First, stratify the sample data
#'
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in a model. In this
#' #   toy example, we will set the minimum year as 2009 and
#' #   maximum year as 2018, effectively only setting up to
#' #   model 10 years of data.
#'
#' p <- prepare_data(s, min_year = 2009, max_year = 2018)

prepare_data <- function(strata_data,
                         min_year = NULL,
                         max_year = NULL,
                         min_n_routes = 3,
                         min_max_route_years = 3,
                         min_mean_route_years = 1,
                         quiet = FALSE,
                         species,
                         model, heavy_tailed, n_knots, basis, sampler,
                         strat_data, strata_rem) {

  # Deprecated/Defunct args
  if(!missing(species)) {
    dep_stop("3.0.0", "species", "the `species` argument in `stratify()`")
  }
  if(!missing(model)) {
    dep_stop("3.0.0", "model", "the `model` argument in `prepare_model()`")
  }
  if(!missing(heavy_tailed)) {
    dep_stop("3.0.0", "heavy_tailed",
             "the `heavy_tailed` argument in `prepare_model()`")
  }
  if(!missing(n_knots)) {
    dep_stop("3.0.0", "n_knots", "the `n_knots` argument in `prepare_model()`")
  }
  if(!missing(basis)) {
    dep_stop("3.0.0", "basis", "the `basis` argument in `prepare_model()`")
  }
  if(!missing(sampler)) dep_stop("3.0.0", "sampler")
  if(!missing(strat_data)) dep_stop("3.0.0", "strat_data", "`strata_data`")
  if(!missing(strata_rem)) {
    dep_stop("3.0.0", "strata_rem",
             "the `custom_strata` argument in `stratify()`")
  }

  # Checks
  if(missing(strata_data)) stop("Missing `strata_data`", call. = FALSE)
  check_data(strata_data)
  check_numeric(min_year, max_year, allow_null = TRUE)
  check_numeric(min_n_routes, min_max_route_years, min_mean_route_years)
  check_logical(quiet)

  # Get observations of interest
  obs <- strata_data$birds_strata %>%
    dplyr::rename(count = "species_total") %>%
    dplyr::select("route", "count", "year", "rpid")

  # Add in routes
  obs <- strata_data$routes_strata %>%
    dplyr::select("country_num", "state_num", "state", "rpid", "bcr", "year",
                  "strata_name", "route", "obs_n") %>%
    dplyr::left_join(obs, by = c("route", "rpid", "year")) %>%
    dplyr::mutate(count = tidyr::replace_na(.data$count, 0))

  # Filter observations
  if(!is.null(min_year)) obs <- dplyr::filter(obs, .data$year >= .env$min_year)
  if(!is.null(max_year)) obs <- dplyr::filter(obs, .data$year <= .env$max_year)

  routes <- obs %>%
    dplyr::group_by(.data$strata_name, .data$route) %>%
    dplyr::summarize(
      # First year each route was run
      first_year = min(.data$year),
      n = dplyr::n(),
      # At least 1 counted
      n_obs = length(.data$count[.data$count > 0]),
      .groups = "drop")

  routes_ever <- routes %>%
    dplyr::filter(.data$n_obs > 0)

  routes_never <- routes %>%
    dplyr::filter(.data$n_obs == 0)

  if(nrow(routes_ever) > 0) {

    routes_by_strata <- routes %>%
      dplyr::group_by(.data$strata_name) %>%
      dplyr::summarize(n_routes = dplyr::n(),
                       n_routes_ever = sum(.data$n_obs > 0),
                       n_routes_never = sum(.data$n_obs == 0),
                       p_routes_ever = .data$n_routes_ever / .data$n_routes,
                       p_routes_never = .data$n_routes_never / .data$n_routes,
                       first_year = min(.data$first_year),
                       max_n_routes_year = max(.data$n_obs),
                       n_obs = sum(.data$n_obs),
                       mean_obs = .data$n_obs / .data$n_routes_ever,
                       .groups = "drop") %>%
      dplyr::mutate(
        first_year = dplyr::if_else(.data$first_year > 2100,
                                    NA_integer_,
                                    as.integer(.data$first_year)),
        max_n_routes_year = dplyr::if_else(.data$max_n_routes_year < 0,
                                 NA_integer_,
                                 as.integer(.data$max_n_routes_year))) %>%
      dplyr::filter(.data$n_routes_ever >= .env$min_n_routes,
                    .data$max_n_routes_year >= .env$min_max_route_years,
                    .data$mean_obs >= .env$min_mean_route_years)

    if(nrow(routes_by_strata) == 0) {
      stop("Not enough routes where this species was counted.\nConsider ",
           "adjusting `min_n_routes`, `min_max_route_years`, and/or ",
           "`min_mean_route_years`\n", call. = FALSE)
    }


    # Only keep obs where route ever had that species
    obs_ever <- dplyr::semi_join(obs, routes_ever, by = "route") %>%
      # Only keep obs where strata meets minimum requirements
      dplyr::inner_join(routes_by_strata, by = "strata_name") %>%
      # Create numeric years from start
      dplyr::mutate(year_num = .data$year - min(.data$year) + 1)

  } else {
    obs_ever <- dplyr::slice(obs, 0)
  }

  obs_final <- obs_ever %>%
    dplyr::mutate(
      # Create strata numbers here to get final set (rather than in stratify())
      strata = as.numeric(factor(.data$strata_name)),
      observer = as.integer(factor(.data$obs_n)),
      site = as.integer(factor(.data$route)),
      obs_route = paste0(.data$route, " - ", .data$obs_n),
      obs_site = as.integer(factor(.data$obs_route))) %>%
    dplyr::group_by(.data$strata_name) %>%
    dplyr::mutate(n_obs_sites = dplyr::n_distinct(.data$obs_site)) %>%
    dplyr::group_by(.data$obs_route) %>%
    dplyr::mutate(first_year = dplyr::if_else(
      .data$year == min(.data$year), 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$strata, .data$route, .data$year) %>%
    dplyr::rename("non_zero_weight" = "p_routes_ever")

  weights <- obs_final %>%
    dplyr::select("strata", "non_zero_weight") %>%
    dplyr::distinct() %>%
    dplyr::pull(.data$non_zero_weight)

  obs_by_site <- obs_final %>%
    dplyr::select("strata", "obs_site", "site", "observer") %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$observer)

  n_obs_sites <- obs_final %>%
    dplyr::select("strata", "n_obs_sites") %>%
    dplyr::distinct() %>%
    dplyr::pull(.data$n_obs_sites)

  n_strata <- length(unique(obs_final$strata))

  # Create matrices
  site_mat <- matrix(data = 0,
                     nrow = n_strata,
                     ncol = max(n_obs_sites))

  obs_mat <- matrix(data = 0,
                    nrow = n_strata,
                    ncol = max(n_obs_sites))

  for(i in 1:n_strata){
    site_mat[i,1:n_obs_sites[i]] <- obs_by_site$site[obs_by_site$strata == i]
    obs_mat[i,1:n_obs_sites[i]] <- obs_by_site$observer[obs_by_site$strata == i]
  }

  model_data <- list(
    # Sample sizes
    n_sites = max(obs_final$site),
    n_strata = length(unique(obs_final$strata)),
    n_counts = nrow(obs_final),
    n_years = as.integer(max(obs_final$year_num)),

    # Basic data
    count = obs_final$count,
    strat = obs_final$strata,  # strat = Stan models, otherwise strata
    year = obs_final$year_num, # year = Stan models, otherwise year_num
    site = obs_final$site,

    # Observer information
    n_observers = max(obs_final$observer),
    observer = obs_final$observer,
    first_year = obs_final$first_year,

    # Ragged array information to link sites and observers to strata
    n_obs_sites_strata = n_obs_sites,
    max_n_obs_sites_strata = max(n_obs_sites),
    ste_mat = site_mat,
    obs_mat = obs_mat,

    # Weights
    non_zero_weight = weights
  )

  # Extra
  meta_strata <- dplyr::select(obs_final, "strata_name", "strata") %>%
    dplyr::distinct() %>%
    dplyr::left_join(strata_data$meta_strata, by = "strata_name")

  raw_data <- dplyr::select(obs_final, -dplyr::matches("^(n|p)_routes_"))

  list("model_data" = model_data,
       "meta_data" = strata_data$meta_data,
       "meta_strata" = meta_strata,
       "raw_data" = raw_data)
}
