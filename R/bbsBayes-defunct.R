#' bbsBayes defunct functions
#'
#' @name bbsBayes-defunct
#'
#' @description
#'
#' ## Superseded
#'
#' `generate_indices()` supersedes
#' - `generate_cont_indices()`
#' - `generate_regional_indices()`
#' - `generate_strata_indices()`
#'
#' `generate_trends()` supersedes
#' - `generate_cont_trend()`
#' - `generate_regional_trends()`
#' - `generate_strata_trends()`
#'
#' `plot_indices()` supersedes `plot_cont_indices()`
#'
#' `prepare_data()` supersedes `prepare_jags_data()`
#'
#' ## No longer relevant
#'
#' `get_prepared_data()` is no longer required. Access the `raw_data` list item
#' in the output of `prepare_data()`. e.g., `prepped_data[["raw_data"]]`
#'
#' `get_mcmc_list()` is no longer required (or relevant). Access the `model_fit`
#' list item in the output of `run_model()` for the `cmdstanr` model object.
#' e.g., `model_output[["model_fit"]]`
#'
#' `get_final_values()` is no longer required. Use `cmdstanr::draws()` to
#' extract final samples from the `model_fit` list item in the output of
#' `run_model()`. e.g., `model_output[["model_fit"]]$draws()`
#'
#'  `get_strata_area()` and `get_composite_regions()` are no longer required.
#'  Access the strata areas and regional data by exploring the included data
#'  `bbs_strata`. e.g, `bbs_strata[["bbs_usgs"]].
#'
#' `p_waic()` and `waic()` are no longer recommended for BBS data. Use
#'  cross validation instead
#'
#'  `extract_index_data()` is no longer recommended.
#'
#'
#' @seealso [bbsBayes-deprecated]
#'
NULL


defunct <- function(usage, type = "defunct", dont_use = FALSE) {
  f <- as.character(sys.call(which = sys.parent(n = 1)))
  if(dont_use) stop("`", f, "()` is no longer recommended.", call. = FALSE)
  msg <- paste0("`", f, "()` is now ", type, ".\nUse ", usage, " instead.")
  if(type == "defunct") stop(msg, call. = FALSE)
  if(type == "deprecated") warning(msg, call. = FALSE)
}


# generate_indices() ------------------------------------------------------

#' @rdname bbsBayes-defunct
generate_cont_indices <- function() {
  defunct("`generate_indices(regions = \"continental\")`")
}

#' @rdname bbsBayes-defunct
generate_regional_indices <- function() {
  defunct("`generate_indices()`")
}

#' @rdname bbsBayes-defunct
generate_strata_indices <- function() {
  defunct("`generate_indices(region = \"stratum\")`")
}



# generate_trends() -------------------------------------------------------

#' @rdname bbsBayes-defunct
generate_cont_trend <- function() {
  defunct("`generate_trends()`")
}

#' @rdname bbsBayes-defunct
generate_regional_trends <- function() {
  defunct("`generate_trends()`")
}

#' @rdname bbsBayes-defunct
generate_strata_trends <- function() {
  defunct("`generate_trends()`")
}

# mcmc stuff --------------------------------------------------------------

#' @rdname bbsBayes-defunct
get_final_values <- function() {
  defunct("`cmdstanr::draws()` (e.g., model_output[[\"model_fit\"]]$draws()`")
}

#' @rdname bbsBayes-defunct
get_mcmc_list <- function() {
  defunct("`model_output[[\"model_fit\"]]`")
}


# other -------------------------------------------------------------------

#' @rdname bbsBayes-defunct
extract_index_data <- function() {
  defunct(dont_use = TRUE)
}

#' @rdname bbsBayes-defunct
get_prepared_data <- function() {
  defunct("the `raw_data` list item in the output of `prepare_data()`")
}

#' @rdname bbsBayes-defunct
get_strata_area <- function() {
  defunct("`bbs_strata` to access strata areas (e.g, `bbs_strata[[\"bbs_usgs\"]])")
}

#' @rdname bbsBayes-defunct
get_composite_regions <- function() {
  defunct("`bbs_strata` to access regional information (e.g, `bbs_strata[[\"bbs_usgs\"]])")
}

#' @rdname bbsBayes-defunct
plot_cont_indices <- function() {
  defunct("`plot_indices()`")
}

#' @rdname bbsBayes-defunct
prepare_jags_data <- function() {
  defunct("`prepare_data()`")
}

#' @rdname bbsBayes-defunct
p_waic <- function() {
  defunct("cross validation")
}

#' @rdname bbsBayes-defunct
waic <- function() {
  defunct("cross validation")
}

