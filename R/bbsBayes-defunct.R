#' bbsBayes defunct functions
#'
#' @param ... Original function arguments
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
#' in the output of `prepare_data()`. e.g., `prepared_data[["raw_data"]]`
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
#'  `bbs_strata`. e.g, `bbs_strata[["bbs_usgs"]]`.
#'
#' `p_waic()` and `waic()` are no longer recommended for BBS data. Use
#'  cross validation instead.
#'
#'  `extract_index_data()` is no longer recommended.
#'
#'
#' @seealso [bbsBayes-deprecated]
#'
NULL

#' Error/Warn on deprecated function or argument
#'
#' @param when Character. Version of bbsBayes when `what` was made defunct.
#' @param what Character. Argument name or `NULL` for containing function.
#' @param replace Character. What the user should use instead. Will be wrapped
#'   in "Use REPLACE instead." So supply backticks as necessary.
#'
#' `dep_stop()` is for creating an error (i.e. user must fix the problem)
#' `dep_warn()` is for creating a warning (i.e. function will continue)
#'
#' @noRd

dep_stop <- function(when, what = NULL, replace = NULL) {
  msg <- dep(when, what, replace, type = "defunct")
  stop(msg, call. = FALSE)
}

dep_warn <- function(when, what = NULL, replace = NULL) {
  msg <- dep(when, what, replace, type = "deprecated")
  warning(msg, call. = FALSE)
}

dep <- function(when, what, replace, type) {
  fun <- as.character(sys.call(which = sys.parent(n = 2))[1])

  if(!is.null(what)) {
    msg <- paste0("The `", what, "` argument for ")
  } else msg <- ""

  msg <- paste0(msg, "`", fun, "()` is ", type, " as of bbsBayes ", when)

  if(!is.null(replace)) msg <- paste0(msg, "\nUse ", replace, " instead.")

  msg
}


# generate_indices() ------------------------------------------------------

#' @rdname bbsBayes-defunct
generate_cont_indices <- function(...) {
  dep_stop("2.2.1", replace = "`generate_indices(regions = \"continental\")`")
}

#' @rdname bbsBayes-defunct
generate_regional_indices <- function(...) {
  dep_stop("2.2.1", replace = "`generate_indices()`")
}

#' @rdname bbsBayes-defunct
generate_strata_indices <- function(...) {
  dep_stop("2.2.1", replace = "`generate_indices(region = \"stratum\")`")
}



# generate_trends() -------------------------------------------------------

#' @rdname bbsBayes-defunct
generate_cont_trend <- function(...) {
  dep_stop("2.2.1", replace = "`generate_trends()`")
}

#' @rdname bbsBayes-defunct
generate_regional_trends <- function(...) {
  dep_stop("2.2.1", replace = "`generate_trends()`")
}

#' @rdname bbsBayes-defunct
generate_strata_trends <- function(...) {
  dep_stop("2.2.1", replace = "`generate_trends()`")
}

# mcmc stuff --------------------------------------------------------------

#' @rdname bbsBayes-defunct
get_final_values <- function(...) {
  dep_stop("3.0.0",
           replace = paste0("`cmdstanr::draws()` ",
                            "(e.g., model_output[[\"model_fit\"]]$draws()`"))
}

#' @rdname bbsBayes-defunct
get_mcmc_list <- function(...) {
  dep_stop("3.0.0", replace = "`model_output[[\"model_fit\"]]`")
}


# other -------------------------------------------------------------------

#' @rdname bbsBayes-defunct
extract_index_data <- function(...) {
  dep_stop("3.0.0")
}

#' @rdname bbsBayes-defunct
get_prepared_data <- function(...) {
  dep_stop("3.0.0", "get_prepared_data",
           "the `raw_data` list item in the output of `prepare_data()`")
}

#' @rdname bbsBayes-defunct
get_strata_area <- function(...) {
  dep_stop("3.0.0",
           replace = paste0("`bbs_strata` to access strata areas ",
                            "(e.g, `bbs_strata[[\"bbs_usgs\"]])"))
}

#' @rdname bbsBayes-defunct
get_composite_regions <- function(...) {
  dep_stop("3.0.0",
           replace = paste0("`bbs_strata` to access regional information ",
                            "(e.g, `bbs_strata[[\"bbs_usgs\"]])"))
}

#' @rdname bbsBayes-defunct
plot_cont_indices <- function(...) {
  dep_stop("2.2.1", replace = "`plot_indices()`")
}

#' @rdname bbsBayes-defunct
prepare_jags_data <- function(...) {
  dep_stop("2.3.7.2020", replace = "`prepare_data()`")
}

#' @rdname bbsBayes-defunct
p_waic <- function(...) {
  dep_stop("2.2.1", replace = "cross validation")
}

#' @rdname bbsBayes-defunct
waic <- function(...) {
  dep_stop("2.2.1", replace = "cross validation")
}

