#' bbsBayes defunct functions
#'
#' @name bbsBayes-defunct
#'
#' @description
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
#' `plot_indices()` supersedes
#'  - `plot_cont_indices()`
#'
#'  `prepare_data()` supersedes
#'  - `prepare_jags_data()`
#'
#'  `p_waic()` and `waic()` are no longer recommended for BBS data. Use
#'  cross validation instead
#'
NULL


defunct <- function(usage) {
  f <- as.character(sys.call(which = sys.parent(n = 1)))
  stop("`", f, "()` is now defunct.\nUse `", usage, "` instead.", call. = FALSE)
}


# generate_indices() ------------------------------------------------------

#' @rdname bbsBayes-defunct
generate_cont_indices <- function() {
  defunct("generate_indices(regions = \"continental\")")
}

#' @rdname bbsBayes-defunct
generate_regional_indices <- function() {
  defunct("generate_indices()")
}

#' @rdname bbsBayes-defunct
generate_strata_indices <- function() {
  defunct("generate_indices(region = \"stratum\")")
}



# generate_trends() -------------------------------------------------------

#' @rdname bbsBayes-defunct
generate_cont_trend <- function() {
  defunct("generate_trends()")
}

#' @rdname bbsBayes-defunct
generate_regional_trends <- function() {
  defunct("generate_trends()")
}

#' @rdname bbsBayes-defunct
generate_strata_trends <- function() {
  defunct("generate_trends()")
}


# other -------------------------------------------------------------------

#' @rdname bbsBayes-defunct
plot_cont_indices <- function() {
  defunct("plot_indices()")
}

#' @rdname bbsBayes-defunct
prepare_jags_data <- function() {
  defunct("prepare_data()")
}

#' @rdname bbsBayes-defunct
p_waic <- function() {
  defunct("cross validation")
}

#' @rdname bbsBayes-defunct
waic <- function() {
  defunct("cross validation")
}

