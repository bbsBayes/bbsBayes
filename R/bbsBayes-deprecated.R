#' bbsBayes deprecated functions
#'
#' @name bbsBayes-deprecated
#'
#' @description
#'
#' `plot_map()` supersedes `generate_map()`
#'
#' `plot_geofacet()` supersedes `geofacet_plot()`
#'
#' @seealso [bbsBayes-defunct]
NULL

#' @rdname bbsBayes-deprecated
generate_map <- function(...) {
  defunct("plot_map()", type = "deprecated")
  plot_map(...)
}

#' @rdname bbsBayes-deprecated
geofacet_plot <- function(..) {
  defunct("plot_map()", type = "deprecated")
  plot_geofacet(...)
}
