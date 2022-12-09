#' bbsBayes deprecated functions
#'
#' @param ... Original function arguments
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
  dep_warn("3.0.0", replace = "`plot_map()`")
  plot_map(...)
}

#' @rdname bbsBayes-deprecated
geofacet_plot <- function(...) {
  dep_warn("3.0.0", replace = "plot_geofacet()")
  plot_geofacet(...)
}
