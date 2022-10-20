#' Load Sample Breeding Bird Survey dataset into R Session
#'
#' \code{load_sample_data} returns the sample data provided by bbsBayes. The data
#' is returned as a list of data frames, similar to what is returned by \code{load_bbs_data}
#'
#' @return Large list (3 elements) consisting of:
#' \item{bird}{Data frame of sample bird point count data per route, per year}
#' \item{route}{Data frame of sample yearly route data}
#' \item{species}{Sample list of North American bird species}
#'
#' @examples
#'
#' sample_data <- load_sample_data()
#'
#'
#' @export
#'
load_sample_data_orig <- function() {
  list(bird = bird_sample,
       route = route_sample,
       species = species_sample)
}

load_sample_data <- function() {
  list(birds = birds_sample,
       routes = routes_sample,
       species = species_sample)
}

