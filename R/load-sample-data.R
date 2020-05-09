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
load_sample_data <- function()
{
  return(list(bird = bbsBayes::bird_sample,
              route = bbsBayes::route_sample,
              species = bbsBayes::species_sample))
}

