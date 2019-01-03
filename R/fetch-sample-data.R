#' Fetch Sample Breeding Bird Survey dataset
#'
#' \code{fetch_sample_data} returns the sample data provided by bbsBayes. The data
#' is returned as a list of data frames, similar to what is returned by \code{fetch_bbs_data}
#'
#' @return Large list (3 elements) consisting of:
#' \item{bird}{Data frame of sample bird point count data per route, per year}
#' \item{route}{Data frame of sample yearly route data}
#' \item{species}{Sample list of North American bird species}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Be sure to assign the output of fetch_sample_data() to a variable.
#' # This function returns a lot of data, and if the output is not
#' # captured in a variable, R attempts to dump it into the console
#' # which tends to get highly bogged down.
#'
#' sample_data <- fetch_sample_data()
#'
#' }
#'
fetch_sample_data <- function()
{
  return(list(bird = bbsBayes::bird_sample,
              route = bbsBayes::route_sample,
              species = bbsBayes::species_sample))
}

