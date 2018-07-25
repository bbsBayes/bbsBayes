#' Get the area of each strata
#'
#' \code{get_strata_area} allows you to obtain the area of each strata
#'   for a given stratification type.
#'
#' @param strata_type Stratification type to return the areas of
#'
#' @return Data frame with the following variables:
#'   \item{region}{Name of the stratum/region}
#'   \item{area_sq_km}{Area of the stratum/region in square kilometres}
#'
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Obtain the strata area for each of the 5 stratification types
#'
#' # USGS BBS
#' st_area <- get_strata_area(strata_type = "bbs_usgs")
#'
#' # CWS BBS
#' st_area <- get_strata_area(strata_type = "bbs_cws")
#'
#' # BCR
#' st_area <- get_strata_area(strata_type = "bcr")
#'
#' # State/Province/Territory
#' st_area <- get_strata_area(strata_type = "state")
#'
#' # Degree block
#' st_area <- get_strata_area(strata_type = "latlong")
#' }
#'

get_strata_area <- function(strata_type = NULL)
{
  if (strata_type %in% c("bbs_usgs",
                         "bbs_cws",
                         "bcr",
                         "state",
                         "latlong"))
  {
    return(read.csv(system.file("area-weight", strata[[strata_type]], package = "bbsBayes")))
  }else
  {
    stop("Not a valid stratification type.")
  }
}
