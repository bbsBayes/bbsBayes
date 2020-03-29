#' Get the area of each strata
#'
#' \code{get_composite_regions} allows you to obtain the dataframe defining the original composite regions
#'   for a given stratification type.
#'
#' @param strata_type Stratification type to return the areas of
#'
#' @return Data frame with at least the following variables:
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
#' # Obtain the potential composite regions for each of the 5 stratification types
#' # Most useful if the user wishes to create an set of custom composite regions
#' #
#' # USGS BBS
#' st_comp_regions <- get_composite_regions(strata_type = "bbs_usgs")
#' # create new column "Great_Plains"
#' gpall <- rep("Outside",nrow(st_comp_regions))
#' gp <- which(st_comp_region$bcr %in% c(11,17,18,19))
#' gpall[gp] <- "Inside"
#' st_comp_region$Great_Plains <- gpall
#' # st_comp_region can now be used as the dataframe input to the argument alt_region_names
#' # in generate_regional_indices,
#' # with "Great_Plains" as the value for the argument region
#'
#'
#' # CWS BBS
#' st_comp_regions <- get_composite_regions(strata_type = "bbs_cws")
#'
#' # BCR
#' st_comp_regions <- get_composite_regions(strata_type = "bcr")
#'
#' # State/Province/Territory
#' st_comp_regions <- get_composite_regions(strata_type = "state")
#'
#' # Degree block
#' st_comp_regions <- get_composite_regions(strata_type = "latlong")
#' }
#'

get_composite_regions <- function(strata_type = NULL)
{
  if (strata_type %in% c("bbs_usgs",
                         "bbs_cws",
                         "bcr",
                         "state",
                         "latlong"))
  {
    return(utils::read.csv(system.file("composite-regions", strata[[strata_type]], package = "bbsBayes")))
  }else
  {
    stop("Not a valid stratification type.")
  }
}
