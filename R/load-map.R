#' Load a map of of the geographic strata.
#'
#' \code{load_map} allows you to load a simple features object (\code{sf} package)
#'   that represents a map of the strata that can be easily plotted.
#'
#' @param stratify_by How were the data stratified?
#'
#' @return sf polygon object
#'
#' @importFrom sf read_sf
#'
#' @examples
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#' strat_map <- load_map(stratify_by = "bbs_cws")
#'
#' # simple plot of the map
#' plot(strat_map)
#'
#'
#' @export
#'

load_map <- function(stratify_by = NULL)
{

  if (is.null(stratify_by))
  {
    stop("No stratification specified."); return(NULL)
  }

  if(isFALSE(is.element(stratify_by, c("state", "bcr", "latlong", "bbs_cws", "bbs_usgs"))))
  {
    stop("Invalid stratification specified, choose one of state, bcr, latlong, bbs_cws, or bbs_usgs"); return(NULL)
  }



  map <- sf::read_sf(dsn = system.file("maps",
                                       package = "bbsBayes"),
                     layer = maps[[stratify_by]],
                     quiet = TRUE)



  return(map)
}
