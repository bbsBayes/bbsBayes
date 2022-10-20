#' Load a map of of the geographic strata.
#'
#' `load_map` allows you to load a simple features object (`sf` package)
#'   that represents a map of the strata that can be easily plotted.
#'
#' @param stratify_by How were the data stratified?
#'
#' @return sf polygon object
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

load_map <- function(stratify_by) {

  check_stratification(stratify_by, name = "stratification (`stratify_by`)")

  sf::read_sf(dsn = system.file("maps", package = "bbsBayes"),
              layer = maps[[stratify_by]],
              quiet = TRUE)
}
