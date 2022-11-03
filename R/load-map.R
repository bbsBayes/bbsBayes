#' Load a map of of the geographic strata.
#'
#' `load_map` allows you to load a simple features object (`sf` package)
#'   that represents a map of the strata that can be easily plotted.
#'
#' @param stratify_by Character. Stratification type. One of
#'   "prov_state", "bcr", "latlong", "bbs_cws", "bbs_usgs".
#' @param type Character. "strata" or political map ("North America", "Canada"
#'   or "US"/"USA"/"United States of America").
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

load_map <- function(stratify_by = NULL, type = "strata") {

  if(type == "strata") {
    stratify_by <- check_strata(stratify_by, simple = TRUE)

    f <- system.file("maps", package = "bbsBayes") %>%
      list.files(pattern = paste0(stratify_by, "_strata"), full.names = TRUE)

      map <- sf::read_sf(dsn = f, quiet = TRUE)
  } else {
    check_rnaturalearth()
    type <- tolower(type)

    country <- dplyr::case_when(
      type == "north america" ~ c("Canada", "United States of America"),
      type == "canada" ~ "Canada",
      type %in% c("us", "usa", "united states of america") ~
        "United States of America")

    map <- rnaturalearth::ne_countries(country = country, returnclass = "sf")
  }
  map
}
