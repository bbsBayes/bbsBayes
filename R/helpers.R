#' Search for species
#'
#' A helper function for finding the appropriate species name for use in
#' `stratify()`.
#'
#' @param species Character/Numeric. Search term, either name in English or
#'   French, AOU code, or scientific genus or species. Matches by regular
#'   expression but ignores case.
#' @param combine_species_forms Logical. Whether or not to search the combined
#'   species data or the uncombined species. Note that this results in different
#'   species names.
#'
#' @return Data frame subset of BBS species data frame
#' @export
#'
#' @examples
#'
#' # Search for various terms
#' search_species("Paridae")
#' search_species("chickadee")
#' search_species("mésang")
#' search_species("Poecile")
#' search_species(7360)
#' search_species(73)
#' search_species("^73") # Use regex to match aou codes starting with 73
#' search_species("blue grouse")
#' search_species("sooty grouse")
#'
#' # To combine or not
#' search_species("blue grouse", combine_species_forms = FALSE)
#' search_species("sooty grouse", combine_species_forms = FALSE)
#' search_species("northern flicker")
#' search_species("northern flicker", combine_species_forms = FALSE)
#'
search_species <- function(species, combine_species_forms = TRUE) {

  if(!inherits(species, c("character", "numeric"))) {
    stop("`species` must be either a text string or number to match against ",
         "species names or AOU codes", call. = FALSE)
  }
  check_logical(combine_species_forms)
  sp_list <- load_bbs_data()$species %>%
    dplyr::filter(.data$unid_combined == .env$combine_species_forms)

  search <- stringr::str_split(species, " ", simplify = TRUE)

  dplyr::filter(sp_list,
                search_col(.data$english, search) |
                  search_col(.data$french, search) |
                  search_col(.data$aou, search) |
                  search_col(.data$family, search) |
                  search_col(.data$spanish, search) # Scientific
  ) %>%
    dplyr::select(-"seq")
}

search_col <- function(column, search) {
  s <- rep(TRUE, length(column))
  for(i in search) {
    s <- s & stringr::str_detect(column, stringr::regex(i, ignore_case = TRUE))
  }
  s
}


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

#' Categorize polygon by Province/State if applicable
#'
#' @param strata_map sf data frame. Strata polygons to be categorized.
#' @param min_overlap Numeric. The minimum proportion of overlap between a
#'   stratum polygon and a Province or State. Below this will raise warnings.
#' @param plot Logical. Whether to plot how polygons were assigned to Provinces
#'   or States
#' @param keep_spatial Logical. Whether the output should be a spatial data
#'   frame or not.
#'
#' @return (Spatial) data frame with strata assigned to Province/State
#'
#' @export
#'
#' @examples
#'
#' # Demonstration of why we can't divide BCR by Provinces and States!
#' map <- load_map("bcr")
#' assign_prov_state(map, plot = TRUE)
#'
#' # Use custom stratification, using sf map object
#' # e.g. with WBPHS stratum boundaries 2019
#' # available: https://ecos.fws.gov/ServCat/Reference/Profile/142628
#'
#' \dontrun{
#' map <- sf::read_sf("../WBPHS_Stratum_Boundaries_2019") %>%
#'   rename(strata_name = STRAT) # expects this column
#' s <- assign_prov_state(map, plot = TRUE)
#' # Some don't divide nicely, we could try a different min_overlap
#'
#' s <- assign_prov_state(map, min_overlap = 0.6, plot = TRUE)
#'}
#'
#'
assign_prov_state <- function(strata_map, min_overlap = 0.75, plot = FALSE,
                              keep_spatial = TRUE) {

  # Checks
  check_sf(strata_map)
  check_numeric(min_overlap)
  check_logical(plot, keep_spatial)

  if(min_overlap <= 0.5 | min_overlap > 1) {
    stop("`min_overlap` must be betwen 0.5 and 1.0 (50-100%)", call. = FALSE)
  }


  ps <- format_ne_states() %>%
    dplyr::select("prov_state", "country", "country_code", "province_state") %>%
    sf::st_transform(3347) %>%
    sf::st_set_agr("constant")

  ovlps <- strata_map %>%
    sf::st_transform(sf::st_crs(ps)) %>%
    dplyr::group_by(.data$strata_name) %>%
    dplyr::summarize() %>%
    sf::st_set_agr("constant") %>%
    sf::st_make_valid() %>%
    sf::st_intersection(ps) %>%
    dplyr::mutate(area = sf::st_area(.))

  ps_assigned <- ovlps %>%
    dplyr::group_by(.data$strata_name) %>%
    dplyr::mutate(p_area = as.numeric(.data$area / sum(.data$area))) %>%
    dplyr::mutate(note = dplyr::case_when(
      any(.data$p_area > .env$min_overlap) &
        .data$p_area == max(.data$p_area) ~ "good",
      !any(.data$p_area > .env$min_overlap) ~ "warn",
      TRUE ~ "ignore")) %>%
    dplyr::filter(.data$note %in% c("good", "warn")) %>%
    dplyr::mutate(
      prov_state = .data$prov_state[.data$p_area == max(.data$p_area)]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"area")

  if(any(ps_assigned$note == "warn")) {
    warning(
      dplyr::n_distinct(ps_assigned$strata_name[ps_assigned$note == "warn"]),
      " strata are assigned to a province or state based on less than the ",
      "minimum specified overlap",
      call. = FALSE)
  }

  if(plot) {
    ps <- sf::st_transform(ps, crs = 3347)
    ps_assigned <- ps_assigned %>%
      dplyr::mutate(strata_name = as.character(.data$strata_name))

    g <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = ps_assigned,
                       ggplot2::aes(fill = .data$prov_state),
                       colour = "grey40", alpha = 0.4) +
      ggplot2::geom_sf(data = ps, colour = "black", fill = NA) +
      ggplot2::scale_fill_viridis_d(end = 0.9) +
      ggplot2::labs(fill = "Strata assigned to Province/State",
                    colour = "Less than min_overlap with Province/State")
    if(any(ps_assigned$note == "warn")) {
      g <- g +
        ggplot2::geom_sf(
          data = dplyr::filter(ps_assigned, .data$note == "warn"),
          ggplot2::aes(colour = .data$strata_name, fill = NA), size = 1.3)
    }
    print(g)
  }

  if(keep_spatial) r <- sf::st_transform(ps_assigned, sf::st_crs(strata_map))
  if(!keep_spatial) r <- sf::st_drop_geometry(ps_assigned)
  r
}

