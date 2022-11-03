ext <- function(file) {
 stringr::str_extract(file, "(?<=\\.)[[:alnum:]]+$")
}

load_internal_file <- function(name, stratify_by = NULL) {
  system.file(name, paste0(stratify_by, ".csv"), package = "bbsBayes") %>%
    readr::read_csv(show_col_types = FALSE, progress = FALSE)
}

get_geo_types <- function(strata_map) {
  sf::st_geometry_type(strata_map) %>%
    stringr::str_remove("MULTI") %>%
    unique()
}


format_ne_states <- function() {
  check_rnaturalearth()

  rnaturalearth::ne_states(
    country = c("United States of America", "Canada"), returnclass = "sf") %>%
    select("province_state" = "name", "code_hasc", "country" = "admin") %>%
    tidyr::separate(.data$code_hasc, sep = "\\.",
                    into = c("country_code", "prov_state")) %>%
    dplyr::mutate(prov_state = if_else(
      .data$prov_state == "NF", "NL", .data$prov_state))
}

#' Categorize polygon by Province/State if applicable
#'
#' @param min_overlap Numeric. The minimum proportion of overlap between a
#'   stratum polygon and a Province or State. Below this will raise warnings.
#' @param plot Logical. Whether to plot how polygons were assigned to Provinces
#'   or States
#' @keep_spatial Logical. Whether the output should be a spatial data frame or
#'   not.
#'
#' @inheritParams
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
assign_prov_state <- function(sf, min_overlap = 0.75, plot = FALSE,
                              keep_spatial = TRUE) {

  # Checks
  if(min_overlap <= 0.5) {
    stop("`min_overlap` must be greater than 0.5 (50%)", call. = FALSE)
  }
  check_sf(sf)

  ps <- format_ne_states() %>%
    dplyr::select("prov_state", "country", "country_code", "province_state") %>%
    sf::st_transform(3347) %>%
    sf::st_set_agr("constant")

  ovlps <- sf %>%
    sf::st_transform(st_crs(ps)) %>%
    dplyr::group_by(.data$strata_name) %>%
    dplyr::summarize() %>%
    sf::st_set_agr("constant") %>%
    sf::st_intersection(ps) %>%
    dplyr::mutate(area = sf::st_area(.))

  ps_assigned <- ovlps %>%
    dplyr::group_by(.data$strata_name) %>%
    dplyr::mutate(p_area = as.numeric(.data$area / sum(.data$area))) %>%
    dplyr::mutate(note = dplyr::case_when(
      any(.data$p_area > .env$min_overlap) & .data$p_area == max(.data$p_area) ~ "good",
      !any(.data$p_area > .env$min_overlap) ~ "warn",
      TRUE ~ "ignore")) %>%
    dplyr::filter(.data$note %in% c("good", "warn")) %>%
    dplyr::mutate(prov_state = .data$prov_state[.data$p_area == max(.data$p_area)]) %>%
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
      ggplot2::geom_sf(data = ps, colour = "black") +
      ggplot2::geom_sf(data = ps_assigned, ggplot2::aes(fill = .data$prov_state),
                       colour = NA, alpha = 0.4) +
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

  if(keep_spatial) r <- sf::st_transform(ps_assigned, sf::st_crs(sf))
  if(!keep_spatial) r <- sf::st_drop_geometry(ps_assigned)
  r
}

#' Search for species
#'
#' A helper function for finding the appropriate species name for use in
#' `stratify()`.
#'
#' @param species Character. Search term, either name in English or French, AOU
#'   code, or scientific genus or species.
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
  sp_list <- load_bbs_data()$species %>%
    dplyr::filter(unid_combined == .env$combine_species_forms)

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
