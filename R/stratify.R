#' Stratify and filter Breeding Bird Survey data
#'
#' Assign count data to strata and filter by species of interest. Routes are
#' assigned to strata based on their geographic location and the stratification
#' specified by the user. Species are filtered by matching English, French or
#' Scientific names to those in the BBS species data (see `search_species()` for
#' a flexible search to identify correct species names).
#'
#'
#' @param by Character. Stratification type. Either an established type, one of
#'   "prov_state", "bcr", "latlong", "bbs_cws", "bbs_usgs", or a custom name
#'   (see `strata_custom` for details).
#' @param species Character. Bird species of interest. Can be specified by
#'   English, French, or Scientific names, or AOU code. Use `search_species()`
#'   for loose matching to find the exact name/code needed.
#' @param strata_custom (`sf`) Data Frame. Data frame
#'   of modified existing stratification, or a `sf` spatial data frame with
#'   polygons defining the custom stratifications. See Details.
#' @param combine_species_forms Logical. Whether to combine ambiguous species
#'   forms. Default `TRUE`. See Details.
#' @param sample_data Logical. Use sample data (just Pacific Wrens). Default
#'   `FALSE`.
#' @param return_omitted Logical. Whether or not to return a data frame of
#'   route-years which were omitted during stratification as they did not
#'   overlap with any stratum. For checking and troubleshooting. Default
#'   `FALSE`.
#' @param lump_species_forms Deprecated. Use `combine_species_forms` instead
#' @param bbs_data Defunct.
#'
#' @inheritParams common_docs
#'
#' @details
#'   To define a custom subset of an existing stratification, specify the
#'   stratification in `by` (e.g., "bbs_cws") and then supply a subset of
#'   `bbs_strata[["bbc_cws"]]` to `strata_custom` (see examples).
#'
#'   To define a completely new custom stratification, specify the name you
#'   would like use in `by` (e.g., "east_west_divide") and then supply a spatial
#'   data frame with polygons identifying the different strata to
#'   `strata_custom`. Note that this data must have a column called
#'   `strata_name` which names all the strata contained (see examples).
#'
#'   If `combine_species_forms` is `TRUE` (default), species with multiple forms
#'   (e.g., "unid. Dusky Grouse / Sooty Grouse") are included in overall species
#'   groupings (i.e., "unid." are combined with "Dusky Grouse" and "Sooty
#'   Grouse" into "Blue Grouse (Dusky/Sooty)"). If the user wishes to keep the
#'   forms separate, `combine_species_forms` can be set to `FALSE`. See the data
#'   frame `species_forms`, for which species are set to be combined with which
#'   other species.
#'
#'   See `vignette("stratification", package = "bbsBayes")` and the article
#'   [custom
#'   stratification](https://steffilazerte.ca/bbsBayes/articles/custom_stratification.html)
#'   for more details.
#'
#' @return List of (meta) data.
#'   - `meta_data` meta data defining the analysis
#'   - `meta_strata` contains a data frame listing strata names and area for all
#'      strata relevant to the data (i.e. some may have been removed due to lack
#'      of count data). Contains at least `strata_name` (the label of the
#'      stratum), and `area_sq_km` (area of the stratum).
#'   - `birds_strata` contains stratified count-level data filtered by species
#'   - `routes_strata` contains stratified route-level data filtered by species
#'
#' @examples
#'
#' # Sample Data - USGS BBS strata
#' s <- stratify(by = "bbs_usgs", sample_data = TRUE)
#'
#' # Full data - species and stratification
#' # Use `search_species()` to get correct species name
#'
#' # Stratify by CWS BBS strata
#' s <- stratify(by = "bbs_cws", species = "Common Loon")
#'
#' # Use use English, French, Scientific, or AOU codes for species names
#' s <- stratify(by = "bbs_cws", species = "Plongeon huard")
#' s <- stratify(by = "bbs_cws", species = 70)
#' s <- stratify(by = "bbs_cws", species = "Gavia immer")
#'
#' # Stratify by Bird Conservation Regions
#' s <- stratify(by = "bcr", species = "Great Horned Owl")
#'
#' # Stratify by CWS BBS strata
#' s <- stratify(by = "bbs_cws", species = "Canada Jay")
#'
#' # Stratify by State/Province/Territory only
#' s <- stratify(by = "prov_state", species = "Common Loon")
#' s <- stratify(by = "prov_state", species = "Plongeon huard")
#' s <- stratify(by = "prov_state", species = 70)
#'
#'
#' # Stratify by blocks of 1 degree of latitude X 1 degree of longitude
#' s <- stratify(by = "latlong", species = "Snowy Owl")
#'
#' # Check routes omitted by stratification
#' s <- stratify(by = "latlong", species = "Snowy Owl", return_omitted = TRUE)
#' s[["routes_omitted"]]
#'
#' # Use combined or non-combined species forms
#'
#' search_species("Sooty grouse")
#' s <- stratify(by = "bbs_usgs", species = "Blue Grouse (Dusky/Sooty)")
#' nrow(s$birds_strata) # Contains all Dusky, Sooty and unidentified
#'
#' search_species("Sooty grouse", combine_species_forms = FALSE)
#' s <- stratify(by = "bbs_usgs", species = "unid. Dusky Grouse / Sooty Grouse",
#'               combine_species_forms = FALSE)
#' nrow(s$birds_strata) # Contains *only* unidentified
#'
#'
#' # Stratify by a subset of an existing stratification
#' library(dplyr)
#' my_cws <- filter(bbs_strata[["bbs_cws"]], country_code == "CA")
#' s <- stratify(by = "bbs_cws", strata_custom = my_cws, species = "Snowy Owl")
#'
#' my_bcr <- filter(bbs_strata[["bcr"]], strata_name == "BCR8")
#' s <- stratify(by = "bcr", strata_custom = my_bcr,
#'               species = "Yellow-rumped Warbler (all forms)")
#'
#' # Stratify by Custom stratification, using sf map object
#' # e.g. with WBPHS stratum boundaries 2019
#' # available: https://ecos.fws.gov/ServCat/Reference/Profile/142628
#'
#' \dontrun{
#' map <- sf::read_sf("../WBPHS_Stratum_Boundaries_2019") %>%
#'   rename(strata_name = STRAT) # stratify expects this column
#'
#' s <- stratify(by = "WBPHS_2019", strata_map = map)
#'}
#'
#'
#'
#' @export

stratify <- function(by,
                     species,
                     strata_custom = NULL,
                     combine_species_forms = TRUE,
                     release = 2022,
                     sample_data = FALSE,
                     return_omitted = FALSE,
                     quiet = FALSE,
                     lump_species_forms, bbs_data) {

  # Deprecated/Defunct args
  if(!missing(lump_species_forms)) {
    dep_warn("3.0.0", "lump_species_forms", "`combine_species_forms`")
    combine_species_forms <- lump_species_forms
  }
  if(!missing(bbs_data)) dep_stop("3.0.0", "bbs_data")

  # Checks
  by <- check_strata(by, custom = strata_custom, quiet = quiet)
  stratify_by <- by[1]
  stratify_type <- by[2]
  if(!inherits(strata_custom, "data.frame")) check_sf(strata_custom, col = TRUE)
  check_logical(combine_species_forms, sample_data, quiet)
  check_release(release)

  # Load BBS Data (full or sample)
  bbs_data <- load_bbs_data(release = release, sample = sample_data,
                            quiet = quiet)

  # Load and filter bbs data
  species_list <- bbs_data$species
  birds <- bbs_data$birds
  routes <- bbs_data$routes

  rm(bbs_data) # Save memory

  # Check and filter species
  if(!sample_data) {
    sp_aou <- check_species(species, species_list, combine_species_forms, quiet)
    birds <- dplyr::filter(birds, .data$aou == .env$sp_aou)
    if(!combine_species_forms) birds <- dplyr::filter(birds,
                                                      !.data$unid_combined)
  } else {
    if(!quiet) message("Using species Pacific Wren (sample data)")
    species <- "Pacific Wren"
  }

  if(!quiet) message("Stratifying data...")

  # Create temporary `rid` (because faster for birds to join)
  routes <- dplyr::mutate(
    routes,
    rid = paste(.data$country_num, .data$state_num, .data$route, sep = "-"))

  birds <- dplyr::select(routes, "country_num", "state_num", "route", "rid") %>%
    dplyr::distinct() %>%
    dplyr::left_join(birds, ., by = c("country_num", "state_num", "route")) %>%
    dplyr::semi_join(routes, by = "rid")

  if (stratify_by == "bbs_cws") {

    if(!quiet) message("  Combining BCR 7 and NS and PEI...")

    # Combine all BCR 7
    bcr7 <- dplyr::filter(routes, .data$bcr == 7) %>%
      dplyr::mutate(st_abrev = "BCR7",
                    route = .data$route + (1000 * .data$state_num),
                    state_num = 777)

    # Combine NS and PEI
    ns_pei <- dplyr::filter(routes, .data$st_abrev %in% c("PE", "NS")) %>%
      dplyr::mutate(
        state = "Nova Scotia Prince Edward Island",
        # Keep PEI route numbers distinct from Nova Scotia routes
        route = dplyr::if_else(.data$st_abrev == "PE",
                               .data$route + (1000 * .data$route),
                               as.double(.data$route)),
        st_abrev = "NSPE",
        state_num = 765)

    # Add to routes
    # Note: This only works because the two data sets are mutually exclusive
    #       Otherwise bind together between sets.
    routes <- routes %>%
      dplyr::filter(.data$bcr != 7,                         # Omit BCR 7
                    !.data$st_abrev %in% c("PE", "NS")) %>% # Omit PE and NS
      dplyr::bind_rows(bcr7, ns_pei)                        # Add combo data

    # Fix birds
    birds <- dplyr::select(birds, -"route", -"state_num") %>%
      dplyr::left_join(dplyr::select(routes, "rid", "route", "state_num") %>%
                         dplyr::distinct(),
                       by = "rid")
  }


  if(stratify_type != "custom") {
    # Stratify by established groups
    routes <- dplyr::mutate(
      routes,
      strata_name = dplyr::case_when(
        stratify_by == "prov_state" ~ .data$st_abrev,
        stratify_by == "bcr" ~ paste0("BCR", .data$bcr),
        stratify_by == "latlong" ~ paste0(trunc(.data$latitude),
                                    "_",
                                    trunc(.data$longitude)),
        stratify_by %in% c("bbs_usgs", "bbs_cws") ~
          paste0(.data$country, "-", .data$st_abrev, "-", .data$bcr)))

    if(stratify_type == "subset") {
      meta_strata <- strata_custom
    } else {
      meta_strata <- bbsBayes::bbs_strata[[stratify_by]]
    }

    # Assing NA to all routes not in stratification (omitted below)
    keep <- routes$strata_name %in% meta_strata$strata_name
    routes$strata_name[!keep] <- NA_character_

  } else if (stratify_type == "custom") {
    # Custom stratification
    strata_custom <- stratify_map(strata_custom, routes, quiet)
    routes <- strata_custom[["routes"]]
    meta_strata <- strata_custom[["meta_strata"]]
  }

  # routes - create rt_uni and rt_uni_y value with newly defined combined states
  if(!quiet) message("  Renaming routes...")
  routes <- dplyr::mutate(routes,
                          route = paste0(.data$state_num, "-", .data$route))

  # Omit strata with no routes
  meta_strata <- dplyr::semi_join(meta_strata, routes, by = "strata_name")

  # Omit routes that do not fit in to strata
  n_na <- sum(is.na(routes$strata_name))

  if(!quiet & n_na > 0) {
    if(return_omitted) {
      msg <- "\n    Returning omitted routes."
    } else {
      msg <- paste0("\n    To see omitted routes use `return_omitted = TRUE` ",
                    "(see ?stratify)")
    }

    message("  Omitting ",
            format(n_na, big.mark = ","), "/",
            format(nrow(routes), big.mark = ","),
            " route-years that do not match a stratum.",
            msg)
  }

  # Filter omitted routes
  if(return_omitted) {
    routes_omitted <- dplyr::filter(routes, is.na(.data$strata_name)) %>%
      dplyr::select("year", "strata_name", "country", "state", "route",
                    "route_name", "latitude", "longitude", "bcr", "obs_n",
                    "total_spp")
  }
  routes <- dplyr::filter(routes, !is.na(.data$strata_name))

  if(nrow(routes) == 0) {
    stop("No routes within this stratification", call. = FALSE)
  }

  # birds - create rt_uni and rt_uni_y by index then join
  b_index <- dplyr::select(birds, "rid", "year", "state_num", "route") %>%
    dplyr::distinct() %>%
    dplyr::mutate(route = paste0(.data$state_num, "-", .data$route)) %>%
    dplyr::select("rid", "year", "route")

  birds <- dplyr::select(birds, -"route") %>%
    dplyr::left_join(b_index, by = c("rid", "year")) %>%
    dplyr::semi_join(routes, by = "rid")

  if(nrow(birds) == 0) {
    stop("No bird counts within this stratification", call. = FALSE)
  }

  # Return list
  out <- list("meta_data" = list("stratify_by" = stratify_by,
                                 "stratify_type" = stratify_type,
                                 "species" = species),
              "meta_strata" = meta_strata,
              "birds_strata" = dplyr::select(birds, -"rid"),
              "routes_strata" = dplyr::select(routes, -"rid"))

  if(return_omitted) {
    out <- append(out, list("routes_omitted" = routes_omitted))
  }

  out
}

stratify_map <- function(strata_map, routes, quiet = FALSE) {

  if(!quiet) {
    c <- sf::st_crs(strata_map, parameters = TRUE)[c("srid", "Name")]
    message("Preparing custom strata (", c$srid, "; ", c$Name, ")...")
  }

  # Keep strata name column only
  strata_map <- dplyr::select(strata_map, "strata_name") %>%
    dplyr::mutate(strata_name = as.character(.data$strata_name))

  n_features <- sf::st_drop_geometry(strata_map) %>%
    dplyr::pull(.data$strata_name) %>%
    dplyr::n_distinct()

  # Check if multiple data records per stratum, if so, summarize
  if(n_features != nrow(strata_map)) {
    if(!quiet) message("  Summarizing strata...")
    strata_map <- strata_map %>%
      dplyr::group_by(.data$strata_name) %>%
      dplyr::summarize()
  }

  if(!quiet) message("  Calculating area weights...")
  strata_map <- strata_map %>%
    dplyr::mutate(area_sq_km = sf::st_area(.),
                  area_sq_km = units::set_units(.data$area_sq_km, "km^2"),
                  area_sq_km = as.numeric(.data$area_sq_km))


  # Merge with map polygons and keep coordinates
  if(!quiet) message("  Joining routes to custom spatial data...")
  routes <- routes %>%
    dplyr::mutate(lon = .data$longitude, lat = .data$latitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_transform(sf::st_crs(strata_map)) %>%
    sf::st_join(strata_map) %>%
    sf::st_drop_geometry() %>%
    dplyr::rename("longitude" = "lon", "latitude" = "lat") %>%
    dplyr::select("strata_name", dplyr::all_of(names(.env$routes))) # reorder

  list("meta_strata" = sf::st_drop_geometry(strata_map),
       "routes" = routes)
}
