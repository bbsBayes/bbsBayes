#' Stratify raw Breeding Bird Survey data
#'
#' Assigns each bird count data point and each route a strata
#'   based on its geographic location and the stratification
#'   as specified by the user.
#'
#' @param by String argument of stratification type.
#'   Options are "state", "bcr", "latlong", "bbs_cws", "bbs_usgs"
#' @param sample_data Should just sample data (just Pacific Wren) be used?
#'   Defaults to FALSE.
#' @param quiet Should progress bars be suppressed?
#' @param bbs_data Raw BBS data saved as a list of 3 data frames.
#'   Not necessary if you have already run \code{fetch_bbs_data}
#' @param lump_species_forms Logical, default is TRUE, indicating that for
#'   species with multiple forms, the "unidentified" form is replaced by
#'   the sum of observations for all forms (including the original unidentified obs).
#'   The underlying BBS database includes separate data for each form,
#'   and these separate forms are retained with their original names.
#'   The original unidentified category for observations that were not specific to
#'   a particular form are replaced by the combined observations. If the user
#'   wishes to keep the unidentified form separate, this can be set to FALSE
#' @param stratify_by Deprecated in favour of 'by'
#'
#' @return Large list (3 elements) consisting of:
#'   \item{bird_strat}{Dataframe of stratified bird data}
#'   \item{route_strat}{Dataframe of stratified route data}
#'   \item{species_strat}{Dataframe of stratified species data}
#'   \item{by}{Argument used for stratification}
#'
#' @examples
#'
#' # Toy examples using Pacific Wren sample data
#'
#' # Stratify by CWS USGS stratifications
#' data_strat <- stratify(by = "bbs_usgs", sample_data = TRUE)
#'
#' # Stratify by Bird Conservation Regions only
#' data_strat <- stratify(by = "bcr", sample_data = TRUE)
#'
#' # Stratify by CWS BBS stratifications
#' data_strat <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Stratify by State/Province/Territory only
#' data_strat <- stratify(by = "state", sample_data = TRUE)
#'
#' # Stratify by blocks of 1 degree of latitude X 1 degree of longitude
#' data_strat <- stratify(by = "latlong", sample_data = TRUE)
#'

#' # To stratify the entire dataset, simply set the sample_data = FALSE,
#' # or drop that argument altogether. The function then requires fetch_bbs_data()
#' # to have been run (takes about 10 minutes). Stratification of the entire dataset
#' # may take up to 3 minutes.
#'
#' @noRd

stratify_orig <- function(by = NULL,
                     sample_data = FALSE,
                     bbs_data = NULL,
                     lump_species_forms = TRUE,
                     quiet = FALSE,
                     stratify_by = NULL)
{
  if (isFALSE(is.null(stratify_by)))
  {
    message("Argument \"stratify_by\" has been deprecated in favour of \"by\"")
    by <- stratify_by
  }

  if(isFALSE(is.element(by, c("state", "bcr", "latlong", "bbs_cws", "bbs_usgs"))))
  {
    stop("Invalid stratification specified, choose one of state, bcr, latlong, bbs_cws, or bbs_usgs"); return(NULL)
  }

  bbs_dir <- app_dir(appname = "bbsBayes")

  if (isTRUE(sample_data))
  {
    bbs_data <- load_sample_data()
  } else if (is.null(bbs_data))
  {
    if(isFALSE(file.exists(paste0(bbs_dir$data(), "/bbs_raw_data.RData"))))
    {
      stop("No BBS data downloaded. Please use fetch_bbs_data() first.")
    }
  }

  # Read lump table prior to analysis to determine progress bar length
  lump_sp <- utils::read.csv(system.file("species-lump-split",
                                         "lump.csv",
                                         package = "bbsBayes"),
                             fileEncoding = get_encoding(),
                             stringsAsFactors = FALSE)
  pb_len <- 8
  if (isTRUE(lump_species_forms))
  {
    pb_len <- pb_len + nrow(lump_sp)
  }
  if (!isTRUE(quiet))
  {
    message("Stratifying data")
    pb <- progress::progress_bar$new(
      format = "\r[:bar] :percent eta: :eta",
      clear = FALSE,
      total = pb_len,
      width = 80)
    pb$tick(0)
  }

  if (isFALSE(sample_data) & is.null(bbs_data))
  {
    load(file = paste0(bbs_dir$data(), "/bbs_raw_data.RData"))
  }
  if (!isTRUE(quiet)){pb$tick()}
  species <- bbs_data$species

  bird <- bbs_data$bird

  if (isFALSE(sample_data) & isTRUE(lump_species_forms)){
    rem1 <- NULL
    rem <- NULL
    tmp1 <- NULL
    tmp2 <- NULL
    tmp <- NULL

  for(lumpi in 1:nrow(lump_sp)){
    aou1 <- lump_sp[lumpi,"aou_original"]
    sp_en <- lump_sp[lumpi,"english_out"]
    sp_fr <- lump_sp[lumpi,"french_out"]
    wsp <- which(species$sp.bbs == aou1)
    species[wsp,"english"] <- sp_en
    species[wsp,"french"] <- sp_fr

    # replace original unidentified data with combined data from all forms
    nadd <- lump_sp[lumpi,"n_add"]
    aoulump <- lump_sp[lumpi,paste0("aou",1:nadd)]
    tmp1 <- bird[which(bird$AOU %in% aou1),]
    tmp2 <- bird[which(bird$AOU %in% aoulump),]
    if(nrow(tmp1) > 0){
      rem1 <- which(bird$AOU %in% aou1)
    #bird <- bird[-which(bird$AOU %in% aou1),]
    tmp <- rbind(tmp1,tmp2)
    }else{
      tmp <- tmp2
    }
    if(nrow(tmp) > 0){
     tmp$AOU <- aou1
    # bird <- rbind(bird,tmp)
    }
    if(lumpi == 1){
      tmp_add <- tmp
      rem <- rem1
    }else{
      tmp_add <- rbind(tmp_add,tmp)
      rem <- c(rem,rem1)
    }

    if (!isTRUE(quiet)){pb$tick()}
  }
    bird <- bird[-rem,]
    bird <- rbind(bird,tmp_add)

  }

  if (!isTRUE(quiet)){pb$tick()}
  route <- bbs_data$route; if (!isTRUE(quiet)){pb$tick()}

  if (by == "bbs_usgs")
  {
    route[,"strat_name"] <- paste(route[,"Country"],
                                  route[,"St_Abrev"],
                                  route[,"BCR"],
                                  sep = "-")
  }else if (by == "state")
  {
    route[,"strat_name"] <- paste(route[,"St_Abrev"],
                                  sep = "")
  }else if (by == "bcr")
  {
    route[,"strat_name"] <- paste("BCR",
                                  route[,"BCR"],
                                  sep = "")
  }else if (by == "latlong")
  {
    route[,"strat_name"] <- paste(trunc(route[,"Latitude"]),
                                  trunc(route[,"Longitude"]),
                                  sep = "_")
  }else if (by == "bbs_cws")
  {
    # Combine all BCR 7
    route[which(route$BCR == 7),"St_Abrev"] <- "BCR7"
    route[which(route$BCR == 7), "Route"] <- route[which(route$BCR == 7), "Route"]+(1000*route[which(route$BCR == 7), "statenum"])
    bird[which(bird$BCR == 7), "Route"] <- bird[which(bird$BCR == 7), "Route"]+(1000*bird[which(bird$BCR == 7), "statenum"])
    route[which(route$BCR == 7), "statenum"] <- 777
    bird[which(bird$BCR == 7), "statenum"] <- 777

    # Combine NS and PEI
    route[which(route$State == "Nova Scotia"), "State"] <- "Nova Scotia Prince Edward Island"
    ## adding 200 to hte PEI route numbers, so they remain distinct from the Nova Scotia routes (1, 5, 9, and 13)
    route[which(route$State == "Prince Edward Island"), "Route"] <- route[which(route$State == "Prince Edward Island"), "Route"]+ (1000*route[which(route$State == "Prince Edward Island"), "statenum"])
    route[which(route$State == "Prince Edward Island"), "State"] <- "Nova Scotia Prince Edward Island"
    route[which(route$State == "Nova Scotia Prince Edward Island"), "St_Abrev"] <- "NSPE"
    route[which(route$St_Abrev == "NSPE"), "statenum"] <- 765
    bird[which(bird$statenum == 65), "statenum"] <- 765
    ## adding statevalue*1000 to hte PEI route numbers, so they remain distinct from the Nova Scotia routes (1, 5, 9, and 13)
    bird[which(bird$statenum == 75), "Route"] <- bird[which(bird$statenum == 75), "Route"]+ (1000* bird[which(bird$statenum == 75), "statenum"])
    bird[which(bird$statenum == 75), "statenum"] <- 765

    route[,"strat_name"] <- paste(route[,"Country"],
                                  route[,"St_Abrev"],
                                  route[,"BCR"],
                                  sep = "-")

  }; if (!isTRUE(quiet)){pb$tick()}

  route[,"rt.uni"] <- paste(route[,"statenum"],route[,"Route"],sep = "-"); if (!isTRUE(quiet)){pb$tick()} # regenerates the rt.uni value with newly defined combined states
  bird[,"rt.uni"] <- paste(bird[,"statenum"],bird[,"Route"],sep = "-"); if (!isTRUE(quiet)){pb$tick()}

  route[,"rt.uni.y"] <- paste(route[,"statenum"],route[,"Route"],route[,"Year"],sep = "-"); if (!isTRUE(quiet)){pb$tick()}  # regenerates the rt.uni.y value with newly defined combined states
  bird[,"rt.uni.y"] <- paste(bird[,"statenum"],bird[,"Route"],bird[,"Year"],sep = "-"); if (!isTRUE(quiet)){pb$tick()}


  return(list(bird_strat = bird,
              route_strat = route,
              species_strat = species,
              stratify_by = by))
}


#' Stratify and filter Breeding Bird Survey data
#'
#' Assigns count data to strata and filters by species of interest. Routes are
#' assigned to strata based on their geographic location and the stratification
#' specified by the user. Species are filtered by matching English, French or
#' Scientific names to those in the BBS species data (see `search_species()` for
#' a flexible search to identify correct species names).
#'
#'
#' @param by Character. Stratification type. Either an established type, one of
#'   "state", "bcr", "latlong", "bbs_cws", "bbs_usgs", or a custom name (see
#'   strata_custom for details).
#' @param strata_custom Data frame or sf spatial data frame. Data frame
#'   of modified existing stratification, or a sf spatial data frame with
#'   polygons defining the custom stratifications. See Details.
#' @param combine_species_forms Logical. Whether to combine ambiguous species
#'   forms. See Details.
#' @param release
#' @param sample_data Logical. Use sample data (just Pacific Wren)
#' @param quiet Logical. Suppress progress messages
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
#'   groupings (e.g., "unid." are combined with "Dusky Grouse" and "Sooty
#'   Grouse" into "Blue Grouse (Dusky/Sooty)"). If the user wishes to keep the
#'   forms separate, `combine_species_forms` can be set to `FALSE`. See the data
#'   frame `species_forms`, for which species are set to be combined with which
#'   other species.
#'
#'   See `vignette("stratification", package = "bbsBayes")` for more details.
#'
#' @return Large list with `meta_data`, `meta_strata`, `birds_strata`, and
#'   `routes_strata`
#'
#' @examples
#'
#' # Sample Data - USGS BBS stara
#' s <- stratify(by = "bbs_usgs", sample_data = TRUE)
#'
#' # Full data - species and stratification
#' # Use `search_species()` to get correct species name
#'
#' # Stratify by Bird Conservation Regions
#' s <- stratify(by = "bcr", species = "Great Horned Owl")
#'
#' # Stratify by CWS BBS strata
#' s <- stratify(by = "bbs_cws", species = "Canada Jay")
#'
#' # Stratify by State/Province/Territory only
#' s <- stratify(by = "prov_state", species = "Common Loon")
#'
#' # Stratify by blocks of 1 degree of latitude X 1 degree of longitude
#' s <- stratify(by = "latlong", species = "Snowy Owl")
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
                     quiet = FALSE) {

  # Checks
  by <- check_strata(by, custom = strata_custom, quiet = quiet)
  stratify_by <- by[1]
  stratify_type <- by[2]
  if(!inherits(strata_custom, "data.frame")) check_sf(strata_custom, col = TRUE)

  # Load BBS Data (full or sample)
  bbs_data <- load_bbs_data(release = release, sample = sample_data, quiet = quiet)

  # Load and filter bbs data
  species_list <- bbs_data$species
  birds <- bbs_data$birds
  routes <- bbs_data$routes

  rm(bbs_data) # Save memory

  # Check and filter species
  if(!sample_data) {
    sp_aou <- check_species(species, species_list, combine_species_forms, quiet)
    birds <- dplyr::filter(birds, aou == .env$sp_aou)
    if(!combine_species_forms) birds <- dplyr::filter(birds, !.data$unid_combined)
  } else {
    if(!quiet) message("Using species Pacific Wren (sample data)")
    species <- "Pacific Wren"
  }

  if(!quiet) message("Stratifying data...")

  # Create temporary `rid` (because faster for birds to join)
  routes <- dplyr::mutate(
    routes, rid = paste(.data$country_num, .data$state_num, .data$route, sep = "-"))

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
      meta_strata <- bbs_strata[[stratify_by]]
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

  # Omit strata with no routes
  meta_strata <- dplyr::semi_join(meta_strata, routes, by = "strata_name")

  # Omit routes that do not fit in to strata
  n_na <- sum(is.na(routes$strata_name))
  if(!quiet & n_na > 0) {
    message(
      "  Omitting ",
      format(n_na, big.mark = ","), "/",
      format(nrow(routes), big.mark = ","),
      " routes that do not match a stratum...")
  }
  routes <- dplyr::filter(routes, !is.na(strata_name))

  if(nrow(routes) == 0) {
    stop("No routes within this stratification", call. = FALSE)
  }

  # routes - create rt_uni and rt_uni_y value with newly defined combined states
  if(!quiet) message("  Renaming routes...")
  routes <- dplyr::mutate(routes,
                          route = paste0(.data$state_num, "-", .data$route))

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
  list("meta_data" = list("stratify_by" = stratify_by,
                          "stratify_type" = stratify_type,
                          "species" = species),
       "meta_strata" = meta_strata,
       "birds_strata" = dplyr::select(birds, -"rid"),
       "routes_strata" = dplyr::select(routes, -"rid"))
}

stratify_map <- function(strata_map, routes, quiet = FALSE) {

  if(!quiet) {
    c <- sf::st_crs(strata_map, parameters = TRUE)[c("srid", "Name")]
    message("Preparing custom strata (", c$srid, "; ", c$Name, ")...")
  }

  # Keep strata name column only
  strata_map <- dplyr::select(strata_map, "strata_name") %>%
    dplyr::mutate(strata_name = as.character(strata_name))

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
    dplyr::select("strata_name", names(routes)) # reorder

  list("meta_strata" = sf::st_drop_geometry(strata_map),
       "routes" = routes)
}
