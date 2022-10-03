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
#' @importFrom progress progress_bar
#' @importFrom rappdirs app_dir
#' @export
#'

stratify <- function(by = NULL,
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
                             fileEncoding = "latin1",
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


#' Stratify raw Breeding Bird Survey data
#'
#' Assigns each bird count data point and each route a strata based on its
#' geographic location and the stratification as specified by the user.
#'
#' @param by Character. Stratification type. One of "state", "bcr", "latlong",
#'   "bbs_cws", "bbs_usgs"
#' @param sample_data Logical. Use sample data (just Pacific Wren).
#' @param quiet Logical. Suppress progress messages
#' @param bbs_data List. Raw BBS data saved as a list of 3 data frames.
#'   Not necessary if you have already run `fetch_bbs_data()`
#' @param lump_species_forms Logical. Whether to lump together species with
#'   multiple forms (default `TRUE`). See Details.
#' @param stratify_by Deprecated in favour of `by`.
#'
#' @details If `lump_species_forms` is `TRUE`, species with multiple forms
#'   (e.g., "(unid. race) Dark-eyed Junco"), are duplicated in the data. Once
#'   with individual, identified, forms, and once by lumping all forms together
#'   (including both unidentified and identified forms). This allows users to
#'   specify either individual forms, (e.g., "(Oregon Junco) Dark-eyed Junco")
#'   or grouped forms (e.g., "Dark-eyed Junco (all forms)") in later steps
#'   (e.g., `prepare_data()`) without rerunning the `stratify()` step. If the
#'   user wishes to keep the unidentified form separate, `lump_species_forms`
#'   can be set to `FALSE`.
#'
#' @return Large list (3 elements) consisting of:
#'   \item{birds_strat}{Data frame of stratified bird counts data}
#'   \item{routes_strat}{Data frame of stratified routes data}
#'   \item{species_strat}{Data frame of stratified species data}
#'   \item{stratify_by}{Argument used for stratification}
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
#' @export
#'

stratify_tidy <- function(by,
                          sample_data = FALSE,
                          bbs_data = NULL,
                          lump_species_forms = TRUE,
                          quiet = FALSE,
                          stratify_by = NULL) {

  if(!is.null(stratify_by)) {
    message("Argument \"stratify_by\" has been deprecated in favour of \"by\"")
    by <- stratify_by
  }

  by <- check_stratification(by)

  # Load BBS Data
  if(sample_data) {
    bbs_data <- load_sample_data_tidy()                  # Load sample data
  } else if(is.null(bbs_data)) {
    if(!quiet) message("Loading BBS data...")
    bbs_data <- load_bbs_data_tidy(level = "state") # Load BBS data
  } else {
    bbs_data <- check_bbs_data(bbs_data)            # Check user supplied data
  }

  species <- bbs_data$species %>%
    dplyr::mutate(aou = as.numeric(aou))

  birds <- bbs_data$birds
  routes <- bbs_data$routes

  if(!sample_data & lump_species_forms) {

    if(!quiet) message("Lumping species forms")

    # Load species forms
    lump_sp <- readr::read_csv(
      system.file("species-lump-split", "lump.csv", package = "bbsBayes"),
      col_types = "ncccnnnnnn", locale = readr::locale(encoding = "latin1"),
      progress = FALSE) %>%
      dplyr::mutate(aou_add = purrr::pmap(list(aou1, aou2, aou3, aou4, aou5),
                                          ~c(...)),
                    aou_add = purrr::map(aou_add, ~.x[!is.na(.x)])) %>%
      dplyr::select(-dplyr::matches("aou[1-5]{1}"))

    # Rename unidentified to name of lumped group
    species <- species %>%
      dplyr::left_join(lump_sp, by = c("sp.bbs" = "aou_original")) %>%
      dplyr::mutate(
        english = dplyr::if_else(!is.na(english_out), english_out, english),
        french = dplyr::if_else(!is.na(french_out), french_out, french)) %>%
      dplyr::select(names(species))

    # Pull out original, non-lumped, non-unidentified species and lump
    lump_sp <- tidyr::unnest(lump_sp, "aou_add")

    b <- birds %>%
      dplyr::inner_join(dplyr::select(lump_sp, "aou_original", "aou_add"),
                        by = c("AOU" = "aou_add")) %>%
      dplyr::mutate(AOU = .data$aou_original) %>%
      dplyr::select(-aou_original)

    # Add to birds
    # (note that this duplicates observations under a different species aou
    # code. This permits use of different species groupings in modelling)
    birds <- dplyr::bind_rows(birds, b)
  }

  if(!quiet) message("Stratifying data...")

  # Create temporary rid (because easier for birds to join)
  routes <- dplyr::mutate(
    routes, rid = paste(.data$countrynum, .data$statenum, .data$Route, sep = "-"))

  birds <- dplyr::select(routes, "countrynum", "statenum", "Route", "rid") %>%
    dplyr::distinct() %>%
    dplyr::left_join(birds, .,
                     by = c("countrynum", "statenum", "Route"))


  if (by == "bbs_cws") {

    if(!quiet) message("  Combining BCR 7 and NS and PEI...")

    # Combine all BCR 7
    bcr7 <- dplyr::filter(routes, .data$BCR == 7) %>%
      dplyr::mutate(St_Abrev = "BCR7",
                    Route = .data$Route + (1000 * .data$statenum),
                    statenum = 777)

    # Combine NS and PEI
    ns_pei <- dplyr::filter(routes, .data$St_Abrev %in% c("PE", "NS")) %>%
      dplyr::mutate(
        State = "Nova Scotia Prince Edward Island",
        # Keep PEI route numbers distinct from Nova Scotia routes
        Route = dplyr::if_else(.data$St_Abrev == "PE",
                               .data$Route + (1000 * .data$Route),
                               .data$Route),
        St_Abrev = "NSPE",
        statenum = 765)

    # Add to routes
    # Note: This only works because the two data sets are mutually exclusive
    #       Otherwise bind together between sets.
    routes <- routes %>%
      dplyr::filter(.data$BCR != 7,                         # Omit BCR 7
                    !.data$St_Abrev %in% c("PE", "NS")) %>% # Omit PE and NS
      dplyr::bind_rows(bcr7, ns_pei)                        # Add combo data

    # Fix birds
    birds <- dplyr::select(birds, -"Route", -"statenum") %>%
      dplyr::left_join(dplyr::select(routes, "rid", "Route", "statenum"),
                       by = "rid")
  }

  # Create Strat Name
  routes <- dplyr::mutate(
    routes,
    strat_name = dplyr::case_when(
      by == "state" ~ .data$St_Abrev,
      by == "bcr" ~ paste0("BCR", .data$BCR),
      by == "latlong" ~ paste0(trunc(.data$Latitude),
                               "_",
                               trunc(.data$Longitude)),
      by %in% c("bbs_usgs", "bbs_cws") ~
        paste0(.data$Country, "-", .data$St_Abrev, "-", .data$BCR)))

  # Re-create rt.uni and rt.uni.y value with newly defined combined states
  if(!quiet) message("  Renaming routes based on stratification...")
  routes <- dplyr::mutate(routes,
                          rt.uni = paste0(.data$statenum, "-", .data$Route),
                          rt.uni.y = paste0(.data$rt.uni, "-", .data$Year))

  # Create rt.uni and rt.uni.y by index then join
  b_index <- dplyr::select(birds, "rid", "Year", "statenum", "Route") %>%
    dplyr::distinct() %>%
    dplyr::mutate(rt.uni = paste0(.data$statenum, "-", .data$Route),
                  rt.uni.y = paste0(.data$rt.uni, "-", .data$Year)) %>%
    dplyr::select("rid", "Year", "rt.uni", "rt.uni.y")

  birds <- dplyr::left_join(birds, b_index, by = c("rid", "Year"))


  list(birds_strat = dplyr::select(birds, -"rid"),
       routes_strat = dplyr::select(routes, -"rid"),
       species_strat = species,
       stratify_by = by)
}
