#' Fetch Breeding Bird Survey dataset
#'
#' Fetch and download Breeding Bird Survey data from the United States
#' Geological Survey (USGS) FTP site. This is the raw data that is uploaded to
#' the site before any analyses are performed. Users can download different
#' types (`state`, `stop`) and different releases (currently `2020` and `2022`).
#'
#' @param force Logical. Should pre-exising BBS data be overwritten? Default
#'   FALSE.
#' @param compression Character. What compression should be used to save data?
#'   Default is `none` which takes up the most space but is the fastest to
#'   load. Must be one of `none`, `gz`, `bz2`, or `xz` (passed to
#'   `readr::write_rds()`'s `compress` argument).
#'
#'
#' @inheritParams common_docs
#'
#' @details
#'
#' Users will be asked before saving the BBS data to a package-specific
#' directory created on their computer. Before downloading any data, users must
#' thoroughly read through the USGS terms and conditions for that data and enter
#' the word "yes" to agree.
#'
#' BBS `state` level counts provide counts beginning in 1966, aggregated in five
#' bins, each of which contains cumulative counts from 10 of the 50 stops along
#' a route. In contrast BBS `stop` level counts provides stop-level data
#' beginning in 1997, which includes counts for each stop along routes
#' individually. **Note that stop-level data is not currently supported by the
#' modelling utilities in bbsBayes.**
#'
#' There are two releases for each type of data, `2020` and `2022`. By default
#' all functions use the most recent release unless otherwise specified. For
#' example, the `release` argument in `stratify()` can be changed to `2020` to
#' use the 2020 release of state-level counts.
#'
#'
#' @examplesIf interactive()
#'
#' fetch_bbs_data(force = TRUE)
#' fetch_bbs_data(level = "stop", force = TRUE)
#' fetch_bbs_data(release = 2020, force = TRUE)
#' fetch_bbs_data(release = 2020, level = "stop", force = TRUE)
#'
#' @export
#'
fetch_bbs_data <- function(level = "state",
                           release = 2022,
                           force = FALSE,
                           quiet = FALSE,
                           compression = "none") {

  check_in(level, c("state", "stop"))
  check_release(release)

  check_logical(c(quiet, force))

  out_file <- check_bbs_data(level, release, force, quiet)

  # Print Terms of Use
  terms <- readChar(system.file(paste0("data-terms-",release),
                                package = "bbsBayes"),
                    file.info(system.file(paste0("data-terms-",release),
                                          package = "bbsBayes"))$size)

  message(terms)
  agree <- readline(prompt = "Type \"yes\" (without quotes) to agree: ")
  if(agree != "yes") return(NULL)

  fetch_bbs_data_internal(level, release, force, quiet, out_file, compression)
}


fetch_bbs_data_internal <- function(level = "state", release = 2022,
                                    force = FALSE, quiet = TRUE,
                                    out_file = NULL, compression = "none") {

  if(is.null(out_file)) out_file <- check_bbs_data(level, release, force, quiet)

  if(!quiet) message("Connecting to USGS ScienceBase...", appendLF = FALSE)

  connection <- sbtools::item_get(sb_id = get_sb_id(release))

  if(!is.null(connection) & !quiet) message("Connected!")

  # Download/load Data --------------
  birds <- get_birds(level, quiet, connection, force)
  routes <- get_routes(release, quiet, connection, force)
  weather <- get_weather(quiet, connection, force)
  regs <- readr::read_csv(
    system.file("data-import", "regs.csv", package = "bbsBayes"),
    col_types = "cnncc", progress = FALSE) %>%
    dplyr::rename_with(snakecase::to_snake_case) %>%
    dplyr::rename(country_num = "countrynum", state_num = "statenum")

  # Combine routes, weather, and region ----------------
  routes <- dplyr::inner_join(routes, weather,
                              by = c("country_num", "state_num", "route"))

  # remove off-road and water routes, as well as non-random and mini-routes
  routes <- routes %>%
    dplyr::filter(.data$route_type_detail_id == 1,
                  .data$route_type_id == 1,
                  .data$run_type == 1) %>%
    dplyr::select(-"stratum")

  routes <- dplyr::inner_join(routes, regs, by = c("country_num", "state_num"))

  # Combine routes and birds -----------
  unique_routes <- routes %>%
    dplyr::select("bcr", "state_num", "route", "country_num") %>%
    dplyr::distinct()

  birds <- dplyr::inner_join(birds, unique_routes,
                             by = c("state_num", "route", "country_num"))

  # Species Data ----------------

  if(!quiet) message("Downloading species data (Task 3/3)")

  temp <- tempfile()
  suppressMessages({
    full_path <- sbtools::item_file_download(sb_id = connection,
                                             names = "SpeciesList.txt",
                                             destinations = temp)
  })
  if(!quiet) message("\n") # For next message

  if(release == 2022) lskip <- 14 #silly differences in file structure
  if(release == 2020) lskip <- 11

  meta <- readr::read_lines(temp, n_max = 30, progress = FALSE)
  lskip <- stringr::str_which(meta, "---------")
  col_names <- stringr::str_split(meta[lskip - 1], "( )+") %>%
    unlist() %>%
    tolower() %>%
    stringr::str_remove("_common_name")
  col_names <- col_names[col_names != ""]

  # col_names should be:
  #  "seq", "aou", "english", "french", "spanish", "order",
  #  "family", "genus", "species"

  widths <- c(7, 6, 51, 51, 51, 51, 51, 51, NA)
  species <- readr::read_fwf(
    file = temp,
    col_positions = readr::fwf_widths(widths, col_names),
    col_types = "iiccccccc",
    skip = lskip, locale = readr::locale(encoding = "latin1"),
    progress = FALSE)


  # Combine species forms -------------------------------------
  b <- combine_species(birds, species, quiet)
  birds <- b$birds
  species <- b$species

  # Write Data -----------------------
  bbs_data <- list(birds = birds,
                   routes = routes,
                   species = species,
                   meta = data.frame(release = release,
                                     download_date = Sys.Date()))

  if(!quiet) message("Saving BBS data to ", out_file)
  readr::write_rds(bbs_data, file = out_file, compress = compression)

  # Clean Up -------------------------
  if(!quiet) message("Removing temp files")
  unlink(list.files(tempdir(), full.names = TRUE), recursive = TRUE)

}

get_sb_id <- function(release) {
  switch(as.character(release),
         "2022" = "625f151ed34e85fa62b7f926",
         "2020" = "5ea04e9a82cefae35a129d65")
}


bbs_dir <- function(quiet = TRUE) {

  d <- R_user_dir("bbsBayes", which = "data") #imported from backports pkg

  if(!dir.exists(d)) {
    if(!quiet) message(paste0("Creating data directory at ", d))
    dir.create(d, recursive = TRUE)
  } else {
    if(!quiet) message(paste0("Using data directory at ", d))
  }

  d
}

#' Remove bbsBayes cache
#'
#' Remove all or some of the data downloaded via `fetch_bbs_data()` as well as
#' model executables created by `cmdstanr::cmdstan_model()` via `run_model()`.
#'
#' @param type Character. Which cached data to remove. One of "all", "bbs_data",
#'   or "models". If "all", removes entire cache directory (and all data
#'   contained therein). If "bbs_data", removes only BBS data downloaded with
#'   `fetch_bbs_data()`. If "models", removes only model executables compiled
#'   when `run_models()` is run.
#' @param level Character. BBS data to remove, one of "all", "state", or "stop".
#'   Only applies if `type = "bbs_data"`.
#' @param release Character/Numeric. BBS data to remove, one of "all", 2020, or
#'   2022. Only applies if `type = "bbs_data"`.
#'
#' @return Nothing.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Remove everything
#' remove_cache(type = "all")
#'
#' # Remove all BBS data files (but not the dir)
#' remove_cache(level = "all", release = "all")
#'
#' # Remove all 'stop' data
#' remove_cache(level = "stop", release = "all")
#'
#' # Remove all 2020 data
#' remove_cache(level = "all", release = 2020)
#'
#' # Remove 2020 stop data
#' remove_cache(level = "stop", release = 2020)
#'
#' # Remove all model executables
#' remove_cache(type = "model")
#' }
#'
remove_cache <- function(type = "bbs_data", level, release) {
  if(type == "all") {
    message("Removing all data files (BBS data and Stan models) ",
            "and cache directory")
    unlink(bbs_dir(), recursive = TRUE)
  } else {

    if(type == "bbs_data") {
      check_in(level, c("all", "state", "stop"))
      check_release(release, all = TRUE)

      if(level == "all") level <- c("state", "stop")
      if(release == "all") release <- c("2020", "2022")

      f <- file.path(bbs_dir(), paste0("bbs_", level, "_data_", release, ".rds"))
      f <- f[file.exists(f)]
    } else if(type == "models") {
      f <- list.files(bbs_dir(), "CV$", full.names = TRUE)
    }

    if(length(f) > 0) {
      message("Removing ", paste0(f, collapse = ", "), " from the cache")
      unlink(f)
    } else message("No data files to remove")
  }
}

#' Check whether BBS data exists locally
#'
#' Use this function to check if you have the BBS data downloaded and where
#' bbsBayes is expecting to find it. If it returns `FALSE`, the data is not
#' present; use `fetch_bbs_data()` to retrieve it.
#'
#' @param level Character. BBS data to check, one of "all", "state", or "stop".
#'   Default "state".
#' @param release Character/Numeric. BBS data to check, one of "all", 2020, or
#'   2022. Default 2022.
#'
#' @inheritParams common_docs
#'
#' @returns `TRUE` if the data is found, `FALSE` otherwise
#'
#' @export
#'
#' @examples
#' have_bbs_data()
#' have_bbs_data(release = 2020)
#' have_bbs_data(release = "all", level = "all")

have_bbs_data <- function(level = "state", release = 2022, quiet = FALSE){
  check_in(level, c("all", "state", "stop"))
  check_release(release, all = TRUE)

  if(level == "all") level <- c("state", "stop")
  if(release == "all") release <- c("2020", "2022")

  # To get all combos
  f <- vector()
  for(r in release) f <- c(f, paste0("bbs_", level, "_data_", r, ".rds"))
  f <- file.path(bbs_dir(), f)

  if(!quiet) {
    msg <- paste0("Expected BBS ", level, " data ", release, ": '", f, "'")
    msg <- paste0(msg, collapse = "\n")
    message(msg)
  }

  file.exists(f)
}

get_encoding <- function() {
  if(l10n_info()[["UTF-8"]]) e <- "latin1" else e <- ""
  e
}

get_birds <- function(level, quiet, connection, force) {
  if (level == "state") count_zip <- "States.zip"
  if (level == "stop") count_zip <- "50-StopData.zip"

  bird_count_filenames <- system.file("data-import",
                                 paste0(level, "-dir.csv"),
                                 package = "bbsBayes") %>%
    utils::read.csv()

  if(!quiet) message("Downloading count data (Task 1/3)")
  suppressMessages({
    full_path <- sbtools::item_file_download(
      sb_id = connection,
      names = count_zip,
      destinations = file.path(tempdir(), count_zip),
      overwrite_file = force)})

  unz_path <- utils::unzip(zipfile = full_path, exdir = tempdir())

  birds <- unz_path %>%
    purrr::map(utils::unzip, exdir = tempdir()) %>%
    purrr::map(readr::read_csv, col_types = "nnnnnnnnnnnnnnn",
               progress = FALSE) %>%
    purrr::map(~dplyr::rename_with(.x, snakecase::to_snake_case)) %>%
    dplyr::bind_rows()

  unlink(full_path)

  birds
}

get_routes <- function(release, quiet, connection, force) {

  if (!quiet) {
    message("Downloading route data (Task 2/3)")
    message("  - routes...")
  }

  if(release == 2020) {
    # if necessary because file name changed between 2020 and 2022 releases
    rtsfl <- "routes.zip"
  } else{
    rtsfl <- "Routes.zip"
  }

  suppressMessages({
    full_path <- sbtools::item_file_download(
      sb_id = connection,
      names = rtsfl,
      destinations = file.path(tempdir(), rtsfl),
      overwrite_file = force)
  })

  routes <- readr::read_csv(
    utils::unzip(zipfile = full_path, exdir = tempdir()),
    na = c("NA", "", "NULL"),
    col_types = "nnncnnnnnnn", progress = FALSE,
    locale = readr::locale(encoding = "latin1")) %>%
    dplyr::rename_with(snakecase::to_snake_case)

  unlink(full_path)

  routes
}


get_weather <- function(quiet, connection, force) {

  if(!quiet) message("  - weather...")
  suppressMessages({
    full_path <- sbtools::item_file_download(
      sb_id = connection,
      names = "Weather.zip",
      destinations = file.path(tempdir(), "Weather.zip"),
      overwrite_file = force)
  })

  weather <- readr::read_csv(
    utils::unzip(zipfile = full_path, exdir = tempdir()),
    col_types = "nnnnnnnnnnnncnnnnnn",
    na = c("NA", "", "NULL"), progress = FALSE) %>%
    dplyr::rename_with(snakecase::to_snake_case)

  unlink(full_path)

  weather
}

combine_species <- function(birds, species, quiet = FALSE) {

  if(!quiet) message("Combining species forms for optional use")

  # Species list - Rename unidentified to name of combo group
  s <- species %>%
    dplyr::left_join(bbsBayes::species_forms, by = c("aou" = "aou_unid")) %>%
    dplyr::mutate(
      english = dplyr::if_else(
        !is.na(.data$english_combined), .data$english_combined, .data$english),
      french = dplyr::if_else(
        !is.na(.data$french_combined), .data$french_combined, .data$french)) %>%
    dplyr::select(names(species)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(unid_combined = TRUE)

  # Birds - Pull out original, non-combined, non-unidentified species, & combine
  sp_forms <- tidyr::unnest(bbsBayes::species_forms, "aou_id")

  b <- birds %>%
    dplyr::inner_join(dplyr::select(sp_forms, "aou_unid", "aou_id"),
                      by = c("aou" = "aou_id")) %>%
    dplyr::mutate(aou = .data$aou_unid) %>%
    dplyr::select(-"aou_unid") %>%
    dplyr::mutate(unid_combined = TRUE)

  # Add combined species to both data sets
  # (note that this duplicates observations/listings, but permits use of
  # different species groupings when specifing later steps

  birds <- dplyr::mutate(birds, unid_combined = FALSE) %>%
    dplyr::bind_rows(b)

  species <- dplyr::mutate(species, unid_combined = FALSE) %>%
    dplyr::bind_rows(s)

  list("birds" = birds,
       "species" = species)
}
