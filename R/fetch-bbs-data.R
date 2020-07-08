#' Fetch Breeding Bird Survey dataset
#'
#' \code{fetch_bbs_data} uses File Transfer Protocol (FTP) to fetch Breeding Bird
#'  Survey data from the United States Geological Survey (USGS) FTP site. This
#'  is the raw data that is uploaded to the site before any analyses are performed.
#'  A package-specific directory is created on the user's computer and the BBS
#'  data is saved to that directory for use by other functions.
#'  Before downloading any data, the user must thoroughly read through the terms
#'  and conditions of the user of the data and type the word "yes" to agree.
#'
#' @param level A string, either "state" or "stop", specifying which counts to
#' fetch. Defaults to "state", which provides counts beginning in 1966,
#' aggregated in five bins, each of which contains cumulative counts from 10 of
#' the 50 stops along a route. Specifying "stop" provides stop-level data
#' beginning in 1997, which includes counts for each stop along routes
#' individually. Note that stop-level data is not currently supported by
#' the modeling utilities in bbsBayes.
#' @param quiet Logical: should progress bars be suppressed? Defaults to FALSE
#' @param force Logical: if BBS data already exists on computer, should it be overwritten? Defaults to FALSE
#'
#'
#'
#' @importFrom utils download.file read.csv read.fwf unzip
#' @importFrom sbtools item_get item_file_download
#' @importFrom progress progress_bar
#' @importFrom rappdirs app_dir
#'
#' @return None
#'
#' @export
#'
#'
fetch_bbs_data <- function(level = "state",
                           quiet = FALSE,
                           force = FALSE)
{
  if (!level %in% c('state', 'stop')) {
    stop("Invalid level argument: level must be one of 'state' or 'stop'.")
  }

  stopifnot(is.logical(quiet))

  # Print Terms of Use
  terms <- readChar(system.file("data-terms",
                                package = "bbsBayes"),
                    file.info(system.file("data-terms",
                                          package = "bbsBayes"))$size)

  cat(terms)

  agree <- readline(prompt = "Type \"yes\" (without quotes) to agree: ")

  if (agree != "yes")
  {
    return(NULL)
  }

  bbs_dir <- app_dir(appname = "bbsBayes")

  if (isFALSE(file.exists(bbs_dir$data())))
  {
    message(paste0("Creating data directory at ", bbs_dir$data()))
    dir.create(bbs_dir$data(), recursive = TRUE)
  }

  if (level == "state")
  {
    if (file.exists(paste0(bbs_dir$data(), "/bbs_raw_data.RData")) &
        isFALSE(force))
    {
      warning("BBS state-level data file already exists. Use \"force = TRUE\" to overwrite.")
      return()
    }
  }else if (level == "stop")
  {
    if (file.exists(paste0(bbs_dir$data(), "/bbs_stop_data.RData")) &
        isFALSE(force))
    {
      warning("BBS stop-level data file already exists. Use \"force = TRUE\" to overwrite.")
      return()
    }
  }

  if (!isTRUE(quiet))
  {
    message("Connecting to USGS ScienceBase...", appendLF = FALSE)
  }

  connection <- sbtools::item_get(sb_id = get_sb_id())
  if (!is.null(connection))
  {
    if (!isTRUE(quiet))
    {
      message("Connected!")
    }
  }

  bird <- get_counts(level = level, quiet = quiet, sb_conn = connection)

  ################################################################
  # Route List Data
  ################################################################
  if (!isTRUE(quiet))
  {
    message("Downloading route data (Task 2/3)")
    pb <- progress::progress_bar$new(
      format = "\r[:bar] :percent eta: :eta",
      clear = FALSE,
      total = 9,
      width = 100)
    pb$tick(0)
  }

  temp <- tempdir()
  full_path <- sbtools::item_file_download(sb_id = connection,
                                           names = "routes.zip",
                                           destinations = file.path(temp, "routes.zip"))
  tick(pb, quiet)

  routes <- utils::read.csv(utils::unzip(zipfile = full_path,
                                         exdir = temp),
                            stringsAsFactors = FALSE)
  unlink(temp)
  tick(pb, quiet)

  # removes the off-road and water routes, as well as non-random and mini-routes
  routes <- routes[routes$RouteTypeDetailID == 1 & routes$RouteTypeID == 1, ]
  routes$Stratum <- NULL
  tick(pb, quiet)

  ################################################################
  # Weather Data
  ################################################################

  temp <- tempdir()
  full_path <- sbtools::item_file_download(sb_id = connection,
                                           names = "Weather.zip",
                                           destinations = file.path(temp, "Weather.zip"))
  tick(pb, quiet)

  weather <- utils::read.csv(utils::unzip(zipfile = full_path,
                                          exdir = temp),
                             stringsAsFactors = FALSE)
  unlink(temp)
  tick(pb, quiet)

  # removes the off-road and water routes, as well as non-random and mini-routes
  weather <- weather[weather$RunType == 1, ]
  tick(pb, quiet)

  # merge weather and routes
  # removes some rows from weather that are associated with the removed
  #   routes above (mini-routes etc.)
  route <- merge(routes, weather, by = c("CountryNum","StateNum","Route"))
  names(route)[tolower(names(route)) == "countrynum"] <- "countrynum"
  names(route)[tolower(names(route)) == "statenum"] <- "statenum"
  tick(pb, quiet)

  # Add region and BCR information to route and bird data frames
  regs <- utils::read.csv(system.file("data-import",
                                      "regs.csv",
                                      package="bbsBayes"),
                          stringsAsFactors = FALSE)
  tick(pb, quiet)

  # merge route data into the bird count data frame
  route <- merge(route, regs, by = c("countrynum", "statenum"))
  unique_routes <- unique(route[, c("BCR", "statenum", "Route", "countrynum")])
  bird <- merge(bird, unique_routes, by = c("statenum", "Route", "countrynum"))
  tick(pb, quiet)

  ################################################################
  # Species Data
  ################################################################

  if (!isTRUE(quiet))
  {
    message("Downloading species data (Task 3/3)")
    pb <- progress::progress_bar$new(
      format = "\r[:bar] :percent eta: :eta",
      clear = FALSE,
      total = 4,
      width = 100)
    pb$tick(0)
  }

  temp <- tempfile()
  full_path <- sbtools::item_file_download(sb_id = connection,
                                           names = "SpeciesList.txt",
                                           destinations = temp)
  tick(pb, quiet)

  species <- utils::read.fwf(temp, skip = 11, strip.white = TRUE, header = FALSE,
                             colClasses = c("integer",
                                            "character",
                                            "character",
                                            "character",
                                            "character",
                                            "character",
                                            "character",
                                            "character",
                                            "character"),
                             widths = c(6, -1, 5, -1, 50, -1, 50, -1, 50, -1,
                                        50, -1, 50, -1, 50, -1, 50),
                             fileEncoding = "iso-8859-1")
  unlink(temp)
  tick(pb, quiet)

  names(species) <- c("seq","aou","english","french","spanish","order","family","genus","species")
  tick(pb, quiet)

  # this reads in the USGS BBS ftp site species file
  species[, "sp.bbs"] <- as.integer(as.character(species[, "aou"]))
  tick(pb, quiet)

  bbs_data <- list(bird = bird,
                   route = route,
                   species = species)

  # temp <- tempfile()
  # utils::download.file(paste0(base_url(), "SpeciesList.txt"), temp, quiet = TRUE)
  # tick(pb, quiet)
  # species <- utils::read.fwf(temp, skip = 10, strip.white = TRUE, header = FALSE,
  #                            colClasses = c("integer",
  #                                           "character",
  #                                           "character",
  #                                           "character",
  #                                           "character",
  #                                           "character",
  #                                           "character",
  #                                           "character",
  #                                           "character"),
  #                            widths = c(6, -1, 5, -1, 50, -1, 50, -1, 50, -1,
  #                                       50, -1, 50, -1, 50, -1, 50),
  #                     fileEncoding = "iso-8859-1")
  # unlink(temp)
  # tick(pb, quiet)

  #species <- species[, -c(4, 5)] # remove french and spanish name
  # names(species) <- c("seq","aou","english","french","spanish","order","family","genus","species")
  # tick(pb, quiet)

  # # this reads in the USGS BBS ftp site species file
  # species[, "sp.bbs"] <- as.integer(as.character(species[, "aou"]))
  # tick(pb, quiet)
  #
  #   bbs_data <- list(bird = bird,
  #                    route = route,
  #                    species = species)

  if (level == "state")
  {
    message(paste0("Saving BBS data to ", bbs_dir$data()))
    save(bbs_data, file = paste0(bbs_dir$data(), "/bbs_raw_data.RData"))
  }else if (level == "stop")
  {
    message(paste0("Saving BBS data to ", bbs_dir$data()))
    save(bbs_data, file = paste0(bbs_dir$data(), "/bbs_stop_data.RData"))
  }

}


get_counts <- function(level, quiet, sb_conn) {
  if (level == "state") {
    count_zip <- "States.zip"
  }
  if (level == "stop") {
    count_zip <- "50-StopData.zip"
  }

  dir_listing_csv <- system.file("data-import",
                                 paste0(level, "-dir.csv"),
                                 package = "bbsBayes")
  bird_count_filenames <- utils::read.csv(dir_listing_csv)

  if (!isTRUE(quiet)) {
    message("Downloading count data (Task 1/3)")
    pb <- progress::progress_bar$new(
      format = "\r[:bar] :percent eta: :eta",
      clear = FALSE,
      total = nrow(bird_count_filenames) + 5,
      width = 100)
    pb$tick(0)
  }

  temp <- tempdir()
  full_path <- sbtools::item_file_download(sb_id = sb_conn,
                                           names = count_zip,
                                           destinations = file.path(temp, count_zip))
  tick(pb, quiet)

  unz_path <- utils::unzip(zipfile = full_path,
                           exdir = temp)
  tick(pb, quiet)

  bird <- vector(mode = "list", length = length(unz_path))
  for (i in 1:length(unz_path))
  {
    bird[[i]] <- utils::read.csv(utils::unzip(zipfile = unz_path[i],
                                              exdir = temp),
                                 stringsAsFactors = FALSE)
    tick(pb, quiet)
  }
  unlink(temp)

  # Coding around mistakes: 2020 data has countrynum and CountryNum
  #    in the 10th data frame of bird, so get rid of countrynum.
  #    Only relevant for level = "stop", this will probably be taken out
  #    next year lol
  if (level == "stop")
  {
    bird[[10]] <- bird[[10]][-c(2)]
  }


  # The "StateNum" column is inconsistently named - fix it to be consistent
  bird <- lapply(bird, function(x){
    names(x) <- ifelse(names(x) == "statenum", "StateNum", names(x))
    x
  })
  tick(pb, quiet)


  bird <- do.call(rbind, bird)
  tick(pb, quiet)

  # column case conventions differ for state vs. stop level data, so we set:
  to_lower <- c('countrynum', 'statenum')
  to_upper <- 'Year'
  names(bird)[match(to_lower, tolower(names(bird)))] <- to_lower
  names(bird)[match(to_upper, tools::toTitleCase(names(bird)))] <- to_upper
  tick(pb, quiet)
  return(bird)

}

get_sb_id <- function()
{
  return("5ea04e9a82cefae35a129d65")
}

tick <- function(pb, quiet) {
  if (!isTRUE(quiet)){
    pb$tick()
  }
}
