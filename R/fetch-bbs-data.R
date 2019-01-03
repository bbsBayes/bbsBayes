#' Fetch Breeding Bird Survey dataset
#'
#' \code{fetch_bbs_data} uses File Transfer Protocol (FTP) to fetch Breeding Bird
#'  Survey data from the United States Geological Survey (USGS) FTP site. This
#'  is the raw data that is uploaded to the site before any analyses are performed.
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
#' @param quiet Logical: should progress bars be suppressed?
#'
#' @return NULL if user does not agree to terms and conditions.
#'   Otherwise: Large list (3 elements) consisting of:
#' \item{bird}{Data frame of bird point count data per route, per year}
#' \item{route}{Data frame of yearly route data}
#' \item{species}{List of North American bird species}
#'
#' @importFrom utils download.file read.csv read.fwf
#' @importFrom progress progress_bar
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Be sure to assign the output of fetch_bbs_data() to a variable.
#' # This function downloads a lot of data, and if the output is not
#' # captured in a variable, R attempts to dump it into the console
#' # which tends to get highly bogged down.
#'
#' bbs_data <- fetch_bbs_data()
#'
#' # USGS updates their data up to twice a year, so one potential
#' # option is to save the bbs_data to a file so that you do not
#' # have to run fetch_bbs_data() every new session.
#'
#' save(bbs_data, file = "bbs_data.RData")
#' }
#'
fetch_bbs_data <- function(level = "state", quiet = FALSE)
{
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

  bird <- get_counts(level = level, quiet = quiet)

  ################################################################
  # Route List Data
  ################################################################
  if (!isTRUE(quiet))
  {
    pb <- progress::progress_bar$new(
      format = "Downloading route data   [:bar] :percent eta: :eta",
      clear = FALSE,
      total = 9,
      width = 80)
    pb$tick(0)
  }

  temp <- tempfile()
  utils::download.file(paste0(base_url(), "routes.zip"), temp, quiet = TRUE)
  tick(pb, quiet)
  routes <- utils::read.csv(unz(temp, paste0("routes.csv")),
                            stringsAsFactors = FALSE)
  unlink(temp)
  tick(pb, quiet)

  #removes the off-road and water routes, as well as non-random and mini-routes
  routes <- routes[routes$RouteTypeDetailID == 1 & routes$RouteTypeID == 1, ]
  routes$Stratum <- NULL
  tick(pb, quiet)

  ################################################################
  # Weather Data
  ################################################################

  temp <- tempfile()
  utils::download.file(paste0(base_url(), "Weather.zip"), temp, quiet = TRUE)
  tick(pb, quiet)
  weather <- utils::read.csv(unz(temp, paste0("weather.csv")),
                             stringsAsFactors = FALSE)
  tick(pb, quiet)
  unlink(temp)

  #removes the off-road and water routes, as well as non-random and mini-routes
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
    pb <- progress::progress_bar$new(
      format = "Downloading species data [:bar] :percent eta: :eta",
      clear = FALSE,
      total = 4,
      width = 80)
    pb$tick(0)
  }

  temp <- tempfile()
  utils::download.file(paste0(base_url(), "SpeciesList.txt"), temp, quiet = TRUE)
  tick(pb, quiet)
  species <- utils::read.fwf(temp, skip = 9, strip.white = TRUE, header = FALSE,
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

  species <- species[, -c(4, 5)] # remove french and scientific name
  names(species) <- c("seq","aou","english","order","family","genus","species")
  tick(pb, quiet)

  # this reads in the USGS BBS ftp site species file
  species[, "sp.bbs"] <- as.integer(as.character(species[, "aou"]))
  tick(pb, quiet)

  return(list(bird = bird,
              route = route,
              species = species))
}


get_counts <- function(level, quiet) {
  if (!level %in% c('state', 'stop')) {
    stop("Invalid level argument: level must be one of 'state' or 'stop'.")
  }
  if (level == "state") {
    count_ftp_subdir <- "States/"
  }
  if (level == "stop") {
    count_ftp_subdir <- "50-StopData/1997ToPresent_SurveyWide/"
  }
  count_ftp_dir <- paste0(base_url(), count_ftp_subdir)

  dir_listing_csv <- system.file("data-import",
                                 paste0(level, "-dir.csv"),
                                 package = "bbsBayes")
  bird_count_filenames <- utils::read.csv(dir_listing_csv)

  if (!isTRUE(quiet)) {
    pb <- progress::progress_bar$new(
      format = "Downloading count data   [:bar] :percent eta: :eta",
      clear = FALSE,
      total = nrow(bird_count_filenames) + 1,
      width = 80)
    pb$tick(0)
  }

  bird <- vector(mode = "list", length = nrow(bird_count_filenames))
  for(i in seq_along(bird_count_filenames$File.Name)) {
    fname <- bird_count_filenames$File.Name[i]
    fname_no_ext <- tools::file_path_sans_ext(fname)
    temp <- tempfile()
    utils::download.file(paste0(count_ftp_dir, fname_no_ext, ".zip"),
                         destfile = temp,
                         quiet = TRUE)
    dataset_name <- paste0(fname_no_ext, ".csv")
    if (level == "stop") {
      # zip files use title case 'Fifty1.zip', but csv filenames are lowercase
      dataset_name <- gsub("^Fifty", replacement = "fifty", x = dataset_name)
    }
    bird[[i]] <- utils::read.csv(unz(temp, dataset_name),
                                 stringsAsFactors = FALSE)
    unlink(temp)
    tick(pb, quiet)
  }

  bird <- do.call(rbind, bird)

  # column case conventions differ for state vs. stop level data, so we set:
  to_lower <- c('countrynum', 'statenum')
  to_upper <- 'Year'
  names(bird)[match(to_lower, tolower(names(bird)))] <- to_lower
  names(bird)[match(to_upper, tools::toTitleCase(names(bird)))] <- to_upper
  tick(pb, quiet)
  return(bird)
}


base_url <- function() {
  "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
}

tick <- function(pb, quiet) {
  if (!isTRUE(quiet)){
    pb$tick()
  }
}
