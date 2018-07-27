#' Fetch Breeding Bird Survey dataset
#'
#' \code{fetch_bbs_data} uses File Transfer Protocol (FTP) to fetch Breeding Bird
#'  Survey data from the United States Geological Survey (USGS) FTP site. This
#'  is the raw data that is uploaded to the site before any analyses are performed.
#'  Before downloading any data, the user must thoroughly read through the terms
#'  and conditions of the user of the data and type the word "yes" to agree.
#'
#' @param quiet Should progress bars be suppressed?
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
#' bbs_data <- fetch_bbs_data()
#' save(bbs_data, file = "bbs_data.RData")
#' }
#'
fetch_bbs_data <- function(quiet = FALSE)
{
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



  base_url <- "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
  version <- 2017 #Figure out how to make this dynamic

  ################################################################
  # Bird Count Data
  ################################################################
  bfiles <- read.csv(system.file("data-import","state-dir.csv",package="bbsBayes"))

  if (!isTRUE(quiet))
  {
    pb <- progress_bar$new(
      format = "Downloading count data   [:bar] :percent eta: :eta",
      clear = FALSE,
      total = nrow(bfiles) + 2,
      width = 80)
    pb$tick(0)
  }

  for(st1 in bfiles$File.Name)
  {
    if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
    st <- substring(st1,first = 1,last = nchar(st1)-4)
    temp <- tempfile()
    download.file(paste0(base_url, "States/", st, ".zip"),temp, quiet = TRUE)
    data <- read.csv(unz(temp, paste0(st,".csv")),stringsAsFactors = F)
    unlink(temp)

    if(st1 == bfiles$File.Name[1])
    {
      bird <- data
    }
    else
    {
      bird <- rbind(bird,data)
    }
  }

  names(bird)[which(tolower(names(bird)) == "countrynum")] = "countrynum"; if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
  names(bird)[which(tolower(names(bird)) == "statenum")] = "statenum"; if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}

  ################################################################
  # Route List Data
  ################################################################
  if (!isTRUE(quiet))
  {
    pb <- progress_bar$new(
      format = "Downloading route data   [:bar] :percent eta: :eta",
      clear = FALSE,
      total = 17,
      width = 80)
    pb$tick(0)
  }

  temp <- tempfile(); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
  download.file(paste0(base_url,"routes.zip"),temp, quiet = TRUE); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
  routes <- read.csv(unz(temp, paste0("routes.csv")),stringsAsFactors = F); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
  unlink(temp); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}

  #removes the off-road and water routes, as well as non-random and mini-routes
  routes <- routes[which(routes$RouteTypeDetailID == 1 & routes$RouteTypeID == 1),]; if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
  routes$Stratum <- NULL

  ################################################################
  # Weather Data
  ################################################################

  temp <- tempfile(); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
  download.file(paste0(base_url,"Weather.zip"),temp, quiet = TRUE); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
  weather <- read.csv(unz(temp, paste0("weather.csv")),stringsAsFactors = F); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}
  unlink(temp); if (!isTRUE(quiet)){pb$tick()}

  #removes the off-road and water routes, as well as non-random and mini-routes
  weather <- weather[which(weather$RunType == 1),]; if (!isTRUE(quiet)){pb$tick()}

  # merge weather and routes
  # removes some rows from weather that are associated with the removed
  #   routes above (mini-routes etc.)
  route <- merge(routes, weather, by = c("CountryNum","StateNum","Route")); if (!isTRUE(quiet)){pb$tick()}
  names(route)[which(tolower(names(route)) == "countrynum")] = "countrynum"; if (!isTRUE(quiet)){pb$tick()}
  names(route)[which(tolower(names(route)) == "statenum")] = "statenum"; if (!isTRUE(quiet)){pb$tick()}

  # Add region and BCR information to route and bird data frames
  regs <- read.csv(system.file("data-import",
                               "regs.csv",
                               package="bbsBayes"),
                   stringsAsFactors = F)
  if (!isTRUE(quiet)){pb$tick()}

  route <- merge(route, regs, by = c("countrynum", "statenum")); if (!isTRUE(quiet)){pb$tick()}
  tmp <- unique(route[,c("BCR","statenum","Route","countrynum")]); if (!isTRUE(quiet)){pb$tick()} # all unique routes by BCR and state
  bird <- merge(bird, tmp, by = c("statenum","Route","countrynum")); if (!isTRUE(quiet)){pb$tick()}

  ################################################################
  # Species Data
  ################################################################

  if (!isTRUE(quiet))
  {
    pb <- progress_bar$new(
      format = "Downloading species data [:bar] :percent eta: :eta",
      clear = FALSE,
      total = 6,
      width = 80)
    pb$tick(0)
  }

  temp <- tempfile(); if (!isTRUE(quiet)){pb$tick()}
  download.file(paste0(base_url,"SpeciesList.txt"),temp, quiet = TRUE); if (!isTRUE(quiet)){pb$tick()}
  species <- read.fwf(temp, skip = 9, strip.white = T,
                      colClasses = c("integer",
                                     "character",
                                     "character",
                                     "character",
                                     "character",
                                     "character",
                                     "character",
                                     "character",
                                     "character"),
                      header = F,
                      widths = c(6,-1,5,-1,50,-1,50,-1,50,-1,50,-1,50,-1,50,-1,50)); if (!isTRUE(quiet)){pb$tick()}
  unlink(temp); if (!isTRUE(quiet)){pb$tick()}

  names(species) <- c("seq","aou","english","french","spanish","order","family","genus","species"); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}

  # this reads in the USGS BBS ftp site species file
  species[,"sp.bbs"] <- as.integer(as.character(species[,"aou"])); if (!isTRUE(quiet)){if (!isTRUE(quiet)){pb$tick()}}

  return(list(bird = bird,
              route = route,
              species = species))
}
