#' Fetch Breeding Bird Survey dataset
#'
#' \code{fetchBBSdata} uses File Transfer Protocol (FTP) to fetch Breeding Bird
#'  Survey data from the United States Geological Survey (USGS) FTP site. This
#'  is the raw data that is uploaded to the site before any analyses are performed.
#'
#' @return Large list (5 elements) containing point count data, route data, weather
#'    data, and species data. This is a very large file.
#' @export
#'
fetchBBSdata <- function()
{
  baseURL <- "ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/"
  version <- 2017 #Figure out how to make this dynamic

  ################################################################
  # Bird Count Data
  ################################################################
  bfiles <- read.csv(system.file("data-import","state-dir.csv",package="bbsBayes"))
  pb <- progress_bar$new(
    format = "Downloading count data   [:bar] :percent eta: :eta",
    clear = FALSE,
    total = nrow(bfiles),
    width = 80)
  pb$tick(0)

  for(st1 in bfiles$File.Name)
  {
    pb$tick()
    st <- substring(st1,first = 1,last = nchar(st1)-4)
    temp <- tempfile()
    download.file(paste0(baseURL, "States/", st, ".zip"),temp, quiet = TRUE)
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

  ################################################################
  # Route List Data
  ################################################################
  pb <- progress_bar$new(
    format = "Downloading route data   [:bar] :percent eta: :eta",
    clear = FALSE,
    total = 5,
    width = 80)
  pb$tick(0)

  temp <- tempfile(); pb$tick()
  download.file(paste0(baseURL,"routes.zip"),temp, quiet = TRUE); pb$tick()
  routes <- read.csv(unz(temp, paste0("routes.csv")),stringsAsFactors = F); pb$tick()
  unlink(temp); pb$tick()

  #removes the off-road and water routes, as well as non-random and mini-routes
  routes <- routes[which(routes$RouteTypeDetailID == 1 & routes$RouteTypeID == 1),]; pb$tick()


  ################################################################
  # Weather Data
  ################################################################
  pb <- progress_bar$new(
    format = "Downloading weather data [:bar] :percent eta: :eta",
    clear = FALSE,
    total = 6,
    width = 80)
  pb$tick(0)

  temp <- tempfile(); pb$tick()
  download.file(paste0(baseURL,"Weather.zip"),temp, quiet = TRUE); pb$tick()
  weather <- read.csv(unz(temp, paste0("weather.csv")),stringsAsFactors = F); pb$tick()
  unlink(temp); pb$tick()

  #removes the off-road and water routes, as well as non-random and mini-routes
  weather <- weather[which(weather$RunType == 1),]; pb$tick()

  # merge weather and routes
  # removes some rows from weather that are associated with the removed
  #   routes above (mini-routes etc.)
  route <- merge(routes, weather, by = c("CountryNum","StateNum","Route")); pb$tick()

  ################################################################
  # Species Data
  ################################################################
  pb <- progress_bar$new(
    format = "Downloading species data [:bar] :percent eta: :eta",
    clear = FALSE,
    total = 4,
    width = 80)
  pb$tick(0)

  temp <- tempfile(); pb$tick()
  download.file(paste0(baseURL,"SpeciesList.txt"),temp, quiet = TRUE); pb$tick()
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
                      width = c(6,-1,5,-1,50,-1,50,-1,50,-1,50,-1,50,-1,50,-1,50)); pb$tick()
  unlink(temp); pb$tick()

  return(list(bird = bird,
              route = route,
              routes = routes,
              weather = weather,
              species = species))
}
