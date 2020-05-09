#' Sample point count data per species (Pacific Wren only)
#'
#' A sample dataset containing 10-stop counts of each bird species seen per route per year.
#'   NOTE: This only contains data for Pacific Wren, not the full data set.
#'   The full count set is obtained via the function \code{fetch_bbs_data}.
#'   The data is obtained from the United States Geological Survey and is
#'   subject to change as new data is added each year. See Details for citation.
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding, V. Aponte and M-A.R. Hudson. 2019.
#'   North American Breeding Bird Survey Dataset 1966 - 2018, version 2018.0.
#'   U.S. Geological Survey, Patuxent Wildlife Research Center. https://doi.org/10.5066/P9HE8XYJ.
#'
#' @format A data frame with 20 rows and 15 variables:
#' \describe{
#'   \item{statenum}{Numerical representation of state or province}
#'   \item{Route}{Numerican representation of the route the species was observed}
#'   \item{countrynum}{Numerical representation of the country}
#'   \item{RouteDataID}{Unique code for each year a route was run}
#'   \item{RPID}{Run Protocol ID}
#'   \item{Year}{Year the species was observed on the route}
#'   \item{AOU}{Numerical representation of the species, designated by American Ornithological Union}
#'   \item{Count10}{Counts for stops 1-10}
#'   \item{Count20}{Counts for stops 11-20}
#'   \item{Count30}{Counts for stops 21-30}
#'   \item{Count40}{Counts for stops 31-40}
#'   \item{Count50}{Counts for stops 41-50}
#'   \item{StopTotal}{Count for all stops}
#'   \item{SpeciesTotal}{Total count for the species on the route run}
#'   \item{BCR}{Bird Conservation Region the route was run in}
#' }
#'
"bird_sample"

#' Sample route data per year run (Pacific Wren only)
#'
#' A dataset containing data for each route run per year.
#'   NOTE: This only contains data for Pacific Wren, not the full data set.
#'   The full count set is obtained via the function \code{fetch_bbs_data}.
#'   The data is obtained from the United States Geological Survey and is
#'   subject to change as new data is added each year. See Details for citation.
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding, V. Aponte and M-A.R. Hudson. 2019.
#'   North American Breeding Bird Survey Dataset 1966 - 2018, version 2018.0.
#'   U.S. Geological Survey, Patuxent Wildlife Research Center. https://doi.org/10.5066/P9HE8XYJ.
#'
#' @format A data frame with 20 rows and 32 variables:
#' \describe{
#'   \item{countrynum}{Numerical representation of the country}
#'   \item{statenum}{Numerical representation of state or province}
#'   \item{Route}{Numerican representation of the route the species was observed}
#'   \item{RouteName}{Name of the route, represented as a string}
#'   \item{Active}{Boolean 0 or 1 as to whether the route is currently active}
#'   \item{Latitude}{Latitude of the start of the route}
#'   \item{Longitude}{Longitude of the start of the route}
#'   \item{BCR}{What bird conservation region is the route in}
#'   \item{RouteTypeID}{Type of the route, only 1 is acceptable}
#'   \item{RouteTypeDetailID}{Route type detail ID}
#'   \item{RouteDataID}{Unique code for each year a route was run}
#'   \item{RPID}{Run Protocol ID}
#'   \item{Year}{Year the route was run}
#'   \item{Month}{Month the route was run}
#'   \item{Day}{Day the route was run}
#'   \item{ObsN}{Unique number for the observer on the route}
#'   \item{TotalSpp}{Total species observed on the route}
#'   \item{StartTemp}{Temperature at the start of the route}
#'   \item{EndTemp}{Temperature at the end of the route}
#'   \item{TempScale}{(C)elsius or (F)arenheit}
#'   \item{StartWind}{Wind type at the beginning of the route}
#'   \item{EndWind}{Wind type at the end of the route}
#'   \item{StartSky}{Sky conditions at the start of the route}
#'   \item{EndSky}{Sky conditions at the end of the route}
#'   \item{StartTime}{Time the route was started}
#'   \item{EndTime}{Time the route was ended}
#'   \item{Assistant}{Boolean 0 or 1 as to whether an assistant was used}
#'   \item{QualityCurrentID}{Quality current ID}
#'   \item{RunType}{Type of BBS route run. Only acceptable run type is 1}
#'   \item{State}{String representation of state or province}
#'   \item{St_Abrev}{Abbreviated state or province}
#'   \item{Country}{Abbreviated country}
#' }
#'
"route_sample"

#' Sample North American bird species list (Pacific Wren only)
#'
#' A dataset containing species list of North America.
#'   NOTE: This only contains data for Pacific Wren, not the full data set.
#'   The full count set is obtained via the function \code{fetch_bbs_data}.
#'   The data is obtained from the United States Geological Survey and is
#'   subject to change as new data is added each year. See Details for citation.
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding, V. Aponte and M-A.R. Hudson. 2019.
#'   North American Breeding Bird Survey Dataset 1966 - 2018, version 2018.0.
#'   U.S. Geological Survey, Patuxent Wildlife Research Center. https://doi.org/10.5066/P9HE8XYJ.
#'
#' @format A data frame with 20 rows and 10 variables:
#' \describe{
#'   \item{seq}{Sequence - USGS use}
#'   \item{aou}{Numerical representation of the species, designated by American Ornithological Union}
#'   \item{english}{Species name in English}
#'   \item{order}{Taxonomic order}
#'   \item{family}{Taxonomic family}
#'   \item{genus}{Taxonomic genus}
#'   \item{species}{Taxonomic species}
#'   \item{sp.bbs}{Same as aou, no leading 0}
#' }
#'
"species_sample"
