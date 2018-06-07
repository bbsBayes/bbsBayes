#' Bird Species Counts by Route
#'
#' A dataset containing counts of individual bird species
#' by route per year.
#'
#' Please use the following citation for BBS data:
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R. Hudson.
#' 2018. North American Breeding Bird Survey Dataset 1966 - 2017,
#' version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research
#' Center. https://doi.org/10.5066/F76972V8.
#'
#'@format A data frame with 6189135 rows and 20 variables:
#'\describe{
#'  \item{statenum}{Numerical representation of political state.}
#'  \item{Route}{Breeding Bird Survey route number.}
#'  \item{countrynum}{Numerical representation of country}
#'  \item{RPID}{}
#'  \item{Year}{Year of the given count.}
#'  \item{Aou}{Numerical species code as assigned by American Ornithological
#'    Union}
#'  \item{count10}{}
#'  \item{count20}{}
#'  \item{count30}{}
#'  \item{count40}{}
#'  \item{count50}{}
#'  \item{StopTotal}{}
#'  \item{SpeciesTotal}{}
#'  \item{sp.bbas}{Numerical species code as assigned by BBS}
#'  \item{BCR}{Numerical representation of geopolitical stratum.}
#'  \item{route}{Breeding Bird Survey route number.}
#'  \item{state}{Numerical representation of political state.}
#'  \item{rt.uni}{Combined state-route}
#'  \item{rt.uni.y}{Combined state-route-year}
#'  \item{runyear}{Year of the route run.}
#'}
"birds"

#' Count Summaries by Species
#'
#' A dataset containing summary totals for each BBS species.
#'
#' Please use the following citation for BBS data:
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R. Hudson.
#' 2018. North American Breeding Bird Survey Dataset 1966 - 2017,
#' version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research
#' Center. https://doi.org/10.5066/F76972V8.
#'
#'@format A data frame with 663 rows and 7 variables:
#'\describe{
#'  \item{sp}{Numerical representation of species assigned by BBS.}
#'  \item{eng}{English name of the bird species.}
#'  \item{nprov.all}{}
#'  \item{nroute.all}{}
#'  \item{nyear.all}{Number of years that this species has been counted.}
#'  \item{nrouteyears.all}{}
#'  \item{rungroup}{Numerical values from 1-5 that allow species to
#'    be run in batches in desired.}
#'}
"datacount.sp"

#' Yearly Route Information
#'
#' A dataset containing information about each route run per year.
#'
#' Please use the following citation for BBS data:
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R. Hudson.
#' 2018. North American Breeding Bird Survey Dataset 1966 - 2017,
#' version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research
#' Center. https://doi.org/10.5066/F76972V8.
#'
#'@format A data frame with 107996 rows and 35 variables:
#'\describe{
#'  \item{countrynum}{Numerical representation of country}
#'  \item{statenum}{Numerical representation of political state.}
#'  \item{Route}{Breeding Bird Survey route number.}
#'  \item{RouteName}{Name of the BBS route.}
#'  \item{Active}{Numerical representation of whether the route is currently active:
#'    0 for inactive, 1 for active.}
#'  \item{Latitude}{Latitude of the route.}
#'  \item{Longitude}{Longitude of the route.}
#'  \item{Stratum}{Stratum number that contains this route.}
#'  \item{BCR}{Numerical representation of geopolitical stratum.}
#'  \item{LandTypeID}{Type of land that occurs on this route.}
#'  \item{RouteTypeID}{Type of BBS route.}
#'  \item{RouteTypeDetailID}{}
#'  \item{RPID}{}
#'  \item{Year}{Year of the given count.}
#'  \item{Month}{Numerical month of the given count.}
#'  \item{Day}{Calendar day of the given count.}
#'  \item{ObsN}{Unique number corresponding to the observer that conducted the route.}
#'  \item{TotalSpp}{Total species observed on the route for the run.}
#'  \item{StartTemp}{Temperature at the start of the route run.}
#'  \item{EndTemp}{Temperature at the end of the route run.}
#'  \item{TempScale}{Scale used for temperature report.}
#'  \item{StartWind}{Wind speed at the start of the route run in UNITS.}
#'  \item{EndWind}{Wind speed at the end of the route run in UNITS.}
#'  \item{StartSky}{Numeric representation of cloud cover at the start of the route run.}
#'  \item{EndSky}{Numeric representation of cloud cover at the end of the route run.}
#'  \item{StartTime}{Time the route run was started.}
#'  \item{EndTime}{The the route run was ended.}
#'  \item{Assistant}{Whether an assistant was used, 1 = YES, 0 = NO, NULL = NA}
#'  \item{RunType}{Type of the run.}
#'  \item{route}{Breeding Bird Survey route number.}
#'  \item{state}{Numerical representation of political state.}
#'  \item{rt.uni}{Combined state-route}
#'  \item{rt.uni.y}{Combined state-route-year}
#'  \item{runyear}{Year of the route run.}
#'}
"route"

#' Species Information
#'
#' A dataset containing information on all speices covered
#' by the American Ornithological Union
#'
#' Please use the following citation for BBS data:
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R. Hudson.
#' 2018. North American Breeding Bird Survey Dataset 1966 - 2017,
#' version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research
#' Center. https://doi.org/10.5066/F76972V8.
#'
#'@format A data frame with 748 rows and 10 variables:
#'\describe{
#'  \item{seq}{}
#'  \item{aou}{Numerical code for the given species as assigned by
#'    American Ornithological Union}
#'  \item{english}{English name of the bird species.}
#'  \item{french}{French name of the bird species.}
#'  \item{spanish}{Spanish name of the bird species.}
#'  \item{order}{Taxonomic order name of the bird species.}
#'  \item{family}{Taxonomic family name of the bird species.}
#'  \item{genus}{Taxonomic genus name of the bird species.}
#'  \item{species}{Taxonomic species name of the bird species.}
#'  \item{sp.bbs}{Numerical code for the given species as assigned by
#'    Breeding Bird Survey.}
#'}
"sp"

#' Strata Information
#'
#' A dataset containing information about all BBS strata.
#'
#' Please use the following citation for BBS data:
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R. Hudson.
#' 2018. North American Breeding Bird Survey Dataset 1966 - 2017,
#' version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research
#' Center. https://doi.org/10.5066/F76972V8.
#'
#'@format A data frame with 201 rows and 13 variables:
#'\describe{
#'  \item{State}{Full name of the state or province.}
#'  \item{country}{Two-letter country code.}
#'  \item{prov}{Two letter state or province code.}
#'  \item{St_12}{Combined two letter codes of country-province/state-
#'    bcr number}
#'  \item{bcr}{Numerical representation of geopolitical stratum.}
#'  \item{SUM_Count_}{}
#'  \item{MIN_BCR}{}
#'  \item{Area}{Area of the stratum.}
#'  \item{St_num}{}
#'  \item{strat.name}{Name of the stratum.}
#'  \item{countrynum}{Numerical representation of the country.}
#'  \item{RegionCode}{Numerical representation of the region.}
#'  \item{state}{}
#'}
"st.areas"

#' Midyear of BBS Counts
#'
#' The middle year of BBS counts, the first count being in 1966 up to
#' this data set's year of 2016.
#'
#' Please use the following citation for BBS data:
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding and M.-A.R. Hudson.
#' 2018. North American Breeding Bird Survey Dataset 1966 - 2017,
#' version 2017.0. U.S. Geological Survey, Patuxent Wildlife Research
#' Center. https://doi.org/10.5066/F76972V8.
#'
#'@format Integer:
#'\describe{
#'  \item{midyear}{Floor of the mid year between the first count year and
#'   current count year, i.e. floor((Current Year + 1966)/2)}
#'}
"midyear"
