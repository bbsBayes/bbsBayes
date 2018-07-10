#' Transform data to use for JAGS input
#'
#' \code{prepare_jags_data} transforms stratified data for use as input
#'   to run JAGS models.
#'
#' @param data Large list of stratified data returned by \code{stratify()}
#' @param species_to_run Character string of the English name of the species to run
#' @param model Character strings or vector of character strings of what
#'   species are wanting to be analysed.
#' @param n_knots Number of knots to be used in GAM function
#' @param min_n_routes Minimum routes per strata where species has been observed.
#'   Defaults to 3
#' @param min_max_route_years Minimum number of years with non-zero observations
#'   of species on at least 1 route. Defaults to 3
#' @param min_mean_route_years Minimum average of years per route with the
#'   species observed. Defaults to 1.
#' @param strata_rem Strata to remove from analysis. Defaults to NA
#' @param ... Additional arguments
#'
#' @return Large list of data to be used in JAGS
#'
#' @importFrom stats median
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Download BBS data and stratify it
#' bbs_data <- fetch_bbs_data()
#' stratified_data <- stratify(bbs_data)
#'
#' # Prepare the stratified data for use in a JAGS model.
#' # This particular instance prepares for the Standard BBS model.
#' data_jags <- prepare_jags_data(data = stratified_data,
#'                                species_to_run = "Spruce Grouse",
#'                                model = "standard")
#'
#' # Prepare data for use the First Difference BBS model.
#' data_jags <- prepare_jags_data(data = stratified_data,
#'                                species_to_run = "Mallard",
#'                                model = "firstdifference")
#'
#' # You can also specify the GAM model, with an optional number of
#' # knots to use for the GAM basis (defaults to 9 knots)
#' data_jags <- prepare_jags_data(data = stratified_data,
#'                                species_to_run = "Barn Swallow",
#'                                model = "gam",
#'                                n_knots = 9)
#'
#' # This function accepts French bird names
#' data_jags <- prepare_jags_data(data = stratified_data,
#'                                species_to_run = "Oie des neiges",
#'                                model = "standard")
#'
#' # Capitalization and punctuation matter (for now)
#' # This code will produce an error.
#' data_jags <- prepare_jags_data(data = stratified_data,
#'                                species_to_run = "Eastern whippoorwill"
#'                                model = "standard")
#' # But this code will be fine
#' data_jags <- prepare_jags_data(data = stratified_data,
#'                                species_to_run = "Eastern Whip-poor-will"
#'                                model = "standard")
#' }
#'

prepare_jags_data <- function(data,
                            species_to_run,
                            model,
                            n_knots = 9,
                            min_n_routes = 3,
                            min_max_route_years = 3,
                            min_mean_route_years = 1,
                            strata_rem = NA,
                            ...)
{
  birds <- data$bird_strat
  route <- data$route_strat
  species <- data$species_strat

  dta <- bugs_data_prep(sp_eng = species_to_run,
                      sp_aou = get_species_aou(species, species_to_run),
                      strata_rem = strata_rem,
                      min_n_routes = min_n_routes,# require 3 or more routes where species has been observed
                      min_max_route_years = min_max_route_years,# require at least 1 route with non-zero obs of species in 3 or more years
                      min_mean_route_years = min_mean_route_years,
                      birds = birds,
                      route = route)# require an average of 1 year per route with the species observed (setting this to 1 effectively removes this criterion)

  spsp_f <- dta$output
  a_wts <- dta$a_wts
  pR_wts <- dta$pR_wts
  pR <- dta$pR
  pR2 <- dta$pR2
  n_observers <- as.integer(dta$n_observers)

  ymin = range(spsp_f$year)[1]
  ymax = range(spsp_f$year)[2]
  nyears = length(ymin:ymax)

  recenter = floor(diff(c(1,ymax))/2)
  rescale = 10 # this generates a year variable with sd ~ 1 because of the ~50 years in the time-series
  spsp_f$yearscale = (spsp_f$year-recenter)/rescale

  scaledyear = seq(min(spsp_f$yearscale),max(spsp_f$yearscale),length = nyears)
  names(scaledyear) <- ymin:ymax
  if(ymin != 1)
  {
    newys = 1:(ymin-1)
    newyscale = (newys-recenter)/rescale
    names(newyscale) <- newys
    scaledyear = c(newyscale,scaledyear)
  }
  yminsc = scaledyear[as.character(ymin)]
  ymaxsc = scaledyear[as.character(ymax)]
  if(ymin != 1)
  {
    yminpred = 1
    yminscpred = scaledyear[as.character(1)]
  }

  nstrata=length(unique(spsp_f$strat))

  to_return <- list(model = model,
                   ncounts = nrow(spsp_f),
                   nstrata=length(unique(spsp_f$strat)),
                   ymin = ymin,
                   ymax = ymax,
                   nonzeroweight = pR_wts$p.r.ever,
                   count = as.integer(spsp_f$count),
                   strat = as.integer(spsp_f$strat),
                   obser = as.integer(spsp_f$obser),
                   year = spsp_f$year,
                   firstyr = spsp_f$firstyr,
                   nobservers = n_observers)

  if (tolower(model) == "standard")
  {
    to_return <- c(to_return,
                   list(fixedyear = median(unique(birds$Year))))
  }

  if (tolower(model) == "gam")
  {
    knotsX<- seq(yminsc,ymaxsc,length=(n_knots+2))[-c(1,n_knots+2)]
    X_K<-(abs(outer(seq(yminsc,ymaxsc,length = nyears),knotsX,"-")))^3
    X_OMEGA_all<-(abs(outer(knotsX,knotsX,"-")))^3
    X_svd.OMEGA_all<-svd(X_OMEGA_all)
    X_sqrt.OMEGA_all<-t(X_svd.OMEGA_all$v  %*% (t(X_svd.OMEGA_all$u)*sqrt(X_svd.OMEGA_all$d)))
    X_basis<-t(solve(X_sqrt.OMEGA_all,t(X_K)))

    to_return <- c(to_return, list(nknots = n_knots, X.basis = X_basis))
  }

  return(to_return)
}

