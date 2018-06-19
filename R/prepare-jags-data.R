#' Transform data to use for JAGS input
#'
#' \code{prepare_jags_data} transforms stratified data for use as input
#'   to run JAGS models.
#'
#' @param data_strat Large list of stratified data returned by \code{stratify()}
#' @param species_to_run Character string of the English name of the species to run
#' @param model Character strings or vector of character strings of what
#'   species are wanting to be analysed.
#' @param n_knots Number of knots to be used in GAM function
#'
#' @return Large list of data to be used in JAGS
#'
#' @export
#'
#' @examples
#' data.jags <- prepareJAGSdata(data_strat, species_to_run = "Barn Swallow", model = "standard")
#' data.jags <- prepareJAGSdata(stratify(bbs.data), species_to_run = "Bufflehead", model = "gam", n_knots = 9)
#'
prepare_jags_data <- function(data_strat,
                            species_to_run,
                            model,
                            output_dir,
                            n_knots = 9)
{
  birds <- data_strat$bird_strat
  route <- data_strat$route_strat
  species <- data_strat$species_strat
  st_areas <- data_strat$strata

  dta <- bugs_data_prep(sp_eng = species_to_run,
                      sp_aou = get_species_aou(species, species_to_run),
                      dir_spsp = output_dir, outdata = T,
                      min_n_routes = 3,# require 3 or more routes where species has been observed
                      min_max_route_years = 3,# require at least 1 route with non-zero obs of species in 3 or more years
                      min_mean_route_years = 1,
                      birds = birds, route = route,
                      st_areas = st_areas)# require an average of 1 year per route with the species observed (setting this to 1 effectively removes this criterion)

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

  to_return <- list(ncounts = nrow(spsp_f),
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
