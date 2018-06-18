speciesDataPrep <- function(sp.eng,
                            modelName,
                            dir.spsp,
                            birds,
                            route,
                            st.areas,
                            species,
                            nknots = 9)
{

  dta <- bugsdataprep(sp.eng = sp.eng,
                      sp.aou = getSpeciesAOU(species, sp.eng),
                      dir.spsp = dir.spsp, outdata = T,
                      minNRoutes = 3,# require 3 or more routes where species has been observed
                      minMaxRouteYears = 3,# require at least 1 route with non-zero obs of species in 3 or more years
                      minMeanRouteYears = 1,
                      birds = birds, route = route,
                      st.areas = st.areas)# require an average of 1 year per route with the species observed (setting this to 1 effectively removes this criterion)

  spsp.f <- dta$output
  a.wts <- dta$a.wts
  pR.wts <- dta$pR.wts
  pR <- dta$pR
  pR2 <- dta$pR2
  nobservers <- as.integer(dta$nobservers)

  ymin = range(spsp.f$year)[1]
  ymax = range(spsp.f$year)[2]
  nyears = length(ymin:ymax)

  recenter = floor(diff(c(1,ymax))/2)
  rescale = 10 # this generates a year variable with sd ~ 1 because of the ~50 years in the time-series
  spsp.f$yearscale = (spsp.f$year-recenter)/rescale

  scaledyear = seq(min(spsp.f$yearscale),max(spsp.f$yearscale),length = nyears)
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

  nstrata=length(unique(spsp.f$strat))

  toReturn <- list(ncounts = nrow(spsp.f),
                   nstrata=length(unique(spsp.f$strat)),
                   ymin = ymin,
                   ymax = ymax,
                   nonzeroweight = pR.wts$p.r.ever,
                   count = as.integer(spsp.f$count),
                   strat = as.integer(spsp.f$strat),
                   obser = as.integer(spsp.f$obser),
                   year = spsp.f$year,
                   firstyr = spsp.f$firstyr,
                   nobservers = nobservers)

  if (tolower(modelName) == "standard")
  {
    toReturn <- c(toReturn,
                   list(fixedyear = median(unique(birds$Year))))
  }

  if (tolower(modelName) == "gam")
  {
    knotsX<- seq(yminsc,ymaxsc,length=(nknots+2))[-c(1,nknots+2)]
    X_K<-(abs(outer(seq(yminsc,ymaxsc,length = nyears),knotsX,"-")))^3
    X_OMEGA_all<-(abs(outer(knotsX,knotsX,"-")))^3
    X_svd.OMEGA_all<-svd(X_OMEGA_all)
    X_sqrt.OMEGA_all<-t(X_svd.OMEGA_all$v  %*% (t(X_svd.OMEGA_all$u)*sqrt(X_svd.OMEGA_all$d)))
    X.basis<-t(solve(X_sqrt.OMEGA_all,t(X_K)))

    toReturn <- c(toReturn, list(nknots = nknots, X.basis = X.basis))
  }

  return(toReturn)
}

