#' Transform data to use for JAGS input
#'
#' \code{prepare_jags_data} transforms stratified data for use as input
#'   to run JAGS models.
#'
#' @param strat_data Large list of stratified data returned by \code{stratify()}
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
#' @param quiet Should progress bars be suppressed?
#' @param ... Additional arguments
#'
#' @return List of data to be used in JAGS, including:
#'   \item{model}{The model to be used in JAGS}
#'   \item{ncounts}{The number of counts containing useful data for the species}
#'   \item{nstrata}{The number of strata used in the analysis}
#'   \item{ymin}{Minimum year used}
#'   \item{ymax}{Maximum year used}
#'   \item{nonzeroweight}{Proportion of routes in each strata with species obervation}
#'   \item{count}{Vector of counts for the species}
#'   \item{strat}{Vector of strata to be used in the analysis}
#'   \item{osber}{Vector of observers}
#'   \item{year}{Vector of years for each count}
#'   \item{firstyr}{Vector of indicator variables as to whether an observer was a first year}
#'   \item{nobservers}{Total number of observers}
#'   \item{fixedyear}{Median of all years, included only with standard model}
#'   \item{nknots}{Number of knots to use for smooting functions, included only with GAM}
#'   \item{X.basis}{Basis function for n smoothing functions, included only with GAM}
#'
#' @importFrom stats median
#' @importFrom progress progress_bar
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

prepare_jags_data <- function(strat_data,
                            species_to_run,
                            model,
                            n_knots = 9,
                            min_n_routes = 3,
                            min_max_route_years = 3,
                            min_mean_route_years = 1,
                            strata_rem = NA,
                            quiet = FALSE,
                            ...)
{
  birds <- strat_data$bird_strat
  route <- strat_data$route_strat
  species <- strat_data$species_strat


  ##################################################################
  # Previous function from Bugs Data Prep
  ##################################################################
  sp_eng <- species_to_run
  sp_aou <- get_species_aou(species, species_to_run)

  spsp <- birds[which(birds$AOU == sp_aou),] # Gets all observations of the species of interest
  names(spsp)[which(names(spsp) == "SpeciesTotal")] <- "TotalInd" # change header to totalInd

  spsp.c <- merge(route,spsp[,-which(names(spsp) %in% c("countrynum",
                                                        "rt.uni.y",
                                                        "rt.uni",
                                                        "RPID"))],
                  by = c("statenum",
                         "Route",
                         "Year",
                         "BCR"),all.x = T)

  spsp.c[which(is.na(spsp.c$TotalInd)),"TotalInd"] <- 0

  if (!is.na(strata_rem)) {spsp.c <- spsp.c[-which(spsp.c$strat_name %in% strata_rem),] }

  if (!isTRUE(quiet))
  {
    pb <- progress_bar$new(
      format = "Preparing JAGS data   [:bar] :percent eta: :eta",
      clear = FALSE,
      total = length(unique(spsp.c$strat_name)) + 26,
      width = 80)
    pb$tick(0)
  }

  spsp_routes_ever <- unique(spsp.c[which(spsp.c$TotalInd != 0),c("strat_name","rt.uni")]) #routes which have had at least 1 species counted
  spsp_routes_never <- unique(spsp.c[-which(spsp.c$rt.uni %in% spsp_routes_ever$rt.uni),c("strat_name","rt.uni")]) #routes that have not had this species before
  spsp.c2 <- spsp.c[which(spsp.c$rt.uni %in% spsp_routes_ever$rt.uni),] #All data counts for routes which has had seen
  if (!isTRUE(quiet)){pb$tick()}

  # first year each given route was run
  miny.rt <- tapply(spsp.c2[,"Year"],spsp.c2[,"rt.uni"],min)
  miny.df <- data.frame(rt.uni = names(miny.rt),fyr_rt_run = as.integer(miny.rt))
  if (!isTRUE(quiet)){pb$tick()}

  # number of times the species was seen on the given route since it has run
  n.yr.rt <- tapply(spsp.c[which(spsp.c$TotalInd != 0),"Year"],spsp.c[which(spsp.c$TotalInd != 0),"rt.uni"],length)
  n.yr.df <- data.frame(rt.uni = names(n.yr.rt),nyr_rt_run = as.integer(n.yr.rt))
  if (!isTRUE(quiet)){pb$tick()}

  if(nrow(spsp_routes_ever) > 0)
  {
    spsp_routes_ever <- merge(spsp_routes_ever,miny.df,by = "rt.uni")
    spsp_routes_ever <- merge(spsp_routes_ever,n.yr.df,by = "rt.uni")
    if (!isTRUE(quiet)){pb$tick()}

    # this will give some ever/never stats PER STRATUM
    pR <- data.frame(strat = unique(spsp.c$strat_name), nr.ever = NA, nr.never = NA, p.r.ever = NA,nry.ever = NA,meanry.ever = NA)
    for(p in unique(spsp.c$strat_name))
    {
      if (!isTRUE(quiet)){pb$tick()}
      # routes ever observed in this strata
      pR[pR$strat == p,"nr.ever"] <- nrow(spsp_routes_ever[spsp_routes_ever$strat_name == p,])
      # routes never observed in this strata
      pR[pR$strat == p,"nr.never"] <- nrow(spsp_routes_never[spsp_routes_never$strat_name == p,])

      # proportion of routes that have ever observed by stratum
      pR[pR$strat == p,"p.r.ever"] <-  pR[pR$strat == p,"nr.ever"]/(pR[pR$strat == p,"nr.ever"]+ pR[pR$strat == p,"nr.never"])

      # total counts that have included this species (ex: 5 routes in year 1, 4 in year 2 = 9 overall)
      pR[pR$strat == p,"nry.ever"] <-  length(spsp.c[which(spsp.c$strat_name == p & spsp.c$TotalInd > 0),"rt.uni.y"])

      pR[pR$strat == p,"fy.wspecies"] <-  suppressWarnings(min(spsp_routes_ever[spsp_routes_ever$strat_name == p,"fyr_rt_run"]))
      pR[pR$strat == p,"max.nry"] <-  suppressWarnings(max(spsp_routes_ever[spsp_routes_ever$strat_name == p,"nyr_rt_run"]))

      pR[pR$strat == p,"meanry.ever"] <-  length(spsp.c[which(spsp.c$strat_name == p & spsp.c$TotalInd > 0),"rt.uni.y"])/pR[pR$strat == p,"nr.ever"]
    }

    pR[,"strat"] <- as.character(pR[,"strat"])

    #gets rid of infinite values
    pR[which(pR$fy.wspecies > 2100),"fy.wspecies"] <- NA
    pR[which(pR$max.nry < 0),"max.nry"] <- NA
    if (!isTRUE(quiet)){pb$tick()}

    spsp.c.l <- spsp.c[which(spsp.c$rt.uni %in% unique(spsp_routes_ever$rt.uni)),]
    spsp.c.drop <- spsp.c[-which(spsp.c$rt.uni %in% unique(spsp_routes_ever$rt.uni)),]
    if (!isTRUE(quiet)){pb$tick()}

    spsp.c <- spsp.c.l
    spsp.2 <- spsp.c
    spsp.2 <- spsp.2[which(spsp.2$strat_name %in% pR[which(pR$nr.ever >= min_n_routes & pR$max.nry >= min_max_route_years & pR$meanry.ever >= min_mean_route_years),"strat"]),]
    if (!isTRUE(quiet)){pb$tick()}

    rts.used <- unique(spsp.2[,c("rt.uni","strat_name")])

    #get number of routes used per strata
    #incidentally this ends up being all the strata that are used
    rts.summary <- tapply(rts.used$rt.uni,rts.used$strat_name,length)
    nrts.used <- data.frame(strat_name = names(rts.summary),nroutes.used = as.integer(rts.summary))
    if (!isTRUE(quiet)){pb$tick()}

    spsp.temp.2 <- merge(spsp.2,pR[,c("strat","p.r.ever","meanry.ever","max.nry")], by.x = "strat_name", by.y = "strat",all.x = T)
    if (!isTRUE(quiet)){pb$tick()}

    spsp.2 <- spsp.temp.2

    spsp.2[,"yr"] <- (spsp.2[,"Year"]+1)-min(spsp.2[,"Year"])

    spsp.2[,"count"] <- spsp.2[,c("TotalInd")]

    spsp_f <- spsp.2
    if (!isTRUE(quiet)){pb$tick()}
  }else
  {
    for(p in unique(spsp.c$strat_name) + 6)
    {
      if (!isTRUE(quiet)){pb$tick()}
    }
    spsp_f <- spsp.c[-c(1:nrow(spsp.c)),]
    if (!isTRUE(quiet)){pb$tick()}
  }

  spsp_f[,"stratcode"] <- spsp_f[,"strat_name"]
  spsp_f[,"stratum"] <- as.numeric(factor(spsp_f$strat_name)); if (!isTRUE(quiet)){pb$tick()}

  spsp_f <- spsp_f[order(spsp_f$stratum,spsp_f$Route,spsp_f$Year),]; if (!isTRUE(quiet)){pb$tick()}

  strat.list <- unique(spsp_f[,c("statenum","countrynum","BCR","stratum","strat_name","stratcode","State")])
  strat.use <- strat.list[,"stratcode"]

  pR2 <- merge(pR[which(pR$strat %in% strat.list[,"strat_name"]),],strat.list,by.x = "strat", by.y = "strat_name")
  if (!isTRUE(quiet)){pb$tick()}

  pR2 <- pR2[order(pR2$stratum),]
  if (!isTRUE(quiet)){pb$tick()}

  spsp_f <- spsp_f[order(spsp_f$stratum,spsp_f$Route,spsp_f$Year),]
  if (!isTRUE(quiet)){pb$tick()}

  spsp_f[,"rt.uni.ob"] <- paste(spsp_f$statenum,spsp_f$Route,spsp_f$ObsN,sep = "-")

  strata.adj <-  pR2[,"stratcode"]

  pR.wts <- unique(spsp_f[,c("strat_name","p.r.ever")])

  tmp <- unique(spsp_f[,c("stratum","rt.uni.ob")])

  # number of observers per stratum
  nobservers <- tapply(tmp$rt.uni.ob,tmp$stratum,length)
  if (!isTRUE(quiet)){pb$tick()}

  for (s in 1:length(nobservers)) {
    sel1 <- which(spsp_f$stratum == s)

    tmp <- unique(spsp_f[sel1,c("stratum","rt.uni.ob")])

    tmp[,"obser"] <- as.numeric(factor(tmp[,"rt.uni.ob"]))
    if(s == 1)
    {
      obsfact <- tmp
    }else
    {
      obsfact <- rbind(obsfact,tmp)
    }
  }
  if (!isTRUE(quiet)){pb$tick()}

  spsp_ft <- merge(spsp_f,obsfact,by = c("stratum","rt.uni.ob"))
  if (!isTRUE(quiet)){pb$tick()}

  spsp_f <- spsp_ft

  fyears <- tapply(spsp_f$Year,spsp_f$rt.uni.ob,min)
  fyr.df <- data.frame(rt.uni.ob = names(fyears),Year = as.integer(fyears),firstyear = 1)
  spsp_ft <- merge(spsp_f,fyr.df,all.x = T,by = c("rt.uni.ob","Year"))
  if (!isTRUE(quiet)){pb$tick()}

  spsp_f <- spsp_ft

  spsp_f[which(is.na(spsp_f$firstyear)),"firstyear"] <- 0
  spsp_f <- spsp_f[,c("count","stratum","obser","yr","firstyear","strat_name","rt.uni","Year")]
  names(spsp_f) <- c("count","strat","obser","year","firstyr","strat_name","route","rYear")
  if (!isTRUE(quiet)){pb$tick()}

  pR_wts <- pR.wts
  n_observers = nobservers
  nrts_used = nrts.used
  if (!isTRUE(quiet)){pb$tick()}

  ####################### END BUGS DATA PREP #######################

  ymin = range(spsp_f$year)[1]
  ymax = range(spsp_f$year)[2]
  nyears = length(ymin:ymax)
  if (!isTRUE(quiet)){pb$tick()}

  recenter = floor(diff(c(1,ymax))/2)
  rescale = 10 # this generates a year variable with sd ~ 1 because of the ~50 years in the time-series
  spsp_f$yearscale = (spsp_f$year-recenter)/rescale
  if (!isTRUE(quiet)){pb$tick()}

  scaledyear = seq(min(spsp_f$yearscale),max(spsp_f$yearscale),length = nyears)
  names(scaledyear) <- ymin:ymax
  if(ymin != 1)
  {
    newys = 1:(ymin-1)
    newyscale = (newys-recenter)/rescale
    names(newyscale) <- newys
    scaledyear = c(newyscale,scaledyear)
  }
  if (!isTRUE(quiet)){pb$tick()}

  yminsc = scaledyear[as.character(ymin)]
  ymaxsc = scaledyear[as.character(ymax)]
  if(ymin != 1)
  {
    yminpred = 1
    yminscpred = scaledyear[as.character(1)]
  }
  if (!isTRUE(quiet)){pb$tick()}

  nstrata=length(unique(spsp_f$strat))

  to_return <- list(model = model,
                   ncounts = nrow(spsp_f),
                   nstrata=length(unique(spsp_f$strat)),
                   ymin = ymin,
                   ymax = ymax,
                   nonzeroweight = pR_wts$p.r.ever,
                   count = as.integer(spsp_f$count),
                   strat_name = spsp_f$strat_name,
                   strat = as.integer(spsp_f$strat),
                   obser = as.integer(spsp_f$obser),
                   year = spsp_f$year,
                   firstyr = spsp_f$firstyr,
                   nobservers = n_observers,
                   stratify_by = strat_data$stratify_by)

  if (tolower(model) == "standard")
  {
    to_return <- c(to_return,
                   list(fixedyear = median(unique(birds$Year))))
  }

  if (tolower(model) %in% c("gam", "gamye"))
  {
    knotsX<- seq(yminsc,ymaxsc,length=(n_knots+2))[-c(1,n_knots+2)]
    X_K<-(abs(outer(seq(yminsc,ymaxsc,length = nyears),knotsX,"-")))^3
    X_OMEGA_all<-(abs(outer(knotsX,knotsX,"-")))^3
    X_svd.OMEGA_all<-svd(X_OMEGA_all)
    X_sqrt.OMEGA_all<-t(X_svd.OMEGA_all$v  %*% (t(X_svd.OMEGA_all$u)*sqrt(X_svd.OMEGA_all$d)))
    X_basis<-t(solve(X_sqrt.OMEGA_all,t(X_K)))

    to_return <- c(to_return, list(nknots = n_knots, X.basis = X_basis))
  }
  if (!isTRUE(quiet)){pb$tick()}

  return(to_return)
}

