bugs_data_prep <- function(sp_eng = sp_eng,
                         sp_aou = sp_aou,
                         strata_rem = NA,
                         min_n_routes = 3,
                         min_max_route_years = 5,
                         min_mean_route_years = 3, birds = birds,
                         route = route) {


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

  spsp_routes_ever <- unique(spsp.c[which(spsp.c$TotalInd != 0),c("strat_name","rt.uni")]) #routes which have had at least 1 species counted
  spsp_routes_never <- unique(spsp.c[-which(spsp.c$rt.uni %in% spsp_routes_ever$rt.uni),c("strat_name","rt.uni")]) #routes that have not had this species before
  spsp.c2 <- spsp.c[which(spsp.c$rt.uni %in% spsp_routes_ever$rt.uni),] #All data counts for routes which has had seen

  # first year each given route was run
  miny.rt <- tapply(spsp.c2[,"Year"],spsp.c2[,"rt.uni"],min)
  miny.df <- data.frame(rt.uni = names(miny.rt),fyr_rt_run = as.integer(miny.rt))

  # number of times the species was seen on the given route since it has run
  n.yr.rt <- tapply(spsp.c[which(spsp.c$TotalInd != 0),"Year"],spsp.c[which(spsp.c$TotalInd != 0),"rt.uni"],length)
  n.yr.df <- data.frame(rt.uni = names(n.yr.rt),nyr_rt_run = as.integer(n.yr.rt))

  if(nrow(spsp_routes_ever) > 0)
  {
    spsp_routes_ever <- merge(spsp_routes_ever,miny.df,by = "rt.uni")
    spsp_routes_ever <- merge(spsp_routes_ever,n.yr.df,by = "rt.uni")

    # this will give some ever/never stats PER STRATUM
    pR <- data.frame(strat = unique(spsp.c$strat_name), nr.ever = NA, nr.never = NA, p.r.ever = NA,nry.ever = NA,meanry.ever = NA)
    for(p in unique(spsp.c$strat_name))
    {
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

    spsp.c.l <- spsp.c[which(spsp.c$rt.uni %in% unique(spsp_routes_ever$rt.uni)),]
    spsp.c.drop <- spsp.c[-which(spsp.c$rt.uni %in% unique(spsp_routes_ever$rt.uni)),]

    spsp.c <- spsp.c.l
    spsp.2 <- spsp.c
    spsp.2 <- spsp.2[which(spsp.2$strat_name %in% pR[which(pR$nr.ever >= min_n_routes & pR$max.nry >= min_max_route_years & pR$meanry.ever >= min_mean_route_years),"strat"]),]

    rts.used <- unique(spsp.2[,c("rt.uni","strat_name")])

    #get number of routes used per strata
    #incidentally this ends up being all the strata that are used
    rts.summary <- tapply(rts.used$rt.uni,rts.used$strat_name,length)
    nrts.used <- data.frame(strat_name = names(rts.summary),nroutes.used = as.integer(rts.summary))

    spsp.temp.2 <- merge(spsp.2,pR[,c("strat","p.r.ever","meanry.ever","max.nry")], by.x = "strat_name", by.y = "strat",all.x = T)

    spsp.2 <- spsp.temp.2

    spsp.2[,"yr"] <- (spsp.2[,"Year"]+1)-min(spsp.2[,"Year"])

    spsp.2[,"count"] <- spsp.2[,c("TotalInd")]

    spsp_f <- spsp.2

  }else
  {
    spsp_f <- spsp.c[-c(1:nrow(spsp.c)),]
  }

  if(nrow(spsp_f) > 0)
  {
    spsp_f[,"stratcode"] <- spsp_f[,"strat_name"]
    spsp_f[,"stratum"] <- as.numeric(factor(spsp_f$strat_name))

    spsp_f <- spsp_f[order(spsp_f$stratum,spsp_f$Route,spsp_f$Year),]

    strat.list <- unique(spsp_f[,c("statenum","countrynum","BCR","stratum","strat_name","stratcode","State")])
    strat.use <- strat.list[,"stratcode"]

    pR2 <- merge(pR[which(pR$strat %in% strat.list[,"strat_name"]),],strat.list,by.x = "strat", by.y = "strat_name")

    pR2 <- pR2[order(pR2$stratum),]

    spsp_f <- spsp_f[order(spsp_f$stratum,spsp_f$Route,spsp_f$Year),]

    spsp_f[,"rt.uni.ob"] <- paste(spsp_f$statenum,spsp_f$Route,spsp_f$ObsN,sep = "-")

    strata.adj <-  pR2[,"stratcode"]

    pR.wts <- unique(spsp_f[,c("strat_name","p.r.ever")])

    tmp <- unique(spsp_f[,c("stratum","rt.uni.ob")])

    # number of observers per stratum
    nobservers <- tapply(tmp$rt.uni.ob,tmp$stratum,length)

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

    spsp_ft <- merge(spsp_f,obsfact,by = c("stratum","rt.uni.ob"))

    spsp_f <- spsp_ft

    fyears <- tapply(spsp_f$Year,spsp_f$rt.uni.ob,min)
    fyr.df <- data.frame(rt.uni.ob = names(fyears),Year = as.integer(fyears),firstyear = 1)
    spsp_ft <- merge(spsp_f,fyr.df,all.x = T,by = c("rt.uni.ob","Year"))

    spsp_f <- spsp_ft

    spsp_f[which(is.na(spsp_f$firstyear)),"firstyear"] <- 0
    output <- spsp_f[,c("count","stratum","obser","yr","firstyear","strat_name","rt.uni","Year")]
    names(output) <- c("count","strat","obser","year","firstyr","strat_name","route","rYear")

    return(list(output = output, pR_wts = pR.wts, pR = pR, pR2 = pR2, n_observers = nobservers, nrts_used = nrts.used)) #adjmat = adjmat,LT5.strata = LT5.strata, num = num, adj = adj, sumNumNeigh = sumNumNeigh
  }else
  {
    output <- spsp_f
    return(list(output = output))
  }

}
