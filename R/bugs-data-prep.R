bugsdataprep <- function(sp.eng = sp.eng,
                         sp.aou = sp.aou,
                         dir.spsp = dir.spsp,
                         strata.rem = NA,
                         outdata = F,
                         minNRoutes = 3,
                         minMaxRouteYears = 5,
                         minMeanRouteYears = 3, bird = bird,
                         route = route) {


  spsp <- bird[which(bird$AOU == sp.aou),] # Gets all observations of the species of interest
  names(spsp)[which(names(spsp) == "StopTotal")] <- "TotalInd" # change header to totalInd

  # Create unique identifiers for each observation
  spsp$rt.uni.y <- paste(spsp$StateNum, spsp$Route, spsp$Year, sep = "-")
  spsp$rt.uni <- paste(spsp$StateNum, spsp$Route, sep = "-")
  route$rt.uni.y <- paste(route$StateNum, route$Route, route$Year, sep = "-")
  route$rt.uni <- paste(route$StateNum, route$Route, sep = "-")



  # create a dataframe that combines BOTH route information for ANY route a species has been seen
  #   for ALL years of a given route regardlless if the species was seen that year.
  # For years where the species was seen on that given route, keep the relevant count information
  # Otherwise, it will just be filled with NA
  spsp.c <- merge(route,spsp[,-which(names(spsp) %in% c("CountryNum",
                                                       "rt.uni.y",
                                                       "rt.uni",
                                                       "StateNum",
                                                       "Route",
                                                       "RPID",
                                                       "Year"))],
                  by = c("state",
                         "route",
                         "runyear",
                         "BCR"),all.x = T)

  # Now those NAs get turned to 0s. We have presence/absence, bitchezz
  spsp.c[which(is.na(spsp.c$TotalInd)),"TotalInd"] <- 0

  # Adds some more inormation I suppose?
  spsp.c <- merge(spsp.c,st.areas[,-which(names(st.areas) == "state")],
                  by.x = c("state","BCR","countrynum"),
                  by.y = c("RegionCode","bcr","countrynum")) ### currently strips off anything in BCR3 canada because those strata are not in st.areas

  #spsp.c[,"ObsN"] <- as.integer(spsp.c[,"ObsN"])

  #spsp.c[,"rt.uni.ob"] <- paste(spsp.c$state,spsp.c$route,spsp.c$ObsN,sep = "-")


  if (!is.na(strata.rem)) {spsp.c <- spsp.c[-which(spsp.c$strata.name %in% strata.rem),] }

  #spsp.c[,"Stratum_New"] <- as.integer(factor(spsp.c[,"strat.name"]))
  #spsp.c[,"Stratum_New"] <- spsp.c[,"strat.name"]


  #spsp.routes <- unique(spsp.c[,"rt.uni.ob"])
  spsp.routes.ever <- unique(spsp.c[which(spsp.c$TotalInd != 0),c("strat.name","rt.uni")]) #routes which have had at least 1 species counted
  spsp.routes.never <- unique(spsp.c[-which(spsp.c$rt.uni %in% spsp.routes.ever$rt.uni),c("strat.name","rt.uni")]) #routes that have not had this species before
  #tmp <-
  spsp.c2 <- spsp.c[which(spsp.c$rt.uni %in% spsp.routes.ever$rt.uni),] #All data counts for routes which has had seen

  # first year each given route was run
  miny.rt <- tapply(spsp.c2[,"runyear"],spsp.c2[,"rt.uni"],min)
  miny.df <- data.frame(rt.uni = names(miny.rt),fyr_rt_run = as.integer(miny.rt))

  # number of times the species was seen on the given route since it has run
  n.yr.rt <- tapply(spsp.c[which(spsp.c$TotalInd != 0),"runyear"],spsp.c[which(spsp.c$TotalInd != 0),"rt.uni"],length)
  n.yr.df <- data.frame(rt.uni = names(n.yr.rt),nyr_rt_run = as.integer(n.yr.rt))

  if(nrow(spsp.routes.ever) > 0){

    spsp.routes.ever <- merge(spsp.routes.ever,miny.df,by = "rt.uni")
    spsp.routes.ever <- merge(spsp.routes.ever,n.yr.df,by = "rt.uni")

    # this will give some ever/never stats PER STRATUM
    pR <- data.frame(strat = unique(spsp.c$strat.name), nr.ever = NA, nr.never = NA, p.r.ever = NA,nry.ever = NA,meanry.ever = NA)
    for(p in unique(spsp.c$strat.name)) {
      # routes ever observed in this strata
      pR[pR$strat == p,"nr.ever"] <- nrow(spsp.routes.ever[spsp.routes.ever$strat.name == p,])
      # routes never observed in this strata
      pR[pR$strat == p,"nr.never"] <- nrow(spsp.routes.never[spsp.routes.never$strat.name == p,])

      # proportion of routes that have ever observed by stratum
      pR[pR$strat == p,"p.r.ever"] <-  pR[pR$strat == p,"nr.ever"]/(pR[pR$strat == p,"nr.ever"]+ pR[pR$strat == p,"nr.never"])

      # total counts that have included this species (ex: 5 routes in year 1, 4 in year 2 = 9 overall)
      pR[pR$strat == p,"nry.ever"] <-  length(spsp.c[which(spsp.c$strat.name == p & spsp.c$TotalInd > 0),"rt.uni.y"])

      # still don't know what these do
      pR[pR$strat == p,"fy.wspecies"] <-  min(spsp.routes.ever[spsp.routes.ever$strat.name == p,"fyr_rt_run"])
      pR[pR$strat == p,"max.nry"] <-  max(spsp.routes.ever[spsp.routes.ever$strat.name == p,"nyr_rt_run"])


      pR[pR$strat == p,"meanry.ever"] <-  length(spsp.c[which(spsp.c$strat.name == p & spsp.c$TotalInd > 0),"rt.uni.y"])/pR[pR$strat == p,"nr.ever"]
    }

    pR[,"strat"] <- as.character(pR[,"strat"])

    #gets rid of infinite values
    pR[which(pR$fy.wspecies > 2100),"fy.wspecies"] <- NA
    pR[which(pR$max.nry < 0),"max.nry"] <- NA

    write.csv(pR,paste(dir.spsp,"\\",sp.1f," pR pre strata exclusion.csv", sep = ""))

    spsp.c.l <- spsp.c[which(spsp.c$rt.uni %in% unique(spsp.routes.ever$rt.uni)),]
    spsp.c.drop <- spsp.c[-which(spsp.c$rt.uni %in% unique(spsp.routes.ever$rt.uni)),]

    spsp.c <- spsp.c.l


    #spsp.2 <- spsp.c[order(spsp.c[,"Stratum_New"]),]
    ##################
    spsp.2 <- spsp.c


    spsp.2 <- spsp.2[which(spsp.2$strat.name %in% pR[which(pR$nr.ever >= minNRoutes & pR$max.nry >= minMaxRouteYears & pR$meanry.ever >= minMeanRouteYears),"strat"]),]



    rts.used <- unique(spsp.2[,c("rt.uni","strat.name")])

    #get number of routes used per strata
    #incidentally this ends up being all the strata that are used
    rts.summary <- tapply(rts.used$rt.uni,rts.used$strat.name,length)
    nrts.used <- data.frame(strat.name = names(rts.summary),nroutes.used = as.integer(rts.summary))



    aw <- data.frame(unique(spsp.2[,c("Area","strat.name","state","BCR","St_12")]))


    spsp.temp.2 <- merge(spsp.2,pR[,c("strat","p.r.ever","meanry.ever","max.nry")], by.x = "strat.name", by.y = "strat",all.x = T)

    spsp.2 <- spsp.temp.2

    #the nth year
    spsp.2[,"yr"] <- (spsp.2[,"runyear"]+1)-min(spsp.2[,"runyear"])


    spsp.2[,"count"] <- spsp.2[,c("TotalInd")]
    #spsp.f <- spsp.2[which(spsp.2$strat.name %in% pR[which(pR$nr.ever > 2 & pR$meanry.ever >= 2),"strat"]),]


    spsp.f <- spsp.2

  }
  else{spsp.f <- spsp.c[-c(1:nrow(spsp.c)),]}

  if(nrow(spsp.f) > 0)
    {
    spsp.f[,"stratcode"] <- spsp.f[,"St_12"]
    spsp.f[,"stratum"] <- as.numeric(factor(spsp.f$St_12))

    spsp.f <- spsp.f[order(spsp.f$stratum,spsp.f$route,spsp.f$runyear),]

    strat.list <- unique(spsp.f[,c("state","countrynum","BCR","prov","stratum","strat.name","stratcode","State")])
    #strat.list1 <- unique(spsp.c[,c("state","countrynum","BCR","prov","stratum","Stratum_dtext")])
    strat.use <- strat.list[,"stratcode"]

    pR2 <- merge(pR[which(pR$strat %in% strat.list[,"strat.name"]),],strat.list,by.x = "strat", by.y = "strat.name")

    pR2 <- pR2[order(pR2$stratum),]




    spsp.f <- spsp.f[order(spsp.f$stratum,spsp.f$route,spsp.f$runyear),]

    spsp.f[,"rt.uni.ob"] <- paste(spsp.f$state,spsp.f$route,spsp.f$ObsN,sep = "-")

    strata.adj <-  pR2[,"stratcode"]
    #strata.adj <- strata.adj[-which(strata.adj %in% c("CA-SK-6"))]

    #load("input\\adj.matrixfullnew.RData")
    #load("input\\BBS strata distance matrix.RData")
    #
    #rownames(adjmat.full)[which(rownames(adjmat.full) == "CA-PENS-14")] <- "CA-NSPE-14"
    #colnames(adjmat.full)[which(colnames(adjmat.full) == "CA-PENS-14")] <- "CA-NSPE-14"
    #
    #rownames(pD)[which(rownames(pD) == "CA-NS-14")] <- "CA-NSPE-14"
    #colnames(pD)[which(colnames(pD) == "CA-NS-14")] <- "CA-NSPE-14"
    ##
    #adjmat <- adjmat.full[strata.adj,strata.adj]
    #pD <- pD[strata.adj,strata.adj]
    #
    #num = integer
    #ajd = integer
    #j <- 0
    #t.snm <- NA
    #for(i in colnames(adjmat)) {
    # if (length(which(adjmat[,i])) == 0) {
    # j <- j+1
    # if (j == 1){str.no.neighb <- i}else{str.no.neighb <- c(str.no.neighb,i)}
    # closest <- names(which.min(pD[i,which(pD[i,] > 0)]))
    #
    #adjmat[closest,i] <- T
    #adjmat[i,closest] <- T
    #} ## if statement assigns the nearest strata as a neighbour for any stratum that lacks adjacent neighbours
    #  } # loop re-writes adjmat to remove strata without neighbours
    #
    #
    #if (ncol(adjmat) < 5) {LT5.strata <- T}else{ LT5.strata <- F}
    #
    #for(i in 1:ncol(adjmat)) {
    #if (i == 1) {
    # num <- length(which(adjmat[,i]))
    # adj <- which(adjmat[,i])
    # }else{
    #
    #
    # num <- c(num,length(which(adjmat[,i])))
    # adj <- c(adj,which(adjmat[,i]))
    #
    #}
    #}
    #adj <- as.integer(adj)
    #sumNumNeigh <- length(adj)
    #
    #str.remain <- names(adjmat[1,])

    #
    #a.wts <- unique(spsp.f[,c("strat.name","Area")])
    pR.wts <- unique(spsp.f[,c("strat.name","p.r.ever")])




    tmp <- unique(spsp.f[,c("stratum","rt.uni.ob")])

    # number of observers per stratum
    nobservers <- tapply(tmp$rt.uni.ob,tmp$stratum,length)


    for (s in 1:length(nobservers)) {
      sel1 <- which(spsp.f$stratum == s)

      tmp <- unique(spsp.f[sel1,c("stratum","rt.uni.ob")])

      #tmp <- tmp[order(tmp[,c("stratum","rt.uni.ob")]),]
      tmp[,"obser"] <- as.numeric(factor(tmp[,"rt.uni.ob"]))
      if(s == 1) {obsfact <- tmp}else{obsfact <- rbind(obsfact,tmp)}
    }

    spsp.ft <- merge(spsp.f,obsfact,by = c("stratum","rt.uni.ob"))

    spsp.f <- spsp.ft


    #for (o in observers) {
    # spsp.f[which(spsp.f$rt.uni.ob == o),"obser"] <- i
    # i = i+1
    #}
    #}

    fyears <- tapply(spsp.f$runyear,spsp.f$rt.uni.ob,min)
    fyr.df <- data.frame(rt.uni.ob = names(fyears),runyear = as.integer(fyears),firstyear = 1)
    spsp.ft <- merge(spsp.f,fyr.df,all.x = T,by = c("rt.uni.ob","runyear"))

    spsp.f <- spsp.ft

    spsp.f[which(is.na(spsp.f$firstyear)),"firstyear"] <- 0
    #for (i in unique(spsp.f$rt.uni.ob)) {
    # spsp.f[which(spsp.f$rt.uni.ob == i & spsp.f$runyear == fyears[i]),"firstyear"] <- 1
    #
    #}



    #output <- data.frame(count = as.integer(spsp.f$count), strat = spsp.f$stratum, obser = spsp.f$obser, year = spsp.f$yr, firstyr = spsp.f$firstyear, yeareff = spsp.f$yr, strat.name = spsp.f$strat.name, route = spsp.f$rt.uni, rYear = spsp.f$runyear)
    output <- spsp.f[,c("count","stratum","obser","yr","firstyear","strat.name","rt.uni","runyear")]
    names(output) <- c("count","strat","obser","year","firstyr","strat.name","route","rYear")

    if (outdata == T) {write.csv(output,paste(dir.spsp,"\\",sp.1f," prebugsdata.csv", sep = ""))
      write.csv(nrts.used,paste(dir.spsp,"\\",sp.1f," number of routes used by stratum.csv", sep = ""),row.names = F)
    }
    return(list(output = output, pR.wts = pR.wts, aw = aw, pR = pR,pR2 = pR2, nobservers = nobservers, nrts.used = nrts.used)) #adjmat = adjmat,LT5.strata = LT5.strata, num = num, adj = adj, sumNumNeigh = sumNumNeigh
  } else {
    output <- spsp.f
    return(list(output = output)) }

}
