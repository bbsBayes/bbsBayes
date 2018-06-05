cleanData <- function(reduced = TRUE)
{
  datacounts <- datacount.sp
  names(datacounts) <- c("sp","eng","nprov","nroute","nyear","nrouteyears","rungroup")

  bcan <- birds
  rcan <- route

  sptorun <- NULL
  if (reduced)
  {
    sptorun <- datacounts[which(datacounts$nroute > 10 & datacounts$nyear > 15),]
  }
  else
  {
    sptorun <- datacounts
  }

  sptorun[,"sp1f"] <- sptorun[,"eng"]

  for(j in grep(" (",sptorun$eng,fixed = T))
  {
    sptorun[j,"sp1f"] <- strsplit(sptorun[j,"sp1f"]," (",fixed = T)[[1]][1]
  }

  for(j in grep(") ",sptorun$eng,fixed = T))
  {
    sptorun[j,"sp1f"] <- strsplit(sptorun[j,"sp1f"],") ",fixed = T)[[1]][1]
    sptorun[j,"sp1f"] <- gsub(sptorun[j,"sp1f"],pattern = "(",replacement = "",fixed = T)
  }

  sptorun2 <- sptorun
  sptorun2[,"unmod"] <- ""
  sptorun2[,"mod"] <- ""

  unmod.sp <- NA
  w <- 1

  species <- sptorun[,"eng"]

  sv <- ls()
  sv <- c(sv,"sv")

  return(list(species = species,
              unmod.sp = unmod.sp,
              sptorun = sptorun,
              sptorun2 = sptorun2))
}

gamPrep <- function(yminsc, ymaxsc, nyears, nknots)
{
  # GAM basis function
  knotsX<- seq(yminsc,ymaxsc,length=(nknots+2))[-c(1,nknots+2)]
  X_K<-(abs(outer(seq(yminsc,ymaxsc,length = nyears),knotsX,"-")))^3
  X_OMEGA_all<-(abs(outer(knotsX,knotsX,"-")))^3
  X_svd.OMEGA_all<-svd(X_OMEGA_all)
  X_sqrt.OMEGA_all<-t(X_svd.OMEGA_all$v  %*% (t(X_svd.OMEGA_all$u)*sqrt(X_svd.OMEGA_all$d)))
  X.basis<-t(solve(X_sqrt.OMEGA_all,t(X_K)))

  return(X.basis)
}

getSpeciesIndex <- function(speciesList, speciesToFind)
{
  indexList <- NULL
  for (species in speciesToFind)
  {
    indexList <- c(indexList, match(species, speciesList))
  }

  return(indexList)
}

speciesDataPrep <- function(species, unmod.sp,
                            sptorun, sptorun2, speciesIndex,
                            gam = FALSE,
                            nknots = 9)
{
  sp.1 <- species[speciesIndex]
  cat(paste(sp.1,date(),"\n"))
  sp.2 <- sptorun[sptorun$eng == sp.1,"sp"]
  sp.1f <- sptorun[sptorun$eng == sp.1,"sp1f"]

  dir.spsp <- paste("output/",
                    format(Sys.Date(), format="%Y-%m-%d"),
                    "-",
                    sp.1,sep = "")

  dir.create("output")
  dir.create(dir.spsp)

  datacounts <- datacount.sp
  names(datacounts) <- c("sp","eng","nprov","nroute","nyear","nrouteyears","rungroup")

  bcan <- birds
  rcan <- route

  dta <- bugsdataprep(sp.1 = sp.1,sp.1f = sp.1f, sp.2 = sp.2,
                      dir.spsp = dir.spsp, outdata = T,
                      minNRoutes = 3,# require 3 or more routes where species has been observed
                      minMaxRouteYears = 3,# require at least 1 route with non-zero obs of species in 3 or more years
                      minMeanRouteYears = 1,
                      bcan = bcan, rcan = rcan)# require an average of 1 year per route with the species observed (setting this to 1 effectively removes this criterion)

  if (nrow(dta$output) == 0 | length(unique(dta$output$strat)) < 4)
  {
    unmod.sp[w] <- sp.1
    species.l <- read.csv("species.run.continental.sum2.csv",stringsAsFactors = F)
    species.l[which(species.l$sp == sp.2),"unmod"] <- "no data"
    write.csv(species.l,"species.run.continental.sum2.csv",row.names = F)
    w <- w+1
    n.sv <- ls()
    n.sv <- n.sv[-which(n.sv %in% sv)]

    rm(list = n.sv)
    gc(verbose = F)
    next
  }

  spsp.f <- dta$output
  a.wts <- dta$a.wts
  pR.wts <- dta$pR.wts
  aw <- dta$aw
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

  toReturn <- list(spsp.f = spsp.f,
                   ymin = ymin,
                   ymax = ymax,
                   pR.wts = pR.wts,
                   nobservers = nobservers,
                   dir = dir.spsp,
                   sp.1 = sp.1)
  if (gam)
  {
    X.basis <- gamPrep(yminsc, ymaxsc, nyears, nknots)
    toReturn <- c(toReturn, list(nknots = nknots, X.basis = X.basis))
  }

  return(toReturn)
}
