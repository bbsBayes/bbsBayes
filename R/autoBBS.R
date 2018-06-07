autoBBS <- function(species = NULL,
                    model = NULL,
                    outputDir = NULL,
                    inits = NULL,
                    adaptSteps = 500,
                    burnInSteps = 20000,
                    nChains = 3,
                    numSavedSteps = 2000,
                    thinSteps = 10,
                    nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ),
                    runParallelChains = FALSE)
{
  processAutoBBSInput(species, model, outputDir)

  cat("Cleaning data...")
  data.cleaned <- cleanData()
  cat("done!\n")

  speciesIndex <- getSpeciesIndex(data.cleaned$species,
                                  species)

  spNum <- 1
  totalSp <- length(species)
  for (index in speciesIndex)
  {
    cat(paste("Species ", spNum, "/", totalSp, ": ", sep = ""))
    data.prep <- speciesDataPrep(data.cleaned$species,
                                 data.cleaned$unmod.sp,
                                 data.cleaned$sptorun,
                                 data.cleaned$sptorun2,
                                 index, model, outputDir)

    data.jags <- list(ncounts = nrow(data.prep$spsp.f),
                      nstrata=length(unique(data.prep$spsp.f$strat)),
                      ymin = data.prep$ymin,
                      ymax = data.prep$ymax,
                      nonzeroweight = data.prep$pR.wts$p.r.ever,
                      count = as.integer(data.prep$spsp.f$count),
                      strat = as.integer(data.prep$spsp.f$strat),
                      obser = as.integer(data.prep$spsp.f$obser),
                      year = data.prep$spsp.f$year,
                      firstyr = data.prep$spsp.f$firstyr,
                      nobservers = data.prep$nobservers)
    if (tolower(model) == "standard")
    {
      data.jags <- c(data.jags, list(fixedyear = midyear))
    }

    sp.params = c("sdbeta",
                  "strata",
                  "STRATA",
                  "sdstrata",
                  "sdobs",
                  "obs",
                  "n",
                  "posdiff",
                  "sdnoise",
                  "eta",
                  "overdisp",
                  "nfzero",
                  "gof",
                  "fgof",
                  "diffgof")

    jagsjob <- runModel(data.jags,
                            NULL,
                            sp.params,
                            mod = standard,
                            nChains,
                            adaptSteps,
                            nIter,
                            burnInSteps,
                            thinSteps,
                            runParallelChains)
    save(jagsjob, file = paste(data.prep$dir, "/jags.Rdata", sep=""))
    spNum <- spNum + 1
  }
}
