autoBBS <- function(speciesList = NULL,
                    modelName = NULL,
                    adaptSteps = 500,
                    burnInSteps = 20000,
                    nChains = 3,
                    numSavedSteps = 2000,
                    thinSteps = 10,
                    nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ),
                    parallel = TRUE)
{
  processInput(speciesList, modelName)

  cat("Cleaning data...")
  data.cleaned <- cleanData()
  cat("done!\n")

  speciesIndex <- getSpeciesIndex(data.cleaned$species,
                                  speciesList)

  spNum <- 1
  totalSp <- length(speciesList)
  for (index in speciesIndex)
  {
    cat(paste("Species ", spNum, "/", totalSp, ": ", sep = ""))
    data.prep <- speciesDataPrep(data.cleaned$species,
                                 data.cleaned$unmod.sp,
                                 data.cleaned$sptorun,
                                 data.cleaned$sptorun2,
                                 index, modelName)
    spNum <- spNum + 1
  }
}
