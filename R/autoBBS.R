autoBBS <- function(speciesList)
{
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
                                 index)
    spNum <- spNum + 1
  }
}
