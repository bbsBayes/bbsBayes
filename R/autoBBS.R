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
    cat(paste("Species ", spNum, "/", totalSp, sep = ""))

    cat("\n")
    spNum <- spNum + 1
  }
}
