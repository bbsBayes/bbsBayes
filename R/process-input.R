processAutoBBSInput <- function(speciesList,
                         model, outputDir)
{
  processSpeciesList(speciesList)
  processModel(model)
  processOutputDir(outputDir)
}

processSpeciesList <- function(speciesList)
{
  # Check for at least 1 species to test.
  if (is.null(speciesList))
  {
    stop("no species were provided to autoBBS().\n")
  }
}

processModel <- function(model)
{
  # Check for at least 1 model to test
  if (is.null(model))
  {
    stop("no model was specified to autoBBS().\n")
  }
  else
  {
    if ((tolower(model) %in% c("standard", "fd")) == FALSE)
    {
      stop(paste(model, "is not a valid model.\n"))
    }
  }
}

processOutputDir <- function(outputDir)
{
  # Check for output directory
  if (is.null(outputDir))
  {
    warning(paste("no output directory was specified.",
                  "Sending output to", getwd()))
  }
}
