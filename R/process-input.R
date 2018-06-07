processInput <- function(speciesList,
                         modelName, outputDir)
{
  # Check for at least 1 species to test.
  if (is.null(speciesList))
  {
    stop("no species were provided to autoBBS().\n")
  }

  # Check for at least 1 model to test
  if (is.null(modelName))
  {
    stop("no model was specified to autoBBS().\n")
  }
  else
  {
    if ((tolower(modelName) %in% c("standard", "fd")) == FALSE)
    {
      stop(paste(modelName, "is not a valid model.\n"))
    }
  }

  # Check for output directory
  if (is.null(outputDir))
  {
    warning(paste("no output directory was specified.",
                  "autoBBS() will default to sending output to",
                  getwd()))
  }
}
