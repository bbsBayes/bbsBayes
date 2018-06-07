processInput <- function(speciesList,
                         modelName)
{
  if (is.null(speciesList))
  {
    stop("no species were provided to autoBBS().\n")
  }

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
}
