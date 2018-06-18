createRunDirectory <- function(outputDir, sp.eng, modelName)
{
  dir1 <- outputDir
  if (is.null(outputDir))
  {
    dir1 <- getwd()
  }
  dir.spsp <- paste(dir1, "/",
                    sp.eng,
                    "-",
                    modelName,
                    "-",
                    format(Sys.Date(), format="%Y-%m-%d"),
                    "-",
                    format(Sys.time(), format="%H%M%S"), sep = "")

  dir.create(dir1, showWarnings = FALSE)
  dir.create(dir.spsp)

  return(dir.spsp)
}

