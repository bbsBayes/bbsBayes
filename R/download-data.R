saveBBSdata <- function(outputDir = NULL)
{
  processOutputDir(outputDir)

  dir1 <- outputDir
  if (is.null(dir1))
  {
    dir1 <- getwd()
  }
  toSave <- c(birds, datacount.sp, route, sp, st.areas, midyear)

  save(toSave,
       file = paste(dir1,"/bbsData.RData", sep =""))
}
