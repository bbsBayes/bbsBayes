strip_model_files <- function(files) {
  f <- files[stringr::str_detect(files, ".csv$")]

  f_new <- stringr::str_remove(f, paste0(Sys.Date(), "-"))
  file.rename(f, f_new)

  # Trim to omit date/run related metrics (stuff that changes)
  for(i in f_new){
    x <- readLines(i)
    x <- x[!stringr::str_detect(x, "^#")]
    writeLines(x, i)
  }

  f_new
}



#' Wrapper for `expect_snapshot_value()` from testthat
#'
#' Copies parameters over from `expect_snapshot_value()`
#'
#' @noRd
expect_snapshot_value_safe <- function(...) {

   if(!interactive()) {
     testthat::expect_snapshot_value(...)
   } else message("Skipping snapshot tests because interactive")

}
