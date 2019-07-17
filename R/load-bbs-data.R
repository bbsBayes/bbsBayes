#' Load Breeding Bird Survey dataset into R Session
#'
#' \code{load_bbs_data} loads the raw, unstratified BBS data into the current R session.
#' The data must have been previously fetched using the \code{fetch_bbs_data} function.
#' Note that this function is not necessary to run a Bayesian analysis of BBS data;
#' calling \code{stratify} will return stratifed BBS data in a list of data frames.
#'
#' @return Large list (3 elements) consisting of:
#' \item{bird}{Data frame of sample bird point count data per route, per year}
#' \item{route}{Data frame of sample yearly route data}
#' \item{species}{Sample list of North American bird species}
#'
#' @export
#'
#'
load_bbs_data <- function()
{
  bbs_data <- NULL
  rm(bbs_data)

  bbs_dir <- app_dir(appname = "bbsBayes")

  if(isFALSE(file.exists(paste0(bbs_dir$data(), "/bbs_raw_data.RData"))))
  {
    stop("No BBS data downloaded. Please use fetch_bbs_data() first.")
  }

  load(file = paste0(bbs_dir$data(), "/bbs_raw_data.RData"))

  return(bbs_data)
}

