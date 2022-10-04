#' Load Breeding Bird Survey dataset into R Session
#'
#' \code{load_bbs_data} loads the raw, unstratified BBS data into the current R session.
#' The data must have been previously fetched using the \code{fetch_bbs_data} function.
#' Note that this function is not necessary to run a Bayesian analysis of BBS data;
#' calling \code{stratify} will return stratifed BBS data in a list of data frames.
#'
#' @param level A string, either "state" or "stop", specifying which counts to
#' load. Defaults to "state", which provides counts beginning in 1966,
#' aggregated in five bins, each of which contains cumulative counts from 10 of
#' the 50 stops along a route. Specifying "stop" provides stop-level data
#' beginning in 1997, which includes counts for each stop along routes
#' individually. Note that stop-level data is not currently supported by
#' the modeling utilities in bbsBayes.
#'
#' @return Large list (3 elements) consisting of:
#' \item{bird}{Data frame of sample bird point count data per route, per year}
#' \item{route}{Data frame of sample yearly route data}
#' \item{species}{Sample list of North American bird species}
#'
#' @export
#'

load_bbs_data_orig <- function(level = "state")
{
  bbs_data <- NULL
  rm(bbs_data)

  bbs_dir <- app_dir(appname = "bbsBayes")

  if (level == "state")
  {
    if(isFALSE(file.exists(paste0(bbs_dir$data(), "/bbs_raw_data.RData"))))
    {
      stop("No BBS data downloaded. Please use fetch_bbs_data() first.")
    }

    load(file = paste0(bbs_dir$data(), "/bbs_raw_data.RData"))
  }else if (level == "stop")
  {
    if(isFALSE(file.exists(paste0(bbs_dir$data(), "/bbs_stop_data.RData"))))
    {
      stop("No BBS stop data downloaded. Please use fetch_bbs_data(level = \"stop\") first.")
    }

    load(file = paste0(bbs_dir$data(), "/bbs_stop_data.RData"))
  }

  return(bbs_data)
}


#' Load Breeding Bird Survey dataset into R Session (TIDY)
#'
#' Loads the raw, unstratified BBS data into the current R session.
#' The data must have been previously fetched using `fetch_bbs_data()`.
#' Note that this is not necessary to run a Bayesian analysis of BBS data;
#' calling `stratify()` will do the loading for you.
#'
#' @param level Character. #ither "state" or "stop", specifying which counts to
#' load. Defaults to "state", which provides counts beginning in 1966,
#' aggregated in five bins, each of which contains cumulative counts from 10 of
#' the 50 stops along a route. Specifying "stop" provides stop-level data
#' beginning in 1997, which includes counts for each stop along routes
#' individually. Note that stop-level data is not currently supported by
#' the modelling utilities in bbsBayes.
#'
#' @return Large list (3 elements) consisting of:
#' \item{birds}{Data frame of sample bird point count data per route, per year}
#' \item{routes}{Data frame of sample yearly route data}
#' \item{species}{Sample list of North American bird species}
#'
#' @export

load_bbs_data <- function(level = "state") {
  bbs_dir <- rappdirs::app_dir(appname = "bbsBayes")

  if (level == "state") f <- file.path(bbs_dir$data(), "bbs_raw_data.rds")
  if (level == "stop") f <- file.path(bbs_dir$data(), "bbs_stop_data.rds")


  if(!file.exists(f)) stop("No BBS data downloaded. ",
                           "Please use `fetch_bbs_data()` first.",
                           call. = FALSE)

  readr::read_rds(f)
}


