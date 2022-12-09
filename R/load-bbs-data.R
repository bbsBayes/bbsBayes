#' Load Breeding Bird Survey dataset into R Session
#'
#' Loads the raw, unstratified BBS data into the current R session.
#' The data must have been previously fetched using `fetch_bbs_data()`.
#' Note that this is not necessary to run a Bayesian analysis of BBS data;
#' calling `stratify()` will do the loading for you.
#'
#' @param sample Logical. Whether or not to use the sample data for Pacific
#'   Wrens. Defaults to `FALSE`. If `TRUE`, `level` and `release` are ignored.
#' @inheritParams common_docs
#'
#'
#' @return Large list (3 elements) consisting of:
#' \item{birds}{Data frame of sample bird point count data per route, per year}
#' \item{routes}{Data frame of sample yearly route data}
#' \item{species}{Sample list of North American bird species}
#'
#' @export

load_bbs_data <- function(level = "state", release = 2022,
                          sample = FALSE, quiet = TRUE) {

  # Return sample data
  if(sample) {
    if(!quiet) message("Using sample BBS data...")
    return(bbsBayes::bbs_data_sample)
  }

  # Return full data
  if(!quiet) message("Loading BBS data...")

  bbs_dir <- rappdirs::app_dir(appname = "bbsBayes")

  f <- file.path(bbs_dir(), paste0("bbs_", level, "_data_", release, ".rds"))

  if(!file.exists(f)) stop("No BBS data downloaded. ",
                           "Please use `fetch_bbs_data()` first.",
                           call. = FALSE)

  readr::read_rds(f)
}


