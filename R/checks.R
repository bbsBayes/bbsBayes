#' Check model name
#'
#' @param model Name of model
#'
#' @noRd
check_model <- function(model) {
  if(is.null(model)) stop("No model specified", call. = FALSE)
  model <- tolower(model)
  m <- c("slope", "firstdiff", "gam", "gamye")
  if(!model %in% m) {
    stop("Invalid model specified. Must be one of ", paste0(m, collapse = ", "),
         call. = FALSE)
  }
  model
}

#' Check basis value
#'
#' @param basis basis specified
#'
#' @noRd
check_basis <- function(basis) {
  if(is.null(basis)) stop("No basis specified", call. = FALSE)
  basis <- tolower(basis)
  b <- c("original", "mgcv")
  if(!basis %in% b) {
    stop("Invalid basis specified. Must be one of ", paste0(b, collapse = ", "),
         call. = FALSE)
  }
  basis
}

#' Check stratification
#'
#' @param strat Stratification type
#' @param name Name of argument to return if missing.
#'
#' @noRd
check_stratification <- function(strat, name = "stratification (`by`)") {
  if(is.null(strat)) stop("No ", name, " specified", call. = FALSE)
  strat <- tolower(strat)
  s <- c("state", "bcr", "latlong", "bbs_cws", "bbs_usgs")
  if(!strat %in% s) {
    stop("Invalid stratification specified, choose one of '",
         paste0(s, collapse = "', '"), "'", call. = FALSE)
  }
  strat
}

#' Check user supplied BBS data
#'
#' @param bbs_data List with three components
#'
#' @noRd
check_bbs_data <- function(bbs_data) {
  bbs_data
}
