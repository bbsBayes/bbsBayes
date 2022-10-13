#' Check model name
#'
#' @param model Name of model
#'
#' @noRd
check_model <- function(model, model_variant) {
  if(is.null(model)) stop("No `model` specified", call. = FALSE)
  if(is.null(model_variant)) stop("No `model_variant` specified",
                               call. = FALSE)
  model <- tolower(model)
  model_variant <- tolower(model_variant)

  if(!model %in% bbs_models$model) {
    stop("Invalid `model` specified. Must be one of ",
         paste0(unique(bbs_models$model), collapse = ", "),
         call. = FALSE)
  }
  if(!model_variant %in% bbs_models$variant) {
    stop("Invalid `model_variant` specified. Must be one of ",
         paste0(unique(bbs_models$variant), collapse = ", "),
         call. = FALSE)
  }
  if(model_variant == "nonhier") {
    if(model != "first_diff") stop("`model_variant` 'nonhier' only allowed ",
                                   "for `first_diff` models", call. = FALSE)
   warning("Non-hierarchial models are generally not recommended ",
           "(see ?bbs_models), but provided for compatibility with the USGS",
           "methods", call. = FALSE)
  }

  c(model, model_variant)
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

check_species <- function(species, species_list,
                          name = "species (`species_to_run`)") {
  if(is.null(species)) stop("No ", name, "specified", call. = FALSE)

  if(!species %in% species_list$english) {
    stop("Invalid species specified. Ensure ", name, " matches a value in\n",
         "the `english` column of the stratified species list ",
         "(`strat_data$species_strat)`.",
         call. = FALSE)
  }
  species
}

#' Check user supplied BBS data
#'
#' @param bbs_data List with three components
#'
#' @noRd
check_bbs_data <- function(bbs_data) {
  bbs_data
}
