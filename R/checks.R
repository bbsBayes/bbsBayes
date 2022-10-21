
#' Check model name
#'
#' @param model Name of model
#'
#' @noRd
check_model <- function(model, model_variant) {
  if(is.null(model)) stop("No `model` specified", call. = FALSE)
  model <- tolower(model)

  if(!model %in% bbs_models$model) {
    stop("Invalid `model` specified. Must be one of ",
         paste0(unique(bbs_models$model), collapse = ", "),
         call. = FALSE)
  }

  if(model_variant == "nonhier") {
    if(model != "first_diff") stop("`model_variant` 'nonhier' only allowed ",
                                   "for `first_diff` models", call. = FALSE)
   warning("Non-hierarchial models are generally not recommended ",
           "(see ?bbs_models), but provided for compatibility with the USGS",
           "methods", call. = FALSE, immediate. = TRUE)
  }

  model
}

check_model_variant <- function(model_variant) {
  if(is.null(model_variant)) stop("No `model_variant` specified",
                                  call. = FALSE)
  model_variant <- tolower(model_variant)
  if(!model_variant %in% bbs_models$variant) {
    stop("Invalid `model_variant` specified. Must be one of ",
         paste0(unique(bbs_models$variant), collapse = ", "),
         call. = FALSE)
  }
  model_variant
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
check_stratification <- function(strata, name = "stratification (`by`)") {
  if(is.null(strata)) stop("No ", name, " specified", call. = FALSE)
  strata <- tolower(strata)
  s <- c("state", "bcr", "latlong", "bbs_cws", "bbs_usgs")
  if(!strata %in% s) {
    stop("Invalid stratification specified, choose one of '",
         paste0(s, collapse = "', '"), "'", call. = FALSE)
  }
  strata
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

check_neighbours <- function(spatial_data, strata) {

  # Check for correct data
  if(!is.list(spatial_data) ||
     !all(c("n", "n_edges", "node1", "node2", "adj_matrix", "strata_name") %in%
          names(spatial_data))) {
    stop("`spatial_data` must a list created by `prepare_spatial()` ",
         "containing\n  at least `n`, `n_edges`, `node1`, `node2`, `adj_matrix` ",
         "and `strata_names`", call. = FALSE)
  }

  # Check for matching strata
  if(!all(spatial_data$strata_names %in% strata)) {
    stop("The same strata must be used in both `prepare_data()` and ",
         "`prepare_spatial()`", call. = FALSE)
  }
}


#' Check user supplied sf object
#'
#' Quietly passes `NULL` objects through
#'
#' @param sf sf spatial data frame
#'
#' @noRd
check_sf <- function(sf) {
  if(!is.null(sf)) {
    if(!inherits(sf, "sf")) {
      stop("`'`", deparse(substitute(sf)), "`'` must be an 'sf' spatial data frame",
           call. = FALSE)
    } else if(nrow(sf) == 0) {
      stop("Empty spatial data frame (`", deparse(substitute(sf)), "`)",
           call. = FALSE)
    }
  }
}


check_type <- function(msg, check_fun, ...) {
  args <- list(...)
  if(is.null(names(args))) {
    names(args) <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = "a")
  }

  ck <- vapply(args, check_fun, FUN.VALUE = TRUE)
  if(!all(ck)) {
    stop("`", paste0(names(ck[!ck]), collapse = "`, `"),
         "` must be ", msg, call. = FALSE)
  }
}

check_logical <- function(...) {
  check_type(msg = "logical (TRUE/FALSE)", check_fun = is.logical, ...)
}

check_numeric <- function(...) {
  check_type(msg = "a number", check_fun = is.numeric, ...)
}

check_in <- function(arg, opts) {
 if(!arg %in% opts) {
   if(is.character(opts)) sep <- "'" else if(is.numeric(opts)) sep <- ""
   stop("`", deparse(substitute(arg)),"` ",
        "must be one of ", sep, paste0(opts, collapse = paste0(sep, ", ", sep)),
        sep, ".",
        call. = FALSE)
 }
}
