check_data <- function(data) {

  type <- deparse(substitute(data))
  n <- c("meta_data", "meta_strata")

  if(type == "strata_data") {
    n <- c(n, "routes_strata", "birds_strata")
    from <- "stratify()"
  } else if(type == "prepped_data") {
    n <- c(n, "model_data", "raw_data")
    from <- "prepare_data()"
  } else if(type == "spatial_data") {
    # Don't use meta_data or meta_strata, n is an actual object returned
    n <- c("n", "n_edges", "node1", "node2", "adj_matrix", "strata_meta")
    from <- "prepare_spatial()"
  } else if(type == "model_output") {
    n <- c(n, "model_fit", "raw_data")
    from <- "run_model()"
  } else if(type == "indices") {
    n <- c(n, "indices", "samples", "raw_data")
    from <- "generate_indices()"
  } else if (type == "trends") {
    n <- c(n, "trends", "raw_data")
    from <- "generate_trends()"
  }

  # All n must be in names(data), but not necessarily the reverse
  # Allows optional data like `map` in prepare_spatial()
  if(!is.list(data) || !all(n %in% names(data))) {
    stop("`", type, "` must a list created by `", from, "` ",
         "containing\n`", paste0(n, collapse = "`, `"), "`",
         call. = FALSE)
  }
}
#' Check bbs data files
#'
#' @param level Type of data (state or stop)
#' @param release Year of data release, currently 2020 or 2022
#' @param force Logical Force download even if exists
#' @param quiet Suppress messages
#'
#' @noRd
check_bbs_data <- function(level, release, force, quiet) {
  out_dir <- bbs_dir(quiet)

  f <- file.path(out_dir, paste0("bbs_", level, "_data_", release, ".rds"))

  if(file.exists(f) & !force) {
    stop("BBS ", level, " data for the ", release, " release already exists ",
         "(", f, ")\n",
         "Use \"force = TRUE\" to overwrite.", call. = FALSE)
  }
  f
}



#' Check model name
#'
#' @param model Name of model
#'
#' @noRd
check_model <- function(model, model_variant) {
  if(is.null(model)) stop("No `model` specified", call. = FALSE)
  model <- tolower(model)

  model_variant <- check_model_variant(model_variant)

  if(!model %in% bbsBayes::bbs_models$model) {
    stop("Invalid `model` specified. Must be one of ",
         paste0(unique(bbsBayes::bbs_models$model), collapse = ", "),
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
  if(!model_variant %in% bbsBayes::bbs_models$variant) {
    stop("Invalid `model_variant` specified. Must be one of ",
         paste0(unique(bbsBayes::bbs_models$variant), collapse = ", "),
         call. = FALSE)
  }
  model_variant
}

check_model_file <- function(model, model_variant, model_file) {
  if(is.null(model_file)) {
    f <- system.file("models",
                     paste0(model, "_", model_variant, "_bbs_CV.stan"),
                     package = "bbsBayes")
  } else f <- model_file

  if(!file.exists(f)) {
    msg <- "Stan model file not found"
    if(is.null(model_file)) {
      msg <- c(msg, ". Please submit an issue at \n",
               "https://github.com/BrandonEdwards/bbsBayes/issues")
    } else {
      msg <- c(msg, " ('", f, "')")
    }

    stop(msg, call. = FALSE)
  }

  f
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

check_init <- function(init, chains) {
  if(inherits(init, "list")) {
    if(length(init) != chains) {
      message("`init` values as a list need one list per chain. ",
              "Assuming values should be duplicated for each chain...")
      orig <- init
      init <- list()
      for(n in seq_len(chains)) init[[n]] <- orig
    }
  }
  init
}

#' Check for slope
#'
#' @param trends Trends data
#'
#' @noRd
check_slope <- function(trends, slope) {
  if(slope && !"slope_trend" %in% names(trends)) {
    stop("To use `slope = TRUE`, `generate_trends()` must have been also ",
         "run with `slope = TRUE`", call. = FALSE)
  }
}

#' Check stratification
#'
#' @param strata Stratification type
#' @param name Name of argument to return if missing.
#' @param custom sf data frame of a map for custom strata
#'
#' @noRd
check_strata <- function(strata, custom = NULL, simple = FALSE,
                         quiet = TRUE) {
  if(is.null(strata)) stop("No stratification (`by`) specified", call. = FALSE)
  strata <- tolower(strata)

  type <- "standard"

  # Simple checks
  if(simple) {
    if(!strata %in% names(bbsBayes::bbs_strata)) {
      stop("Invalid stratification specified, choose one of '",
           paste0(names(bbsBayes::bbs_strata), collapse = "', '"), "'",
           call. = FALSE)
    } else {
      return(strata)
    }
  }

  # Working with a custom stratification
  if(!strata %in% names(bbsBayes::bbs_strata)) {

    if(is.null(custom) || !inherits(custom, "sf")) {
      stop("Invalid stratification specified, choose one of '",
           paste0(names(bbsBayes::bbs_strata), collapse = "', '"),
           "',\n or provide an sf spatial data frame to `strata_custom` ",
           "to use a custom stratification", call. = FALSE)
    }
    type <- "custom"
  }

  # Working with an established stratification
  if(strata %in% names(bbsBayes::bbs_strata)) {

    if(!is.null(custom)) {
      # Check if strata established, and custom is a data frame
      if(!inherits(custom, "data.frame")) {
        stop("`strata_custom` is provided, but is not a data frame. \n",
             "If using an establish stratification ('", strata, "'), ",
             "`strata_custom` must be either empty, or a data frame",
             call. = FALSE)
      } else {
        # Check if strata established, and custom is subset of the correct data

        # - Check cols, then check strata names
        if(!all(names(bbsBayes::bbs_strata[[strata]]) %in% names(custom)) ||
           !all(custom$strata_name %in%
                bbsBayes::bbs_strata[[strata]]$strata_name)) {
          stop("`strata_custom` is not a subset of ",
               "`bbs_strata[[\"", strata, "\"]]`.\n",
               "If using a custom set of an established stratification ",
               "('", strata, "'), the filtered data must have columns and ",
               "strata names from the original", call. = FALSE)
        }

      }

      # Don't modify if the exact same
      if(!isTRUE(all.equal(bbsBayes::bbs_strata[[strata]], custom))) {
        type <- "subset"
      }
    }
  }

  if(!quiet) message("Using '", strata, "' (", type, ") stratification")

  c(strata, type)
}

check_regions <- function(regions, stratify_by, stratify_type,
                          regions_index = NULL) {

  r <- c("continent", "country", "stratum", "prov_state", "bcr",
         "bcr_by_country")

  if(!is.null(regions_index)) {
    if(!"strata_name" %in% names(regions_index)) {
      stop("`regions_index` must have a `strata_name` column", call. = FALSE)
    }

    r <- union(r, names(regions_index))
    r <- r[r != "strata_name"]
  }

  if(!all(regions %in% r)) {
    stop("`regions` must be any of `", paste0(r, collapse = "`, `"), "`",
         call. = FALSE)
  }

  msg <- "Stratification does not match desired regions:\n"

  if(stratify_by %in% c("bcr", "latlong") &
     any(regions %in% c("country", "prov_state"))) {
    stop(msg,
         "BCRs and lat-long degree block stratifications can not be divided ",
         "into regions with political boundaries ('country', 'prov_state').",
         call. = FALSE)
  }

  if(stratify_by == "prov_state" & "bcr" %in% regions){
    stop(msg,
         "The States and Provinces stratification",
         "can not be divided into BCR regions.",
         call. = FALSE)
  }

  # Check custom
  if(stratify_type == "custom" && is.null(regions_index) &&
     any(!regions %in% c("continent", "stratum"))) {
      stop(msg, "Custom stratifications can only be divided into 'stratum' ",
           "and 'continent' regions, unless a `region_index` is provided.",
           call. = FALSE)
  }
}


#' Get AOU numerical code of a species
#'
#' Return the 4-5 digit BBS code for the given species by matching English,
#' French, or Scientific names, or AOU codes.
#'
#' @param species Character. English name of species
#' @param species_list Data frame. Species List
#' @param combine_species_forms Logical. Whether to use combined or not
#'
#' @return 4-5 digit AOU code for the given species
#'
#' @noRd
check_species <- function(species, species_list, combine_species_forms,
                          quiet = FALSE) {
  nm <- deparse(substitute(species))
  if(missing(species) || is.null(species)) {
    stop("No `", nm, "` specified", call. = FALSE)
  }

  species <- tolower(species)
  s_list_combo <- dplyr::filter(species_list, .data$unid_combined == TRUE)
  s_list_no_combo <- dplyr::filter(species_list, .data$unid_combined == FALSE)

  s_combo <- dplyr::filter(
    s_list_combo,
    .env$species == tolower(.data$english) |
      .env$species == tolower(.data$french) |
      .env$species == tolower(paste(.data$genus, .data$species)) |
      .env$species == .data$aou)

  s_no_combo <- dplyr::filter(
    s_list_no_combo,
    .env$species == tolower(.data$english) |
      .env$species == tolower(.data$french) |
      .env$species == tolower(paste(.data$genus, .data$species)) |
      .env$species == .data$aou)


  if(nrow(s_combo) == 0 & nrow(s_no_combo) == 0) {
    stop("Invalid species. Ensure `", nm, "` matches a ",
         "English or French species name or AOU code.\n",
         "See `search_species()` for a generic search.",
         call. = FALSE)
  } else if(nrow(s_no_combo) == 0 &
            !combine_species_forms & nrow(s_combo) == 1){
    stop("`combine_species_forms = FALSE` but '",
         s_combo$english, "' is a combined form...\nif you want this species, ",
         "set `combine_species_forms = TRUE`", call. = FALSE)
  } else if(nrow(s_combo) == 0 & combine_species_forms & nrow(s_no_combo) == 1){
    stop("`combine_species_forms = TRUE` but '",
         s_no_combo$english, "' is an unidentified,\nnon-combined form...",
         "if you want this species, set ",
         "`combine_species_forms = FALSE`", call. = FALSE)
  } else if((combine_species_forms & nrow(s_combo) > 1) ||
            (!combine_species_forms & nrow(s_no_combo) > 1)) {
    stop("Multiple species matched. ",
         "See `search_species()`for a generic search.", call. = FALSE)
  }
  if(combine_species_forms) s <- s_combo else s <- s_no_combo

  if(!quiet) message("Filtering to species ", s$english, " (", s$aou, ")")
  s$aou
}

check_spatial <- function(spatial_data, strata) {

  # Check for correct data
  check_data(spatial_data)

  # Check for matching strata
  s <- spatial_data$strata_meta$strata_name
  if(!all(strata %in% s) | !all(s %in% strata)) {
    stop("The strata in `prepped_data` and `spatial_data` don't match.\n",
         "`prepare_spatial()` should have been run with the same ",
         "`prepped_data` as `run_model()`.", call. = FALSE)
  }
}

#' Check user supplied sf object
#'
#' Quietly passes `NULL` objects through
#'
#' @param sf sf spatial data frame
#'
#' @noRd
check_sf <- function(sf, check_poly = FALSE, col = FALSE) {
  if(!is.null(sf)) {
    nm <- deparse(substitute(sf))

    # Check type and not empty
    if(!inherits(sf, "sf")) {
      stop("`", nm, "` must be an 'sf' spatial data frame", call. = FALSE)
    } else if(nrow(sf) == 0) {
      stop("Empty spatial data frame (`", nm, "`)", call. = FALSE)
    }

    # Check for invalid properties
    if(any(!sf::st_is_valid(sf))) {
      stop("Invalid spatial properties found ",
           "(not all `sf::st_is_valid(", nm, ")` are TRUE\n",
           "Consider using `sf::st_make_valid()` first.", call. = FALSE)
    }

    # Check for correct column names
    if(col && !"strata_name" %in% names(sf)) {
      stop("`", nm, "` missing column `strata_name`", call. = FALSE)
    }

    # Check for feature types
    if(check_poly) {
      geo_type <- get_geo_types(sf)
      if(!all(geo_type == "POLYGON")) {
        stop("Spatial data (`", nm, "`) must be comprised of ",
             "(MULTI)POLYGONS only", call. = FALSE)
      }
    }
  }
}

check_rnaturalearth <- function() {
  if(!requireNamespace("rnaturalearth", quietly = TRUE)){
    stop("The 'rnaturalearth' package is required for this function.\n",
         "You can install with: `install.packages('rnaturalearth')`",
         call. = FALSE)
  }
}


check_type <- function(msg, check_fun, allow_null, ...) {
  args <- list(...)
  if(is.null(names(args))) {
    names(args) <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = "a")
  }
  ck <- vapply(args, check_fun, FUN.VALUE = TRUE)
  if(allow_null) ck <- ck | vapply(args, is.null, FUN.VALUE = TRUE)
  if(!all(ck)) {
    stop("`", paste0(names(ck[!ck]), collapse = "`, `"),
         "` must be ", msg, call. = FALSE)
  }
}

check_logical <- function(..., allow_null = FALSE) {
  check_type(msg = "logical (TRUE/FALSE)", check_fun = is.logical, allow_null,
             ...)
}

check_numeric <- function(..., allow_null = FALSE) {
  check_type(msg = "a number", check_fun = is.numeric, allow_null, ...)
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

check_range <- function(arg, range) {
  if(!all(arg >= range[1] & arg <= range[2])) {
    stop("`", deparse(substitute(arg)),"` ",
         "must be range between ", range[1], " and ", range[2],
         call. = FALSE)
  }
}

