#' Sample point count data per species (Pacific Wren only)
#'
#' A sample dataset containing 10-stop counts of each bird species seen per route per year.
#'   NOTE: This only contains data for Pacific Wren, not the full data set.
#'   The full count set is obtained via the function \code{fetch_bbs_data}.
#'   The data is obtained from the United States Geological Survey and is
#'   subject to change as new data is added each year. See Details for citation.
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding, V. Aponte and M-A.R. Hudson. 2019.
#'   North American Breeding Bird Survey Dataset 1966 - 2018, version 2018.0.
#'   U.S. Geological Survey, Patuxent Wildlife Research Center. https://doi.org/10.5066/P9HE8XYJ.
#'
#'
"bird_sample"

#' Sample route data per year run (Pacific Wren only)
#'
#' A dataset containing data for each route run per year.
#'   NOTE: This only contains data for Pacific Wren, not the full data set.
#'   The full count set is obtained via the function \code{fetch_bbs_data}.
#'   The data is obtained from the United States Geological Survey and is
#'   subject to change as new data is added each year. See Details for citation.
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding, V. Aponte and M-A.R. Hudson. 2019.
#'   North American Breeding Bird Survey Dataset 1966 - 2018, version 2018.0.
#'   U.S. Geological Survey, Patuxent Wildlife Research Center. https://doi.org/10.5066/P9HE8XYJ.
#'
#'
"route_sample"

#' Sample North American bird species list (Pacific Wren only)
#'
#' A dataset containing species list of North America.
#'   NOTE: This only contains data for Pacific Wren, not the full data set.
#'   The full count set is obtained via the function \code{fetch_bbs_data}.
#'   The data is obtained from the United States Geological Survey and is
#'   subject to change as new data is added each year. See Details for citation.
#'
#' Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding, V. Aponte and M-A.R. Hudson. 2019.
#'   North American Breeding Bird Survey Dataset 1966 - 2018, version 2018.0.
#'   U.S. Geological Survey, Patuxent Wildlife Research Center. https://doi.org/10.5066/P9HE8XYJ.
#'
"species_sample"


#' Sample BBS data
#'
#' Contains only Pacific Wren data
#'
"bbs_data_sample"

#' List of models available
#'
"bbs_models"

#' List of established strata
#'
#' Each list item contains a data frame describing the strata for that
#' stratification.
#'
#' Contains `bbs_usgs`, `bbs_cws`, `bcr`, `latlong` and `prov_state`
"bbs_strata"

#' Example model output
#'
#' Pacific wren model output
#'
"pacific_wren_model"

#' Species forms
#'
#' Data frame of species forms that will be combined if `combine_species_forms`
#' is `TRUE` in `stratify()`.
"species_forms"
