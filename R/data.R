#' Sample BBS data
#'
#' Contains only Pacific Wren data
#'
#' A sample dataset containing only data for Pacific Wrens for the 2022
#' state-level BBS data. The full count set is obtained via the function
#' `fetch_bbs_data()`. The data is obtained from the United States Geological
#' Survey and is subject to change as new data is added each year. See
#' References for citation.
#'
#' @format ## `bbs_data_sample`
#' A list containing:
#'  - `birds` - counts of each bird seen per route per
#'  - `routes` - data for each route run per year
#'  - `species` - species list of North America
#'
#' @source <https://www.sciencebase.gov/> via `fetch_bbs_data()`
#'
#' @references
#'
#' Ziolkowski Jr., D.J., Lutmerding, M., Aponte, V.I., and Hudson, M-A.R., 2022,
#'  North American Breeding Bird Survey Dataset 1966 - 2021: U.S. Geological
#'  Survey data release, https://doi.org/10.5066/P97WAZE5
#'
"bbs_data_sample"

#' Stan models included in bbsBayes
#'
#' These models are included in bbsBayes. The model files themselves can be
#' found by running `system.file("models", package = "bbsBayes")`.
#'
#' To create a custom Stan model, see `copy_model_file()` and the `model_file`
#' argument of `prepare_model()`. See also the [models
#' article](https://steffilazerte.ca/bbsBayes/articles/models.html) for more
#' details.
#'
#' @format ## `bbs_models`
#' A data frame with 9 rows and 3 columns:
#'
#' - `model` - Model type
#'
#'    - `first_diff` - First difference models
#'    - `gam` - General Additive Models (GAM)
#'    - `gamye` - General Additive Models (GAM) with Year Effect
#'    - `slope` - Slope models
#'
#' - `variant` - Variant of the model to run
#'
#'    - `nonhier` - Non-hierarchical models
#'       (only available for first difference models)
#'    - `hier` - Hierarchical models
#'    - `spatial` - Spatial models
#'
#' - `file` - Stan model file name
#'
"bbs_models"

#' List of included strata
#'
#' List of strata included in bbsBayes. Each list item contains a data frame
#' describing the strata for that stratification (name, area, country, etc.)
#'
#' @format ## `bbs_strata`
#' A list of 5 data frames
#'
#' Contains `bbs_usgs`, `bbs_cws`, `bcr`, `latlong` and `prov_state`
"bbs_strata"

#' Example model output
#'
#' Example model output from running a hierarchical first difference model
#' on the included sample data for Pacific Wrens.
#'
#' @format ## `pacific_wren_model`
#' A list output from `run_model()` with 4 items
#'
#' - `model_fit` - The output from `cmdstanr::sample()`
#' - `meta_data` - Meta data related to the model run
#' - `meta_strata` - Meta data related to the strata
#' - `raw_data` - Data compiled by `prepare_data()`
#'
"pacific_wren_model"

#' Species forms
#'
#' Species forms which will be combined if `combine_species_forms`
#' is `TRUE` in `stratify()`.
#'
#' @format ## `species_forms`
#' A data frame with 13 rows and 5 columns
#'
#' - `aou_unid` - The AOU id number which will identify the combined unidentified form
#' - `ensligh_original` - The English name of the original 'unidentified' form
#' - `english_combined` - The English name of the new 'combined' forms
#' - `french_combined` - The French name of the new 'combined' forms
#' - `aou_id` - The AOU id numbers of all the forms which will be combined
"species_forms"
