
#' Convergence metrics
#'
#'
#' @inheritParams common_docs
#'
#' @return Data frame of convergence metrics for all model variables. Contains
#'   `variable_type, `variable`, `ess_bulk`, `ess_tail`, and `rhat`
#' @export
#'
#' @examples
#'
#' # Using the example model for Pacific Wrens
#'
#' conv <- get_convergence(pacific_wren_model)
#' conv <- get_convergence(pacific_wren_model, variables = "strata_raw")
#' conv <- get_convergence(pacific_wren_model, variables = "strata_raw[9]")

get_convergence <- function(model_output, variables = NULL) {

  check_data(model_output)

  draws <- model_output$model_fit$draws(variables)

  # Calculate convergence metrics on *each* variable
  dplyr::tibble(variable = posterior::variables(draws)) %>%
    dplyr::mutate(d = purrr::map(
      .data$variable, ~posterior::extract_variable_matrix(.env$draws, .x))) %>%
    dplyr::mutate(
      rhat = purrr::map_dbl(.data$d, posterior::rhat),
      ess_bulk = purrr::map_dbl(.data$d, posterior::ess_bulk),
      ess_tail = purrr::map_dbl(.data$d, posterior::ess_tail),
      variable_type = stringr::str_extract(.data$variable, "^\\w+")) %>%
    dplyr::select("variable_type", "variable", "rhat", "ess_bulk", "ess_tail")
}

#' Get model variables
#'
#' Returns the basic model variables types (note that most variables have
#' different iterations for each strata and each year).
#'
#' @param all Logical. Whether or not to return **all**, specific variables
#'   (e.g., `strata_raw[1]` or just variable types (e.g., `strata_raw`).
#'   Defaults to `FALSE` (variable types only).
#'
#' @inheritParams common_docs
#'
#' @return A character vector of all model variable types.
#' @export
#'
#' @examples
#'
#' # Using the example model for Pacific Wrens...
#'
#' # List variable types
#' get_model_vars(pacific_wren_model)
#'
#' # List all variables
#' get_model_vars(pacific_wren_model, all = TRUE)
#'
get_model_vars <- function(model_output, all = FALSE) {

  check_data(model_output)

  v <- model_output$model_fit$draws() %>%
    posterior::variables()

  if(!all) v <- unique(stringr::str_extract(v, "^\\w+"))

  v
}

#' Return the `cmdstanr` summary
#'
#' Extract and return the model summary using `cmdstanr::summary()`.
#'
#'
#' @inheritParams common_docs
#'
#' @return A data frame of model summary statistics
#' @export
#'
#' @examples
#'
#' # Using the example model for Pacific Wrens
#'
#' get_summary(pacific_wren_model)
#' get_summary(pacific_wren_model, variables = "strata_raw")
#' get_summary(pacific_wren_model, variables = "strata_raw[9]")
#'
get_summary <- function(model_output, variables = NULL) {

  check_data(model_output)

  model_output$model_fit$summary(variables)
}
