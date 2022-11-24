
#' Convergence metrics
#'
#' @inheritParams common_docs
#'
#' @return Data frame of convergence metrics for all model variables. Contains
#'   `variable_type, `variable`, `ess_bulk`, `ess_tail`, and `rhat`
#' @export
#'
#' @examples
#'
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in modelling
#' d <- prepare_data(s, min_year = 2009, max_year = 2018)
#'
#' # Now run the model (fast but not good, just for illustration)
#' m <- run_model(d, model = "first_diff",
#'                iter_sampling = 5, iter_warmup = 5, chains = 2)
#'
#' # Calculate convergence metrics on each variable, a variable type, or on a
#' # single specific variable
#' conv <- get_convergence(m)
#' conv <- get_convergence(m, variables = "strata_raw")
#' conv <- get_convergence(m, variables = "strata_raw[9]")
#'

get_convergence <- function(model_output, variables = NULL) {

  draws <- model_output$model_fit$draws(variables)

  # Calculate convergence metrics on *each* variable
  dplyr::tibble(variable = posterior::variables(draws)) %>%
    dplyr::mutate(d = purrr::map(
      variable, ~posterior::extract_variable_matrix(draws, .x))) %>%
    dplyr::mutate(
      rhat = purrr::map_dbl(d, posterior::rhat),
      ess_bulk = purrr::map_dbl(d, posterior::ess_bulk),
      ess_tail = purrr::map_dbl(d, posterior::ess_tail),
      variable_type = stringr::str_extract(variable, "^\\w+")) %>%
    dplyr::select("variable_type", "variable", "rhat", "ess_bulk", "ess_tail")
}

#' Get model variables
#'
#' Returns the basic model variables types (note that most variables have
#' different iterations for each strata and each year).
#'
#' @inheritParams common_docs
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in modelling
#' d <- prepare_data(s, min_year = 2009, max_year = 2018)
#'
#' # Now run the model (fast but not good, just for illustration)
#' m <- run_model(d, model = "first_diff",
#'                iter_sampling = 5, iter_warmup = 5, chains = 2)
#'
#' # Calculate convergence metrics on each variable
#' get_model_vars(m)
#'
get_model_vars <- function(model_output, all = FALSE) {
  v <- model_output$model_fit$draws() %>%
    posterior::variables()

  if(!all) v <- unique(stringr::str_extract(v, "^\\w+"))

  v
}

#' Return the `cmdstanr` summary
#'
#' Extract and return the model summary using `cmdstanr::summary()`.
#'
#' @param variables Character vector. Specific variables (e.g., "strata_raw[1]")
#'   or variable types (e.g., "strata_raw") to include.
#'
#' @inheritParams common_docs
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in modelling
#' d <- prepare_data(s, min_year = 2009, max_year = 2018)
#'
#' # Now run the model (fast but not good, just for illustration)
#' m <- run_model(d, model = "first_diff",
#'                iter_sampling = 5, iter_warmup = 5, chains = 2)
#'
#' # Calculate convergence metrics on each variable, a variable type, or on a
#' # single specific variable
#' get_summary(m)
#' get_summary(m, variables = "strata_raw")
#' get_summary(m, variables = "strata_raw[9]")
#'
get_summary <- function(model_output, variables = NULL) {
  model_output$model_fit$summary(variables)
}
