
#' Convergence metrics
#'
#' @param model_fit
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
#' d <- prepare_data(s, species = "Pacific Wren",
#'                   min_year = 2009,
#'                   max_year = 2018)
#'
#' # Now run the model (fast but not good, just for illustration)
#' m <- run_model(d, model = "first_diff",
#'                iter_sampling = 20, iter_warmup = 20, chains = 2)
#'
#' # Calculate convergence metrics on each variable
#' conv <- convergence(m)
#'

convergence <- function(model_output) {

  model_fit <- model_output$model_fit

  # Calculate convergence metrics on *each* variable
  dplyr::tibble(variable = posterior::variables(draws)) %>%
    dplyr::mutate(d = purrr::map(variable, ~model_fit$draws(variables = .x))) %>%
    dplyr::mutate(
      ess_bulk = purrr::map_dbl(d, posterior::ess_bulk),
      ess_tail = purrr::map_dbl(d, posterior::ess_tail),
      rhat = purrr::map_dbl(d, posterior::rhat),
      variable_type = stringr::str_remove(variable, "\\[[0-9]+\\]$")) %>%
    dplyr::select("variable_type", "variable", "ess_bulk", "ess_tail", "rhat")
}

