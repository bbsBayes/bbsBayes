
#' Convergence metrics
#'
#' @param model_fit
#'
#' @return
#' @export
#'
#' @examples
#'
#' load(system.file("model_outputs", "pacific_wren_slope_BBS_fit.RData", package = "bbsBayes"))
#'
convergence <- function(model_fit) {

  draws <- model_fit$draws(variables = "strata")

  vars <- posterior::variables(draws)

  draws <- model_fit$draws(variables = stringr::str_subset(vars, "strata_raw"))

  model_fit$summary()

  conv <- data.frame(variable = c("yeareffect", "strata")) %>%
    dplyr::mutate(v = purrr::map(variable, ~stringr::str_subset(vars, paste0(.x, "_raw"))),
                  d = purrr::map(v, ~model_fit$draws(variables = .x))) %>%
    dplyr::mutate(ess_bulk = purrr::map(d, posterior::ess_bulk),
                  ess_tail = purrr::map(d, posterior::ess_tail),
                  rhat = purrr::map(d, posterior::rhat))


}

