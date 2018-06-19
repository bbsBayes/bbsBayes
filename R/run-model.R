#' Run JAGS model for prepared species data
#'
#' \code{run-model} runs a JAGS model as specified by the user for
#'   the species of interest
#'
#' @return jagsUI object
#'
#' @export
#'
run_model <- function(data,
                      init_vals = NULL,
                      params = c("n"),
                      model,
                      n_chains = 3,
                      n_adapt = 500,
                      num_saved_steps = 2000,
                      n_burnin = 20000,
                      n_thin = 10,
                      n_iter = ceiling( ( num_saved_steps * n_thin ) / n_chains ),
                      parallel = FALSE)
{
  return(jags(data = data,
              inits = init_vals,
              parameters.to.save = params,
              model.file = system.file("models",models[[model]],package="bbsBayes"),
              n.chains = n_chains,
              n.adapt = n_adapt,
              n.iter = n_iter + n_burnin,
              n.burnin = n_burnin,
              n.thin = n_thin,
              parallel = parallel))
}
