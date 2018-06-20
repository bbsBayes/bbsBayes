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
  jags <- jags.model(file = system.file("models",models[[model]],package="bbsBayes"),
                     data = data,
                     inits = init_vals,
                     n.chains = n_chains,
                     n.adapt = n_adapt)

  if (n_burnin > 0)
  {
    cat(paste("Burning in for", n_burnin, "iterations.\n"))
    update(object = jags,
           n.iter = n_burnin)
  }

  cat("Sampling MCMC chain.\n")
  mcmc_samples <- coda.samples(model = jags,
                               variable.names = params,
                               n.iter = n_iter,
                               thin = n_thin)


  return(list(jags_model = jags,
              mcmc_samples = mcmc_samples))
}
