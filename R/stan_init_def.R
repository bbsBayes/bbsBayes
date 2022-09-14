

init_def <- function(){
  list(noise_raw = rnorm(stan_data$ncounts*stan_data$use_pois,0,0.1),
       strata_raw = rnorm(stan_data$nstrata,0,0.1),
       STRATA = 0,
       nu = 10,
       sdstrata = runif(1,0.01,0.1),
       eta = 0,
       yeareffect_raw = matrix(rnorm(stan_data$nstrata*stan_data$nyears,0,0.1),nrow = stan_data$nstrata,ncol = stan_data$nyears),
       obs_raw = rnorm(stan_data$nobservers,0,0.1),
       ste_raw = rnorm(stan_data$nsites,0,0.1),
       sdnoise = runif(1,0.3,1.3),
       sdobs = runif(1,0.01,0.1),
       sdste = runif(1,0.01,0.2),
       sdbeta = runif(1,0.01,0.1),
       sdyear = runif(stan_data$nstrata,0.01,0.1),
       BETA = rnorm(1,0,0.1),
       beta_raw = rnorm(stan_data$nstrata,0,0.1))
}
