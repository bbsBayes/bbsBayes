inits <- function()
{
  list(B.X = mcmc.params$B.X,
       Elambda = mcmc.params$Elambda,
       STRATA = mcmc.params$STRATA,
       beta.X = mcmc.params$beta.X,
       eta = mcmc.params$eta,
       fcount = mcmc.params$fcount,
       logtauobs = mcmc.params$logtauobs,
       mulogtauobs = mcmc.params$mulogtauobs,
       nu = mcmc.params$nu,
       obs = mcmc.params$obs,
       sdbeta = mcmc.params$sdbeta,
       strata = mcmc.params$strata,
       tauX = mcmc.params$tauX,
       taulogtauobs = mcmc.params$taulogtauobs,
       taunoise = mcmc.params$taunoise,
       taustrata = mcmc.params$taustrata,
       tauyear = mcmc.params$tauyear)
}

runModel <- function(data.jags, initVals, sp.params, mod,
                     nChains, nAdapt, nIter, nBurnin, nThin, parallel)
{
  return(jags(data = data.jags,
              inits = initVals,
              parameters.to.save = sp.params,
              model.file = mod,
              n.chains = nChains,
              n.adapt = nAdapt,
              n.iter = nIter + nBurnin,
              n.burnin = nBurnin,
              n.thin = nThin,
              parallel = parallel))
}
