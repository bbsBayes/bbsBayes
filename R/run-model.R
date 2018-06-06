runModel <- function(data.jags, initVals, sp.params, mod,
                     nChains, nAdapt, nIter, nBurnin, nThin, parallel)
{
  return(jags(data = data.jags,
              inits = initVals,
              parameters.to.save = sp.params,
              model.file = system.file("models","standard.jags",package="bbsBayes"),
              n.chains = nChains,
              n.adapt = nAdapt,
              n.iter = nIter + nBurnin,
              n.burnin = nBurnin,
              n.thin = nThin,
              parallel = parallel))
}
