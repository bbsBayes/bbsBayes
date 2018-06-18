runModel <- function(data, initVals, params, mod,
                     nChains, nAdapt, nIter, nBurnin, nThin, parallel)
{
  return(jags(data = data,
              inits = initVals,
              parameters.to.save = params,
              model.file = system.file("models",mod,package="bbsBayes"),
              n.chains = nChains,
              n.adapt = nAdapt,
              n.iter = nIter + nBurnin,
              n.burnin = nBurnin,
              n.thin = nThin,
              parallel = parallel))
}
