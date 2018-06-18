#' Automatically run a BBS analysis for a given species
#'
#' \code{autoBBS} generates MCMC chains for a list of species using BBS
#'   data.
#'
#' @param species Character strings or vector of character strings of what
#'   species are wanting to be analysed
#' @param model Character strings or vector of character strings of what
#'   species are wanting to be analysed.
#' @param outputDir Path to directory where output should be directed to.
#'   Defaults to current directory.
#' @param inits Optional list of initialization values for JAGS model.
#'   If none are specified, the JAGS model will generate its own
#'   initial values.
#' @param adaptSteps Optional integer specifying the number of steps to
#'   adapt the JAGS model. It is recommended to do a minimum of 100,
#'   \code{autoBBS} will default to 500.
#' @param burnInSteps Optional integer specifying the number of iterations
#'   to burn in the model. Defaults to 20000 per chain.
#' @param nChains Optional number of chains to run. Defaults to 3.
#' @param numSavedSteps Optional number of steps to save per chain.
#'   Defaults to 2000.
#' @param thinSteps Optional number of steps to thin or discard.
#' @param nIter Optional number of iterations.
#' @param runParallelChains Optional boolean value to specify whether
#'   the chains are run in parallel. By default, the chains are not
#'   run in parallel. If TRUE, the number of cores used will be the
#'   minimum of number of chains and number of available cores.
#'   JAGS console output is supressed with run in parallel.
#' @return The sum of \code{x} and \code{y}
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
autoBBS <- function(species = NULL,
                    model = NULL,
                    bbs.data = NA,
                    stratify.by = "bbs",
                    outputDir = NULL,
                    inits = NULL,
                    params = c("n"),
                    adaptSteps = 500,
                    burnInSteps = 20000,
                    nChains = 3,
                    numSavedSteps = 2000,
                    thinSteps = 10,
                    nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ),
                    runParallelChains = FALSE)
{
  processAutoBBSInput(species, model, outputDir)

  if (is.na(bbs.data))
  {
    bbs.data <- fetchBBSdata()
  }
  bird <- bbs.data$bird
  route <- bbs.data$route
  routes <- bbs.data$routes
  weather <- bbs.data$weather
  speciesList <- bbs.data$species
  remove(bbs.data)

  cat("Stratify...\n")
  data.strat <- stratify(bird, route, routes, stratify.by)

  spNum <- 1
  totalSp <- length(species)
  for (sp in species)
  {
    cat(paste("Species ", spNum, "/", totalSp, ": ", sep = ""))
    cat(paste(sp, model, date(),"\n")) # output information about run and time

    dir <- createRunDirectory(outputDir, sp, model)

    data.jags <- speciesDataPrep(sp,
                                 model,
                                 dir,
                                 data.strat$birds,
                                 data.strat$route,
                                 data.strat$st.areas,
                                 speciesList)

    jagsjob <- runModel(data = data.jags,
                            initVals = NULL,
                            params = params,
                            mod = models[[model]],
                            nChains = nChains,
                            nAdapt = adaptSteps,
                            nIter = nIter,
                            nBurnin = burnInSteps,
                            nThin = thinSteps,
                            parallel = runParallelChains)
    save(jagsjob, file = paste(dir, "/jags.Rdata", sep=""))
    spNum <- spNum + 1
  }
}
