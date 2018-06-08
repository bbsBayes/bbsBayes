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
                    outputDir = NULL,
                    inits = NULL,
                    adaptSteps = 500,
                    burnInSteps = 20000,
                    nChains = 3,
                    numSavedSteps = 2000,
                    thinSteps = 10,
                    nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ),
                    runParallelChains = FALSE)
{
  processAutoBBSInput(species, model, outputDir)

  cat("Cleaning data...")
  data.cleaned <- cleanData()
  cat("done!\n")

  speciesIndex <- getSpeciesIndex(data.cleaned$species,
                                  species)

  spNum <- 1
  totalSp <- length(species)
  for (index in speciesIndex)
  {
    cat(paste("Species ", spNum, "/", totalSp, ": ", sep = ""))
    data.prep <- speciesDataPrep(data.cleaned$species,
                                 data.cleaned$unmod.sp,
                                 data.cleaned$sptorun,
                                 data.cleaned$sptorun2,
                                 index, model, outputDir)

    data.jags <- list(ncounts = nrow(data.prep$spsp.f),
                      nstrata=length(unique(data.prep$spsp.f$strat)),
                      ymin = data.prep$ymin,
                      ymax = data.prep$ymax,
                      nonzeroweight = data.prep$pR.wts$p.r.ever,
                      count = as.integer(data.prep$spsp.f$count),
                      strat = as.integer(data.prep$spsp.f$strat),
                      obser = as.integer(data.prep$spsp.f$obser),
                      year = data.prep$spsp.f$year,
                      firstyr = data.prep$spsp.f$firstyr,
                      nobservers = data.prep$nobservers)
    if (tolower(model) == "standard")
    {
      data.jags <- c(data.jags, list(fixedyear = midyear))
    }

    sp.params = c("sdbeta",
                  "strata",
                  "STRATA",
                  "sdstrata",
                  "sdobs",
                  "obs",
                  "n",
                  "posdiff",
                  "sdnoise",
                  "eta",
                  "overdisp",
                  "nfzero",
                  "gof",
                  "fgof",
                  "diffgof")

    jagsjob <- runModel(data.jags,
                            NULL,
                            sp.params,
                            mod = standard,
                            nChains,
                            adaptSteps,
                            nIter,
                            burnInSteps,
                            thinSteps,
                            runParallelChains)
    save(jagsjob, file = paste(data.prep$dir, "/jags.Rdata", sep=""))
    spNum <- spNum + 1
  }
}
