#' Automatically run a BBS analysis for a given species
#'
#' \code{auto_bbs} generates MCMC chains for a list of species using BBS
#'   data.
#'
#' @param species Character strings or vector of character strings of what
#'   species are wanting to be analysed
#' @param model Character strings or vector of character strings of what
#'   species are wanting to be analysed.
#' @param bbs_data Large list of BBS data as downloaded using \code{fetchBBSdata()}.
#'   If this argument is left blank, \code{autoBBS()} will default to downloading
#'   the latest data from the USGS website.
#' @param stratify_by Character string of how to stratify the BBS data
#' @param output_dir Path to directory where output should be directed to.
#'   Defaults to current directory.
#' @param inits Optional list of initialization values for JAGS model.
#'   If none are specified, the JAGS model will generate its own
#'   initial values.
#' @param params Character vector of parameters to monitor in JAGS. Defaults
#'   to just monitoring "n"
#' @param adapt_steps Optional integer specifying the number of steps to
#'   adapt the JAGS model. It is recommended to do a minimum of 100,
#'   \code{autoBBS} will default to 500.
#' @param burn_in_steps Optional integer specifying the number of iterations
#'   to burn in the model. Defaults to 20000 per chain.
#' @param n_chains Optional number of chains to run. Defaults to 3.
#' @param num_saved_steps Optional number of steps to save per chain.
#'   Defaults to 2000.
#' @param thin_steps Optional number of steps to thin or discard.
#' @param n_iter Optional number of iterations.
#' @param run_parallel_chains Optional boolean value to specify whether
#'   the chains are run in parallel. By default, the chains are not
#'   run in parallel. If TRUE, the number of cores used will be the
#'   minimum of number of chains and number of available cores.
#'   JAGS console output is supressed with run in parallel.
#'
#' @return Some sort of output with cool stuff. Don't know yet
#'
#' @export
#'
auto_bbs <- function(species = NULL,
                    model = NULL,
                    bbs_data = NULL,
                    stratify_by = "bbs",
                    output_dir = NULL,
                    inits = NULL,
                    params = c("n"),
                    adapt_steps = 500,
                    burn_in_steps = 20000,
                    n_chains = 3,
                    num_saved_steps = 2000,
                    thin_steps = 10,
                    n_iter = ceiling( ( num_saved_steps * thin_steps ) / n_chains ),
                    run_parallel_chains = FALSE)
{
  process_auto_bbs_input(species, model, output_dir)

  if (is.null(bbs_data))
  {
    bbs_data <- fetch_bbs_data()
  }
  species_list <- bbs_data$species

  data_strat <- stratify(bbs_data, stratify_by)

  sp_num <- 1
  total_sp <- length(species)
  for (sp in species)
  {
    cat(paste("Prepping data for species ",
              sp_num,
              " of ",
              total_sp,
              ": ",
              sp,
              "\n",
              sep = ""))
    #cat(paste(sp, model, date(),"\n")) # output information about run and time
    data_jags <- prepare_jags_data(data_strat,
                                 sp,
                                 model)

    cat("Beginning JAGS model.\n")
    jagsjob <- run_model(data = data_jags,
                            init_vals = NULL,
                            params = params,
                            model = model,
                            n_chains = n_chains,
                            n_adapt = adapt_steps,
                            n_iter = n_iter,
                            n_burnin = burn_in_steps,
                            n_thin = thin_steps,
                            parallel = run_parallel_chains)
    sp_num <- sp_num + 1
  }
}
