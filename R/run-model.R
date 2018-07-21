#' Run JAGS model for prepared species data
#'
#' \code{run-model} runs a JAGS model as specified by the user for
#'   the species of interest
#'
#' @param jags_data List or environment containing the data to model, as output
#'   by \code{prepare_jags_data}
#' @param model Character string of the model that should be run.
#'   Note that this should be the same model used by \code{prepare_jags_data}
#' @param inits Optional list of initialization values for JAGS model.
#'   If none are specified, the JAGS model will generate its own
#'   initial values.
#' @param variable_names Character vector of parameters to monitor in JAGS. Defaults
#'   to just monitoring "n"
#' @param n_chains Optional number of chains to run. Defaults to 3.
#' @param n_adapt Optional integer specifying the number of steps to
#'   adapt the JAGS model. It is recommended to do a minimum of 100,
#'   \code{run-model} will default to 500.
#' @param n_burnin Optional integer specifying the number of iterations
#'   to burn in the model. Defaults to 20000 per chain.
#' @param n_thin Optional number of steps to thin or discard.
#' @param n_iter Optional number of iterations.
#' @param n_saved_steps Optional number of steps to save per chain.
#'   Defaults to 2000.
#' @param ... Additional arguments
#' @return
#'   \item{jags_job}{Object created by jagsUI containing coda samples and summary statistics.}
#'
#' @importFrom jagsUI jags
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Download BBS data, stratify, and prepare data for a JAGS run with Standard model
#' bbs_data <- fetch_bbs_data()
#' data_stratified <- stratify(bbs_data)
#' data_jags <- prepare_jags_data(data = data_stratified,
#'                                species_to_run = "Bufflehead",
#'                                model = "standard")
#'
#' # Run a JAGS model with the Standard BBS model using the default JAGS arguments
#' jags_mod <- run_model(jags_data = data_jags, model = "standard")
#'
#' # You can specify how many chains to run, how many adaptation interations,
#' # how many burn in iterations, how many sampling interations, and how much\
#' # thinning.
#' jags_mod <- run_model(jags_data = data_jags,
#'                       model = "standard",
#'                       n_chains = 2,
#'                       n_adapt = 200,
#'                       n_burnin = 5000,
#'                       n_thin = 10,
#'                       n_iter = 10000)
#'
#' # You can specify a vector of variable names to monitor. By default, the
#' # variable "n" is monitored to produce trends
#' jags_mod <- run_model(jags_data = data_jags,
#'                       model = "standard",
#'                       variable_names = c("n", "strata"))
#'
#' # If you used \code{prepare_jags_data} to create the data sent to \code{run_model},
#' # you don't actually have to specify the model again, as this information is
#' # saved in the output for \code{prepare_jags_data}. The following code works:
#' jags_mod <- run_model(jags_data = data_jags)
#'
#' # If you happen to use a different function to prepare the JAGS data, you will
#' # need to specify the model. The following code will not work.
#' data_jags_other <- some_other_preparation_function(...)
#' jags_mod <- run_model(jags_data = data_jags_other)
#'
#' # If you specify one model in \code{prepare_jags_data} and specify a different
#' # model in \code{run_model}, the function will default to using the model specified
#' # in \code{prepare_jags_data}, with a warning.
#' data_jags <- prepare_jags_data(data = data_stratified,
#'                                species_to_run = "Bufflehead",
#'                                model = "standard")
#' jags_mod <- run_model(jags_data = data_jags, model = "firstdifference")
#' }
#'
run_model <- function(jags_data = NULL,
                      model = NULL,
                      inits = NULL,
                      variable_names = c("n"),
                      n_chains = 3,
                      n_adapt = 500,
                      n_burnin = 20000,
                      n_thin = 10,
                      n_saved_steps = 2000,
                      n_iter = ceiling( ( n_saved_steps * n_thin ) / n_chains ),
                      ...)
{
  # The case where the user does their own data prep. Rare, but possible
  if (is.null(model) & is.null(jags_data[["model"]]))
  {
    stop("No model specified.")
  }

  # If the user happens to specify a model for this function, check that they
  # are the same. The data was prepped for a certain model so that one has to
  # be used anyway.
  if (!is.null(model))
  {
    if (models[[model]] != models[[jags_data[["model"]]]])
    {
      warning("Model supplied does not match model used in JAGS preparation.")
    }
  }
  model <- jags_data[["model"]]
  jags_data[["model"]] <- NULL

  strata_used <- jags_data[["strat_name"]]
  jags_data[["strat_name"]] <- NULL

  jags_job <- jags(data = jags_data,
                   inits = inits,
                   parameters.to.save = variable_names,
                   model.file = system.file("models",models[[model]],package="bbsBayes"),
                   n.chains = n_chains,
                   n.adapt = n_adapt,
                   n.burnin = n_burnin,
                   n.iter = n_iter,
                   n.thin = n_thin)

  jags_job$strat_name <- strata_used

  return(jags_job)
}
