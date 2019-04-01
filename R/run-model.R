#' Run JAGS model for prepared species data
#'
#' \code{run-model} runs a JAGS model as specified by the user for
#'   the species of interest
#'
#' @param jags_data List or environment containing the data to model, as output
#'   by \code{prepare_jags_data}
#' @param model_file_path Path to custom model. Overrides the \code{model}
#'   variable set by \code{prepare_jags_data}
#' @param inits Optional list of initialization values for JAGS model.
#'   If none are specified, the JAGS model will generate its own
#'   initial values.
#' @param parameters_to_save Character vector of parameters to monitor in JAGS. Defaults
#'   to just monitoring "n"
#' @param dont_track_n Force \code{run_model} to not track "n".
#'   By default, the parameter "n" will always be tracked, even if the user
#'   forgets to specify it. However, if the user is positive they do not want
#'   to track "n", this parameter can be be set to \code{TRUE}. NOTE: you will
#'   not be able to generate annual indices if "n" is not tracked.
#' @param n_chains Optional number of chains to run. Defaults to 3.
#' @param n_adapt Optional integer specifying the number of steps to
#'   adapt the JAGS model. It is recommended to do a minimum of 100,
#'   \code{run-model} will default to 500.
#' @param n_burnin Optional integer specifying the number of iterations
#'   to burn in the model. Defaults to 20000 per chain.
#' @param n_thin Optional number of steps to thin or discard.
#' @param n_iter Optional number of iterations per chain. Defaults to 10000.
#' @param n_saved_steps Optional number of steps to save per chain.
#'   Defaults to 2000.
#' @param parallel Should each chain be run parallel on separate cores?
#'   If TRUE, the number of cores used will be the minimum of the
#'   \code{n_chains} specified and the number of cores on your computer
#' @param quiet Should JAGS output be suppressed?
#' @param ... Additional arguments
#' @return jagsUI object
#'
#' @importFrom jagsUI jags
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Download BBS data, stratify, and prepare data for a JAGS run with Standard model
#' bbs_data <- fetch_bbs_data()
#' data_stratified <- stratify(bbs_data, stratify_by = "latlong")
#' data_jags <- prepare_jags_data(strat_data = data_stratified,
#'                                species_to_run = "Bufflehead",
#'                                model = "standard")
#'
#' # Run a JAGS model with default JAGS arguments
#' # Note that you do not have to specify the model file. This is saved
#' #   in the output from prepare_jags_data
#' jags_mod <- run_model(jags_data = data_jags)
#'
#' # You can specify how many chains to run, how many adaptation interations,
#' # how many burn in iterations, how many sampling interations, and how much\
#' # thinning.
#' jags_mod <- run_model(jags_data = data_jags,
#'                       n_chains = 2,
#'                       n_adapt = 200,
#'                       n_burnin = 5000,
#'                       n_thin = 10,
#'                       n_iter = 10000)
#'
#' # You can specify a vector of variable names to monitor. By default, the
#' #   variable "n" is monitored to produce trends
#' jags_mod <- run_model(jags_data = data_jags,
#'                       parameters_to_save = c("n", "strata"))
#'
#' # In fact, the variable "n" will always be tracked, even if you don't specify
#' #  it to be tracked. This ensures that trends can always be calculated in
#' #  case the user forgets to specify "n" if they went to specify other
#' #  variables to track. If, however, you are absolutely sure you do not
#' #  want to track "n", you can set the parameter "dont_track_n" to TRUE
#'
#' jags_mod <- run_model(jags_data = data_jags,
#'                       parameters_to_save = ("strata", "beta"),
#'                       dont_track_n = TRUE)
#'}
#'
run_model <- function(jags_data = NULL,
                      model_file_path = NULL,
                      inits = NULL,
                      parameters_to_save = c("n"),
                      dont_track_n = FALSE,
                      n_chains = 3,
                      n_adapt = 500,
                      n_burnin = 20000,
                      n_thin = 10,
                      n_saved_steps = 2000,
                      n_iter = 10000,
                      parallel = FALSE,
                      quiet = FALSE,
                      ...)
{
  if (is.null(jags_data))
  {
    stop("No data supplied for model."); return(NULL)
  }
  if (!is.null(model_file_path))
  {
    model <- model_file_path
    jags_data[["model"]] <- NULL
  }else{
    model <- jags_data[["model"]]
    model <- system.file("models",
                         models[[model]],
                         package="bbsBayes")
    jags_data[["model"]] <- NULL
  }

  strata_used <- jags_data[["strat_name"]]
  jags_data[["strat_name"]] <- NULL

  stratify_by <- jags_data[["stratify_by"]]
  jags_data[["stratify_by"]] <- NULL

  r_year <- jags_data[["r_year"]]
  jags_data[["r_year"]] <- NULL

  jags_data[["route"]] <- NULL

  # The case where the user DOES NOT want to track n
  if (isTRUE(dont_track_n))
  {
    # remove n from the list of parameters to save if it exists
    if ("n" %in% parameters_to_save)
    {
      parameters_to_save <- setdiff(parameters_to_save, "n")
    }
  }

  # The case where the user DOES want to track n
  if (!isTRUE(dont_track_n))
  {
    # Add n to the list of parameters to save if it does not yet exist
    if (!("n" %in% parameters_to_save))
    {
      parameters_to_save <- c(parameters_to_save, "n")
    }
  }

  jags_job <- jagsUI::jags(data = jags_data,
                           inits = inits,
                           parameters.to.save = parameters_to_save,
                           model.file = model,
                           n.chains = n_chains,
                           n.adapt = n_adapt,
                           n.burnin = n_burnin,
                           n.iter = n_iter + n_burnin,
                           n.thin = n_thin,
                           parallel = parallel,
                           verbose = !quiet)

  jags_job$strat_name <- strata_used
  jags_job$stratify_by <- stratify_by
  jags_job$r_year <- r_year

  return(jags_job)
}
