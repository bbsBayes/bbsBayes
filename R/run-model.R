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
#' @param track_n By default, the parameter "n" will always be tracked, even if the user
#'   forgets to specify it. However, if the user is positive they do not want
#'   to track "n", this parameter can be be set to \code{FALSE}. NOTE: you will
#'   not be able to generate annual indices if "n" is not tracked.
#' @param n_chains Optional number of chains to run. Defaults to 3.
#' @param n_adapt Optional integer specifying the number of steps to
#'   adapt the JAGS model.  The default is \code{NULL}, which will result in
#'   the function running groups of 100 adaptation iterations (to amax of 10,000)
#'   until JAGS reports adaptation is sufficient. If you set it manually,
#'   1000 is the recommended minimum value.
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
#' @param modules Character vector of JAGS modules to load before analysis. By default no extra modules are loaded (other than 'basemod' and 'bugs'). To force glm or other modules to load, use modules = "glm". Be warned, our experience suggests that including the glm module may cause problems with the BBS data.
#' @param ... Additional arguments
#' @return jagsUI object
#'
#' @importFrom jagsUI jags
#' @export
#'
#' @examples
#'
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#'
#' strat_data <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in a JAGS model.
#' jags_data <- prepare_jags_data(strat_data = strat_data,
#'                                species_to_run = "Pacific Wren",
#'                                model = "firstdiff",
#'                                min_year = 2009,
#'                                max_year = 2018)
#'
#' # Now run a JAGS model. For the sake of speed, we've adjusted
#' #   some arguments so that the JAGS model will not run any
#' #   adaptation steps (n_adapt = 0), no burnin steps (n_burnin = 0),
#' #   only 50 iterations per chain (n_iter = 50), and will not
#' #   thin the chain (n_thin = 1). This will produce several convergence
#' #   warnings, but we can ignore them for the sake of this toy example.
#'
#' jags_mod <- run_model(jags_data = jags_data,
#'                       n_adapt = 0,
#'                       n_burnin = 0,
#'                       n_iter = 10,
#'                       n_thin = 1,
#'                       parameters_to_save = c("n", "strata"))
#'

run_model_orig <- function(jags_data = NULL,
                      model_file_path = NULL,
                      inits = NULL,
                      parameters_to_save = c("n"),
                      track_n = TRUE,
                      n_chains = 3,
                      n_adapt = NULL,
                      n_burnin = 20000,
                      n_thin = 10,
                      n_saved_steps = 2000,
                      n_iter = 10000,
                      parallel = FALSE,
                      quiet = FALSE,
                      modules = NULL,
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
    jags_data[["heavy_tailed"]] <- NULL
  }else{
    model <- jags_data[["model"]]
    heavy_tailed <- jags_data[["heavy_tailed"]]
    jags_data[["heavy_tailed"]] <- NULL
    if(heavy_tailed)
      {
      model <- paste0(model,"_heavy")
    }
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
  jags_data[["basis"]] <- NULL
  jags_data[["month"]] <- NULL
  jags_data[["day"]] <- NULL

  # The case where the user DOES NOT want to track n
  if (isFALSE(track_n))
  {
    # remove n from the list of parameters to save if it exists
    if ("n" %in% parameters_to_save)
    {
      parameters_to_save <- setdiff(parameters_to_save, "n")
    }
  }

  # The case where the user DOES want to track n
  if (isTRUE(track_n))
  {
    # Add n to the list of parameters to save if it does not yet exist
    if (!("n" %in% parameters_to_save))
    {
      parameters_to_save <- c(parameters_to_save, "n")
    }
  }

  # Deprecated model parameter name check. Adding this in for the next few versions
  if ("n3" %in% parameters_to_save)
  {
    if (model %in% c("gam", "gamye", "gam_heavy", "gamye_heavy"))
    {
      warning("The parameter \"n3\" is now \"nsmooth\" in GAM models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }else if (model %in% c("slope", "slope_heavy"))
    {
      warning("The parameter \"n3\" is now \"nslope\" in slope models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("n4" %in% parameters_to_save)
  {
    if (model %in% c("gam", "gamye", "gam_heavy", "gamye_heavy"))
    {
      warning("The parameter \"n4\" is now \"nsmooth2\" in GAM models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }else if (model %in% c("slope", "slope_heavy"))
    {
      warning("The parameter \"n4\" is now \"nslope2\" in slope models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("yeareffect" %in% parameters_to_save)
  {
    if (model %in% c("gam", "gamye", "gam_heavy", "gamye_heavy"))
    {
      warning("The parameter \"yeareffect\" is now \"smooth\" in GAM models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("yy" %in% parameters_to_save)
  {
    if (model %in% c("gamye", "gamye_heavy"))
    {
      warning("The parameter \"yy\" is now \"yeareffect\" in GAMYE models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("tauyy" %in% parameters_to_save)
  {
    if (model %in% c("gamye", "gamye_heavy"))
    {
      warning("The parameter \"tauyy\" is now \"tauyeareffect\" in GAMYE models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("mulogtauyy" %in% parameters_to_save)
  {
    if (model %in% c("gamye", "gamye_heavy"))
    {
      warning("The parameter \"mulogtauyy\" is now \"mulogtauyeareffect\" in GAMYE models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("taulogtauyy" %in% parameters_to_save)
  {
    if (model %in% c("gamye", "gamye_heavy"))
    {
      warning("The parameter \"taulogtauyy\" is now \"taulogtauyeareffect\" in GAMYE models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("logtauyy" %in% parameters_to_save)
  {
    if (model %in% c("gamye", "gamye_heavy"))
    {
      warning("The parameter \"logtauyy\" is now \"logtauyeareffect\" in GAMYE models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("tauyear" %in% parameters_to_save)
  {
    if (model %in% c("slope", "slope_heavy"))
    {
      warning("The parameter \"tauyear\" is now \"tauyeareffect\" in Slope models as of bbsBayes v2.4.0.2020, and will be output as such.")
    }
  }
  if ("strata.p" %in% parameters_to_save)
  {
    if (model %in% c("firstdiff", "firstdiff_heavy"))
    {
      warning("The parameter \"strata.p\" is no longer used in the First Difference model and will be dropped.")
      parameters_to_save <- parameters_to_save[!parameters_to_save == "strata.p"]
    }
  }
  if ("mulogtauyear" %in% parameters_to_save)
  {
    if (model %in% c("firstdiff", "firstdiff_heavy"))
    {
      warning("The parameter \"mulogtauyear\" is no longer used in the First Difference model and will be dropped.")
      parameters_to_save <- parameters_to_save[!parameters_to_save == "mulogtauyear"]
    }
  }
  if ("taulogtauyear" %in% parameters_to_save)
  {
    if (model %in% c("firstdiff", "firstdiff_heavy"))
    {
      warning("The parameter \"taulogtauyear\" is no longer used in the First Difference model and will be dropped.")
      parameters_to_save <- parameters_to_save[!parameters_to_save == "taulogtauyear"]
    }
  }
  message("Console output messages for JAGS models are supplied by jagsUI package.")

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
                           verbose = !quiet,
                           modules = modules)

  jags_job$strat_name <- strata_used
  jags_job$stratify_by <- stratify_by
  jags_job$r_year <- r_year

  #### check the Rhat values for convergence failure
  #### if there are failures, then throw a warning
  #### alternatively, incoporate a logical option to automatically continue running the model until some minimum convergence criterion is met
  rhat_check = r_hat(jags_job,
                     threshold = 1.1)
  if(nrow(rhat_check) > 1){
    failed = paste(rhat_check$Parameter,collapse = " ; ")
    nfail = nrow(rhat_check)
   warning(paste("Warning",nfail,"parameters did not converged (Rhat > 1.1). Consider re-running with a longer burn-in and-or more posterior samples."))

    warning(paste("Convergence failure on the following parameters:",failed))
  }

  return(jags_job)
}


run_model <- function(model_data,
                      out_name = NULL,
                      out_dir = "./model_run",
                      n_chains = 3,
                      parallel_chains = 3,
                      iter_sampling = 1000,
                      iter_warmup = 1000) {


  # Check inputs

  # Prepare files and directory
  if(!dir.exists(out_dir)) {
    stop("'", out_dir, "' does not exist. Please create it first.",
         call. = FALSE)
  }

  if(is.null(out_name)) out_name <- paste0("BBS_STAN_",
                                           model_data$model, "_",
                                           Sys.Date())

  # Keep track of data
  meta_data <- list()
  meta_data[["stratify_by"]] <- model_data[["stratify_by"]]
  meta_data[["model"]] <- model_data[["model"]]
  meta_data[["alt_data"]] <- model_data[["alt_data"]]

  model_data[["stratify_by"]] <- NULL
  model_data[["model"]] <- NULL
  model_data[["alt_data"]] <- NULL

  model <- meta_data[["model"]]

  # Select model

  if(model == "slope") {
    model <- system.file("models", "slope_bbs_CV.stan", package = "bbsBayes")
  } else {
    # . . .
  }

  # Compile model
  model <- cmdstanr::cmdstan_model(model)

  # Get initial values
  init_def <- create_init_def(model, model_data, n_chains)

  # What here should be changeable? can use ... and reference cmdstanr docs...
  model_fit <- model$sample(
    data = model_data,
    refresh = 1,
    chains = n_chains,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    parallel_chains = parallel_chains,
    #pars = parms,
    adapt_delta = 0.95,
    max_treedepth = 14,
    seed = 123,
    init = init_def,
    output_dir = out_dir,
    output_basename = out_name)

  #message("Calculating run summary")
  #fit_summary <- stan_fit$summary()

  list("model_fit" = model_fit, "meta_data" = meta_data)
}



create_init_def <- function(model, model_data, n_chains) {

  model <- paste0(model[1], "_", model[2])

  # Generic --------------
  init_generic <-
    list(
      noise_raw  = rnorm(model_data$ncounts * model_data$use_pois, 0, 0.1),
      strata_raw = rnorm(model_data$nstrata, 0, 0.1),
      STRATA     = 0,
      nu         = 10,
      sdstrata   = runif(1, 0.01, 0.1),
      eta        = 0,
      obs_raw    = rnorm(model_data$nobservers, 0, 0.1),
      ste_raw    = rnorm(model_data$nsites, 0, 0.1),
      sdnoise    = runif(1,0.3,1.3),
      sdobs      = runif(1,0.01,0.1),
      sdste      = runif(1,0.01,0.2)
      )

  # By model -------------

  # Matrices
  m_yrs1 <-  function() matrix(rnorm(model_data$nyears * model_data$nstrata, 0, 0.1),
                               nrow = model_data$nstrata,
                               ncol = model_data$nyears)

  m_yrs2 <-  function() matrix(rnorm((model_data$nyears - 1) * model_data$nstrata, 0, 0.1),
                               nrow = model_data$nstrata,
                               ncol = model_data$nyears - 1)

  m_knots <- function() matrix(rnorm(model_data$nknots_year * model_data$nstrata, 0, 0.01),
                               nrow = model_data$nstrata,
                               ncol = model_data$nknots_year)

  # Vectors
  v_rand1 <-  function() runif(1, 0.01, 0.1)
  v_rand2 <-  function() rnorm(1,    0, 0.1)
  v_strat1 <- function() runif(model_data$nstrata,   0.01, 0.1)
  v_strat2 <- function() rnorm(model_data$nstrata,      0, 0.1)
  v_yrs <-    function() rnorm((model_data$nyears - 1), 0, 0.1)
  v_knots <-  function() rnorm(model_data$nknots_year,  0, 0.1)

  # Initial defs
  init_specific <- dplyr::tribble(
    ~yeareffect_raw, ~sdyear,  ~sdbeta,  ~sdBETA, ~BETA_raw, ~beta_raw, ~BETA,
    "",              "",       v_strat1, "",      "",        m_yrs2,    "",     # first diff non-hier
    "",              "",       v_rand1,  v_rand1, v_yrs,     m_yrs2,    "",     # first diff hier
    "",              "",       v_rand1,  v_rand1, v_yrs,     m_yrs2,    "",     # first diff spatial
    "",              "",       v_strat1, v_rand1, v_knots,   m_knots,   "",     # gam hier
    "",              "",       v_rand1,  v_rand1, v_knots,   m_knots,   "",     # gam spatial
    m_yrs1,          v_strat1, v_strat1, v_rand1, v_knots,   m_knots,   "",     # gamye hier
    m_yrs1,          v_strat1, v_rand1,  v_rand1, v_knots,   m_knots,   "",     # gamye spatial
    m_yrs1,          v_strat1, v_rand1,  "",      "",        v_strat2,  v_rand2,# slope hier
    m_yrs1,          v_strat1, v_rand1,  "",      "",        v_strat2,  v_rand2 # slope spatial
  ) %>%
    dplyr::bind_cols(bbs_models) %>%
    dplyr::filter(paste0(.data$model, "_", .data$variant) == .env$model) %>%
    dplyr::select(-"model", -"variant", -"file") %>%
    unlist()

  # Drop empty values
  init_specific <- init_specific[init_specific != ""]

  # Run all the functions
  for(i in seq_along(init_specific)) init_specific[[i]] <- rlang::exec(init_specific[[i]])

  # Join with generic values
  init <- append(init_generic, init_specific)

  # Create one list for each chain
  init_def <- list()
  for(i in seq_len(n_chains)) init_def[[i]] <- init

  init_def
}
