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

#' Run Bayesian model
#'
#'
#'
#' @param prepped_data List. Output of `prepare_data()`.
#' @param model Character.
#' @param model_variant
#' @param spatial_data List. Output of `prepare_spatial()`.
#' @param heavy_tailed
#' @param n_knots
#' @param basis
#' @param use_pois
#' @param calculate_nu
#' @param calculate_log_lik
#' @param calculate_CV
#' @param refresh
#' @param chains
#' @param parallel_chains
#' @param iter_sampling
#' @param iter_warmup
#' @param adapt_delta
#' @param max_treedepth
#' @param init_def
#' @param out_name
#' @param out_dir
#' @param quiet
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' # You can also specify the GAM model, with an optional number of
#' # knots to use for the GAM basis.
#' # By default, the number of knots will be equal to the floor
#' # of the total unique years for the species / 4
#' model_data <- prepare_data(strat_data = strat_data,
#'                            species_to_run = "Pacific Wren",
#'                            model = "gam",
#'                            n_knots = 9)
#'
run_model <- function(prepped_data,
                      model,
                      model_variant = "hier",
                      spatial_data = NULL,
                      heavy_tailed = TRUE,
                      n_knots = NULL,
                      basis = "mgcv",
                      use_pois = FALSE,
                      calculate_nu = FALSE,
                      calculate_log_lik = FALSE,
                      calculate_CV = FALSE,
                      refresh = 100,
                      chains = 4,
                      parallel_chains = 4,
                      iter_sampling = 1000,
                      iter_warmup = 1000,
                      adapt_delta = 0.95,
                      max_treedepth = 14,
                      init_def = NULL,
                      out_name = NULL,
                      out_dir = ".",
                      quiet = FALSE,
                      ...) {

  # Check inputs
  model_variant <- check_model_variant(model_variant)
  model <- check_model(model, model_variant)
  basis <- check_basis(basis)

  model_data <- prepped_data$model_data

  if(model_variant == "spatial") {

    if(is.null(spatial_data)) {
      stop("When `model_variant = 'spatial'`, you must provide a list ",
           "of neighbour nodes\n(created with `prepare_spatial()`) to ",
           "`spatial_data`. ",
           "See ?run_model for details", call. = FALSE)
    }
    check_neighbours(spatial_data,
                     unique(prepped_data$raw_data$strata_name))

  } else if(!is.null(spatial_data)) {
    if(!quiet) message("Model isn't spatial, ignoring `spatial_data` argument")
  }

  check_logical(heavy_tailed, use_pois, calculate_nu, calculate_log_lik,
                calculate_CV)
  check_numeric(chains, parallel_chains, iter_sampling, iter_warmup)

  if(!heavy_tailed & !use_pois) {
    stop("Heavy-tailed models are implied with Negative Binomial ",
         "(`use_pois == FALSE`). Set `heavy_tailed = TRUE`",  call. = FALSE)
  }

  # Check files and directory
  if(!dir.exists(out_dir)) {
    stop("'", out_dir, "' does not exist. Please create it first.",
         call. = FALSE)
  }

  if(is.null(out_name)) {
    out_name <- paste0("BBS_STAN_", model, "_", model_variant, "_", Sys.Date())
  } else if(!is.na(ext(out_name))) {
    stop("`out_name` should not have a file extension", call. = FALSE)
  }


  # Add model settings as parameters
  params <- model_params(
    model, n_strata = model_data$n_strata,
    year = model_data$year, n_counts = model_data$n_counts,
    basis, n_knots, heavy_tailed, use_pois,
    calculate_nu, calculate_log_lik, calculate_CV)

  # Create master parameter list
  model_data <- append(model_data, params)
  model_data <- append(model_data, spatial_data[c("n_edges", "node1", "node2")])


  # Keep track of data
  meta_data <- prepped_data[["meta_data"]]
  meta_data[["model"]] <- model
  meta_data[["model_variant"]] <- model_variant
  meta_data[["run_date"]] <- Sys.time()

  # Get initial values
  if(is.null(init_def)) {
    init_def <- create_init_def(model, model_variant, model_data, chains)
  }

  # Load model
  model <- system.file("models",
                       paste0(model, "_", model_variant, "_bbs_CV.stan"),
                       package = "bbsBayes")

  if(length(model) == 0) {
    stop("Stan model not found. Please submit an issue at \n",
         "https://github.com/BrandonEdwards/bbsBayes/issues", call. = FALSE)
  }

  # Compile model
  model <- cmdstanr::cmdstan_model(model)

  # What here should be changeable? can use ... and reference cmdstanr docs...
  model_fit <- model$sample(
    data = model_data,
    refresh = refresh,
    chains = chains,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    parallel_chains = parallel_chains,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    init = init_def,
    output_dir = out_dir,
    output_basename = out_name)

  list("model_fit" = model_fit,
       "non_zero_weight" = model_data$non_zero_weight,
       "meta_data" = meta_data,
       "raw_data" = prepped_data[["raw_data"]])
}

model_params <- function(model, n_strata, year, n_counts,
                         basis, n_knots, heavy_tailed, use_pois,
                         calculate_nu, calculate_log_lik, calculate_CV) {


  params <- list()

  # Model options

  # Extra Poisson variance options
  # 0 = use df == 3 (do not calculate df for the t-distributed noise)
  # 1 = use nu ~ gamma(2,0.1))
  params[["calc_nu"]] <- as.integer(calculate_nu)
  # 1 = heavy-tailed t-dist. noise; 0 = normal dist)
  params[["heavy_tailed"]] <- as.integer(heavy_tailed)
  # 1 = over-dispersed poisson; 0 = Negative Binomial
  params[["use_pois"]] <- as.integer(use_pois)

  # Calc point-wise log-likelihood of data given model
  params[["calc_log_lik"]] <- as.integer(calculate_log_lik)

  # Cross validation options
  params[["calc_CV"]] <- as.integer(calculate_CV) # 1 = do cross-validation
  params[["train"]] <- as.integer(1:n_counts) # indices of obs in the training dataset
  params[["test"]] <- 1L          # indices of obs in the test dataset
  params[["n_train"]] <- n_counts # no. training data (must == n_counts if calc_CV == 0)
  params[["n_test"]] <- 1L        # no. testing data  (ignored if calc_CV == 0)


  # Calculate additional model parameters
  ymin <- min(year)
  ymax <- max(year)
  n_years <- length(ymin:ymax)
  years <- ymin:ymax


  if(model %in% c("slope", "first_diff")) {
    fixed_year <- floor(stats::median(unique(years)))
    params[["fixed_year"]] <- fixed_year
  }

  if(model %in% "first_diff"){
    zero_betas <- rep(0, n_strata)
    Iy1 <- (fixed_year - 1):1
    n_Iy1 <- length(Iy1)
    Iy2 <- (fixed_year + 1):n_years
    n_Iy2 <- length(Iy2)

    params[["zero_betas"]] <- zero_betas
    params[["Iy1"]] <- Iy1
    params[["n_Iy1"]] <- n_Iy1
    params[["Iy2"]] <- Iy2
    params[["n_Iy2"]] <- n_Iy2
  }


  if(model %in% c("gam", "gamye")) {
    if(is.null(n_knots)) n_knots <- floor(length(unique((years)))/4)
    if(basis == "mgcv") {
      smooth_basis <- mgcv::smoothCon(
        mgcv::s(x, k = n_knots + 1, bs = "tp"), data = data.frame(x = years),
        # drops constant and absorbs identifiability constraints into the basis
        absorb.cons = TRUE,
        # If TRUE, the smooth is reparameterized to turn the penalty into an
        # identity matrix, with the final diagonal elements zeroed
        # (corresponding to the penalty nullspace).
        diagonal.penalty = TRUE)

      year_basis <- smooth_basis[[1]]$X

    } else {
      recenter <- floor(diff(c(1, ymax))/2)

      # generates a year variable with range = 1, this rescaling helps the
      # convergence for the GAM beta parameters
      rescale <- ymax

      year_scale <- (years - recenter) / ymax

      scaled_year <- seq(min(year_scale),
                         max(year_scale),
                         length = n_years) %>%
        setNames(ymin:ymax)

      if(ymin != 1) {
        new_yr <- 1:(ymin - 1)
        new_yr_scale <- (new_yr - recenter)/rescale
        names(new_yr_scale) <- new_yr
        scaled_year = c(new_yr_scale, scaled_year)
      }

      ymin_scale <- scaled_year[as.character(ymin)]
      ymax_scale <- scaled_year[as.character(ymax)]

      if(ymin != 1) {
        ymin_pred <- 1
        ymin_scale_pred <- scaled_year[as.character(1)]
      }

      knotsX <- seq(ymin_scale, ymax_scale, length = (n_knots + 2))[-c(1, n_knots + 2)]
      X_K <- (abs(outer(seq(ymin_scale, ymax_scale, length = n_years), knotsX, "-")))^3
      X_OMEGA_all <- (abs(outer(knotsX, knotsX, "-")))^3
      X_svd_OMEGA_all <- svd(X_OMEGA_all)
      X_sqrt_OMEGA_all <- t(X_svd_OMEGA_all$v  %*%
                              (t(X_svd_OMEGA_all$u) * sqrt(X_svd_OMEGA_all$d)))
      year_basis <- t(solve(X_sqrt_OMEGA_all, t(X_K)))
    }

    params[["n_knots_year"]] <- n_knots
    params[["year_basis"]] <- year_basis
  }

  params
}



create_init_def <- function(model, model_variant, model_data, chains) {

  # Generic --------------
  init_generic <-
    list(
      noise_raw  = rnorm(model_data$n_counts * model_data$use_pois, 0, 0.1),
      strata_raw = rnorm(model_data$n_strata, 0, 0.1),
      STRATA     = 0,
      nu         = 10,
      sdstrata   = runif(1, 0.01, 0.1),
      eta        = 0,
      obs_raw    = rnorm(model_data$n_observers, 0, 0.1),
      ste_raw    = rnorm(model_data$n_sites, 0, 0.1),
      sdnoise    = runif(1,0.3,1.3),
      sdobs      = runif(1,0.01,0.1),
      sdste      = runif(1,0.01,0.2)
      )

  # By model -------------

  # Matrices
  m_yrs1 <-  function() matrix(
    rnorm(model_data$n_years * model_data$n_strata, 0, 0.1),
    nrow = model_data$n_strata,
    ncol = model_data$n_years)

  m_yrs2 <-  function() matrix(
    rnorm((model_data$n_years - 1) * model_data$n_strata, 0, 0.1),
    nrow = model_data$n_strata,
    ncol = model_data$n_years - 1)

  m_knots <- function() matrix(
    rnorm(model_data$n_knots_year * model_data$n_strata, 0, 0.01),
    nrow = model_data$n_strata,
    ncol = model_data$n_knots_year)

  # Vectors
  v_rand1 <-  function() runif( 1, 0.01, 0.1)
  v_rand2 <-  function() rnorm( 1,    0, 0.1)
  v_strat1 <- function() runif( model_data$n_strata,   0.01, 0.1)
  v_strat2 <- function() rnorm( model_data$n_strata,      0, 0.1)
  v_yrs <-    function() rnorm((model_data$n_years - 1), 0, 0.1)
  v_knots <-  function() rnorm( model_data$n_knots_year,  0, 0.1)

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
  )


  # Join by model and get relevant variant
  init_specific <- init_specific %>%
    dplyr::bind_cols(bbs_models) %>%
    dplyr::filter(.data$model == .env$model, .data$variant == .env$model_variant) %>%
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
  for(i in seq_len(chains)) init_def[[i]] <- init

  init_def
}

