#' Run Bayesian model
#'
#'
#'
#' @param model_file Character. Location of a custom Stan model file to use.
#' @param spatial_data List. Output of `prepare_spatial()`.
#' @param heavy_tailed Logical. Whether extra-Poisson error distributions should
#'   be modelled as a t-distribution, with heavier tails than the standard
#'   normal distribution. Default is `TRUE`. Recent results suggest this is best
#'   even though it requires much longer convergence times. Can only be used
#'   with Negative Binomial models (i.e. `use_pois = FALSE`).
#' @param n_knots Numeric. Number of knots for "gam" and "gamye" models
#' @param basis Character. Basis function to use for GAM smooth, one of
#'   "original" or "mgcv". Default is "original", the same basis used in Smith
#'   and Edwards 2020. "mgcv" is an alternate that uses the "tp" basis from the
#'   package mgcv (also used in brms, and rstanarm). If using the "mgcv" option,
#'   the user may want to consider adjusting the prior distributions for the
#'   parameters and their precision.
#' @param use_pois Logical. Whether to use an Over-Dispersed Poisson model
#'   (`TRUE`) or an Negative Binomial model (`FALSE`, default).
#' @param calculate_nu Logical. Whether to calculate the `nu` parameter as
#'  a factor of `gamma(2, 0.1)`. Default is `FALSE`.
#' @param calculate_log_lik Logical. Whether to calculate point-wise
#'   log-likelihood of the data given the model. Default is `FALSE`.
#' @param calculate_CV Logical. Whether to use cross validation.
#' @param refresh Numeric. Passed to `cmdstanr::sample()`. Number of iterations
#'   between screen updates. If 0, only errors are shown.
#' @param chains Numeric. Passed to `cmdstanr::sample()`. Number of Markov
#'   chains to run.
#' @param parallel_chains Numeric. Passed to `cmdstanr::sample()`. Maximum
#'   number of chains to run in parallel.
#' @param iter_warmup Numeric. Passed to `cmdstanr::sample()`. Number of warmup
#'   iterations per chain.
#' @param iter_sampling Numeric. Passed to `cmdstanr::sample()`. Number of
#'   sampling (post-warmup) iterations per chain.
#' @param adapt_delta Numeric. Passed to `cmdstanr::sample()`. The adaptation
#'   target acceptance statistic.
#' @param max_treedepth Numeric. Passed to `cmdstanr::sample()`. The maximum
#'   allowed tree depth for the NUTS engine. See `?cmdstanr::sample`.
#' @param init (multiple options). An initialization method passed to the `init`
#'   argument of `cmdstanr::sample()`. See ?cmdstanr::sample for more details.
#'   The default (`NULL`) is for bbsBayes to supply initial parameter values
#'   according to model and model variant selected.
#' @param init_only Logical. Whether to return the list of initial parameter
#'   values only (and not run the model). This is useful if you wish to see
#'   which `init` parameters are being supplied.
#' @param output_basename Character. Name of the files created as part of the
#'   Stan model run and the final model output RDS file if `save_output = TRUE`.
#' @param output_dir Character. Directory in which all model files will be
#'   created.
#' @param save_output Logical. Whether or not to save the model output to file
#'   as an RDS object with all required data. Defaults to `TRUE`.
#' @param ... Other arguments passed on to `cmdstanr::sample()`
#' @param jags_data Defunct.
#' @param inits Defunct.
#' @param parameters_to_save Defunct.
#' @param track_n Defunct.
#' @param n_adapt Defunct.
#' @param n_burnin Defunct.
#' @param n_thin Defunct.
#' @param n_chains Defunct.
#' @param n_saved_steps Defunct.
#' @param n_iter Defunct.
#' @param modules Defunct.
#' @param parallel Defunct.
#' @param n_cores Defunct.
#'
#' @inheritParams common_docs
#'
#' @details There are two ways you can customize
#' the model run. The first is to supply a custom `model_file` created with the
#' `copy_model_file()` function and then edited by hand.
#'
#' Second, you can supply a custom list of initialization parameters. You can
#' supply these parameters in anyway that `cmdstanr::sample()` accepts the
#' `init` argument. One way is to use the bbsBayes created parameters and modify
#' them as needed. To do this, setup the `run_model()` function as you would
#' normally use it, but add `init_only = TRUE`. This will return the list of
#' initial definitions. You can edit this list and then run your model with
#' `run_model(... init = new_inits)`.
#'
#' See the [models
#' article](https://steffilazerte.ca/bbsBayes/articles/models.html) for more
#' advanced examples and explanations.
#'
#' @return A list containing the model output (`model_fit`), meta data for the
#'   analysis (`meta_data`), meta data for the strata (`meta_strata`) and
#'   prepared data counts from `prepare_data()` (`raw_data`).
#' @export
#'
#' @examples
#'
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#' p <- prepare_data(s)
#'
#' # Run model (quick and dirty)
#' m <- run_model(p, model = "first_diff", model_variant = "hier",
#'                iter_warmup = 20, iter_sampling = 20, chains = 2)
#'
#' # Clean up (remove model files)
#' unlink(list.files(pattern = paste0("BBS_STAN_first_diff_hier_", Sys.Date())))

run_model <- function(prepped_data,
                      model,
                      model_variant = "hier",
                      model_file = NULL,
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
                      iter_warmup = 1000,
                      iter_sampling = 1000,
                      adapt_delta = 0.95,
                      max_treedepth = 14,
                      init = NULL,
                      init_only = FALSE,
                      output_basename = NULL,
                      output_dir = ".",
                      save_output = TRUE,
                      quiet = FALSE,
                      jags_data, inits, parameters_to_save, track_n, n_adapt,
                      n_burnin, n_thin, n_chains, n_saved_steps, n_iter,
                      modules, parallel, n_cores,
                      ...) {

  # Deprecated/Defunct args
  msg <- "alternate arguments for `cmdstanr` (see ?run_model)"
  if(!missing(jags_data)) dep_stop("3.0.0", "jags_data", msg)
  if(!missing(inits)) dep_stop("3.0.0", "inits", msg)
  if(!missing(parameters_to_save)) dep_stop("3.0.0", "parameters_to_save", msg)
  if(!missing(track_n)) dep_stop("3.0.0", "track_n", msg)
  if(!missing(n_chains)) dep_stop("3.0.0", "n_chains", msg)
  if(!missing(n_adapt)) dep_stop("3.0.0", "n_adapt", msg)
  if(!missing(n_saved_steps)) dep_stop("3.0.0", "n_saved_steps", msg)
  if(!missing(n_burnin)) dep_stop("3.0.0", "n_burnin", msg)
  if(!missing(n_thin)) dep_stop("3.0.0", "n_thin", msg)
  if(!missing(n_iter)) dep_stop("3.0.0", "n_iter", msg)
  if(!missing(modules)) dep_stop("3.0.0", "modules", msg)
  if(!missing(parallel)) dep_stop("3.0.0", "parallel", msg)
  if(!missing(n_cores)) dep_stop("3.0.0", "n_cores", msg)

  # Check inputs
  check_data(prepped_data)
  model <- check_model(model, model_variant)
  model_file <- check_model_file(model, model_variant, model_file)
  basis <- check_basis(basis)

  check_logical(heavy_tailed, use_pois, calculate_nu, calculate_log_lik,
                calculate_CV, save_output, quiet)
  check_numeric(refresh, chains, parallel_chains, iter_sampling, iter_warmup,
                adapt_delta, max_treedepth)

  model_data <- prepped_data$model_data

  if(model_variant == "spatial") {
    check_spatial(spatial_data, unique(prepped_data$raw_data$strata_name))
  } else if(!is.null(spatial_data)) {
    if(!quiet) message("Model isn't spatial, ignoring `spatial_data` argument")
  }

  check_logical(heavy_tailed, use_pois, calculate_nu, calculate_log_lik,
                calculate_CV)
  check_numeric(chains, parallel_chains, iter_sampling, iter_warmup)

  if(!heavy_tailed & !use_pois) {
    stop("Negative Binomial models (`use_pois == FALSE`) have to be ",
         "heavy-tailed (`heavy_tailed = TRUE`). ",
         "Set `heavy_tailed = TRUE` or `use_pois = TRUE`",
         call. = FALSE)
  }

  # Files and directory
  check_dir(output_dir)
  output_basename <- check_file(output_basename, model, model_variant)

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
  meta_data <- append(
    prepped_data[["meta_data"]],
    list("model" = model,
         "model_variant" = model_variant,
         "model_file" = model_file,
         "run_date" = Sys.time(),
         "bbsBayes_version" = as.character(packageVersion("bbsBayes")),
         "cmdstan_path" = cmdstanr::cmdstan_path(),
         "cmdstan_version" = cmdstanr::cmdstan_version()))

  # Get initial values
  if(is.null(init)) {
    init <- create_init(model, model_variant, model_data, chains)
  } else {
    init <- check_init(init, chains)
  }

  if(init_only) return(init[[1]])


  # Compile model
  model <- cmdstanr::cmdstan_model(model_file, dir = bbs_dir())

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
    init = init,
    output_dir = output_dir,
    output_basename = output_basename,
    ...)

  model_output <- list("model_fit" = model_fit,
                       "meta_data" = meta_data,
                       "meta_strata" = prepped_data[["meta_strata"]],
                       "raw_data" = prepped_data[["raw_data"]])

  if(save_output) save_model_run(model_output)

  model_output
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
  params[["train"]] <- as.integer(1:n_counts) # indices of obs in training data
  params[["test"]] <- 1L          # indices of obs in the test dataset
  params[["n_train"]] <- n_counts # n training data (n_counts if calc_CV == 0)
  params[["n_test"]] <- 1L        # n testing data  (ignored if calc_CV == 0)


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
      # Use 'years' to avoid note (cheating as still referencing data.frame)
      smooth_basis <- mgcv::smoothCon(
        mgcv::s(years, k = n_knots + 1, bs = "tp"), data = data.frame(years),
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
        stats::setNames(ymin:ymax)

      if(ymin != 1) {
        new_yr <- 1:(ymin - 1)
        new_yr_scale <- (new_yr - recenter)/rescale
        names(new_yr_scale) <- new_yr
        scaled_year <- c(new_yr_scale, scaled_year)
      }

      ymin_scale <- scaled_year[as.character(ymin)]
      ymax_scale <- scaled_year[as.character(ymax)]

      if(ymin != 1) {
        ymin_pred <- 1
        ymin_scale_pred <- scaled_year[as.character(1)]
      }

      knotsX <- seq(ymin_scale, ymax_scale,
                    length = (n_knots + 2))[-c(1, n_knots + 2)]
      X_K <- (abs(outer(seq(ymin_scale, ymax_scale, length = n_years),
                        knotsX, "-")))^3
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



#' Create the initial definition list
#'
#' Creates list of initial parameter definitions to supply to
#' `cmdstanr::sample()`.
#'
#' @noRd
create_init <- function(model, model_variant, model_data, chains) {

  # Generic --------------
  init_generic <-
    list(
      noise_raw  = stats::rnorm(model_data$n_counts * model_data$use_pois,
                                0, 0.1),
      strata_raw = stats::rnorm(model_data$n_strata, 0, 0.1),
      STRATA     = 0,
      nu         = 10,
      sdstrata   = stats::runif(1, 0.01, 0.1),
      eta        = 0,
      obs_raw    = stats::rnorm(model_data$n_observers, 0, 0.1),
      ste_raw    = stats::rnorm(model_data$n_sites, 0, 0.1),
      sdnoise    = stats::runif(1,0.3,1.3),
      sdobs      = stats::runif(1,0.01,0.1),
      sdste      = stats::runif(1,0.01,0.2)
      )

  # By model -------------

  # Matrices
  m_yrs1 <-  function() matrix(
    stats:: rnorm(model_data$n_years * model_data$n_strata, 0, 0.1),
    nrow = model_data$n_strata,
    ncol = model_data$n_years)

  m_yrs2 <-  function() matrix(
    stats::rnorm((model_data$n_years - 1) * model_data$n_strata, 0, 0.1),
    nrow = model_data$n_strata,
    ncol = model_data$n_years - 1)

  m_knots <- function() matrix(
    stats::rnorm(model_data$n_knots_year * model_data$n_strata, 0, 0.01),
    nrow = model_data$n_strata,
    ncol = model_data$n_knots_year)

  # Vectors
  v_rand1 <-  function() stats::runif( 1, 0.01, 0.1)
  v_rand2 <-  function() stats::rnorm( 1,    0, 0.1)
  v_strat1 <- function() stats::runif( model_data$n_strata,   0.01, 0.1)
  v_strat2 <- function() stats::rnorm( model_data$n_strata,      0, 0.1)
  v_yrs <-    function() stats::rnorm((model_data$n_years - 1), 0, 0.1)
  v_knots <-  function() stats::rnorm( model_data$n_knots_year,  0, 0.1)

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
    dplyr::bind_cols(bbsBayes::bbs_models) %>%
    dplyr::filter(.data$model == .env$model,
                  .data$variant == .env$model_variant) %>%
    dplyr::select(-"model", -"variant", -"file") %>%
    unlist()

  # Drop empty values
  init_specific <- init_specific[init_specific != ""]

  # Run all the functions
  for(i in seq_along(init_specific)) {
    init_specific[[i]] <- rlang::exec(init_specific[[i]])
  }

  # Join with generic values
  init <- append(init_generic, init_specific)

  # Create one list for each chain
  init_def <- list()
  for(i in seq_len(chains)) init_def[[i]] <- init

  init_def
}


#' Save output of run_model()
#'
#' This function closely imitate `cmdstanr::save_object()` but saves the
#' entire model output object from `run_model()` which contains more details
#' regarding stratification etc.
#'
#' Files are saved to `path`, or if not provided to the original location of
#' the Stan model run files (provided the original files exist).
#'
#' @param path Character. Optional file path to use for saved data. Defaults to
#' the file path used for the original run
#'
#' @inheritParams common_docs
#'
#' @return Nothing. Creates .rds file at `path`.
#' @export
#'
#' @examples
#'
#' # By default, the model is saved as an RDS file during `run_model()`
#'
#' # But you can also deliberately save the file (here with an example model)
#' save_model_run(pacific_wren_model, path = "my_model.rds")
#'
#' # Clean up
#' unlink("my_model.rds")

save_model_run <- function(model_output, path = NULL, quiet = FALSE) {

  check_data(model_output)

  model_fit <- model_output$model_fit

  if(is.null(path)) {
    path <- model_fit$output_files()
    if(any(!file.exists(path))) {
      stop("Cannot find original model file location, please specify `path`",
           call. = FALSE)
    }

    path <- path %>%
      normalizePath() %>%
      stringr::str_remove("-[0-9]{1,3}.csv$") %>%
      unique()

    n <- length(list.files(path = dirname(path),
                           pattern = paste0(basename(path), "[.]*.rds$")))

    path <- paste0(path, "_",
                   stringr::str_pad(n + 1, width = 2, side = "left", pad = 0),
                   ".rds")
    if(!quiet) message("Saving model output to ", path)
  } else {

    check_dir(dirname(path))
    if(ext(path) != "rds") {
      stop("File must have a .rds extension", call. = FALSE)
    }
  }

  # Ensure all lazy data loaded (see ?cmdstanr::save_object)
  model_fit$draws()
  try(model_fit$sampler_diagnostics(), silent = TRUE)
  try(model_fit$init(), silent = TRUE)
  try(model_fit$profiles(), silent = TRUE)

  # Update entire model output object and save
  model_output[["model_fit"]] <- model_fit
  readr::write_rds(model_output, path)

  invisible(model_output)
}


#' Copy model file
#'
#' Save a predefined Stan model file to a local text file for editing. These
#' files can then be used in `run_models()` by specifying the `model_file`
#' argument.
#'
#' @param dir Character. Directory where file should be saved.
#' @param overwrite Logical. Whether to overwrite an existing copy of the model
#'   file.
#'
#' @inheritParams common_docs
#'
#' @return File path to copy of the model file.
#'
#' @examples
#'
#' # Save the Slope model in temp directory
#' copy_model_file(model = "slope", model_variant = "spatial", dir = tempdir())
#'
#' # Overwrite an existing copy
#' copy_model_file(model = "slope", model_variant = "spatial", dir = tempdir(),
#'                 overwrite = TRUE)
#'
#' # Clean up
#' unlink(file.path(tempdir(), "slope_spatial_bbs_CV_COPY.stan"))
#'
#' @export

copy_model_file <- function(model, model_variant, dir, overwrite = FALSE) {

  check_model(model, model_variant)
  check_dir(dir)

  f <- check_model_file(model, model_variant)

  f_new <- stringr::str_replace(basename(f), ".stan", "_COPY.stan") %>%
    file.path(dir, .)

  if(file.exists(f_new) & !overwrite) {
    stop("An existing copy of this file (", f_new, ")\nalready exists. ",
         "Either use `overwrite = TRUE` or rename the existing file",
         call. = FALSE)
  }

  message("Copying model file ", basename(f), " to ", f_new)
  file.copy(f, f_new, overwrite = overwrite)
  f_new
}
