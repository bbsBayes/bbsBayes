#' Run Bayesian model
#'
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
#' @param k Numeric. The K-fold group to run for cross-validation. Only relevant
#'   if folds defined by `prepare_model(calculate_cv = TRUE)` or custom
#'   definition. See `?prepare_model` or the [models
#'   article](https://steffilazerte.ca/bbsBayes/articles/models.html) for more
#'   details.
#' @param output_basename Character. Name of the files created as part of the
#'   Stan model run and the final model output RDS file if `save_model = TRUE`.
#' @param output_dir Character. Directory in which all model files will be
#'   created.
#' @param overwrite Logical. Whether to overwrite an existing model output file
#'   when saving
#' @param save_model Logical. Whether or not to save the model output to file
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
#' @details The model is set up in `prepare_model()`. The `run_model()` function
#' does the final (and often long-running) step of actually running the model.
#' Here is where you can tweak how the model will be run (iterations etc.).
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
#' pm <- prepare_model(p, model = "first_diff", model_variant = "hier")
#'
#' # Run model (quick and dirty)
#' m <- run_model(pm, iter_warmup = 20, iter_sampling = 20, chains = 2)
#'
#' # Clean up (remove model files)
#' unlink(list.files(pattern = paste0("BBS_STAN_first_diff_hier_", Sys.Date())))

run_model <- function(model_data,
                      refresh = 100,
                      chains = 4,
                      parallel_chains = 4,
                      iter_warmup = 1000,
                      iter_sampling = 1000,
                      adapt_delta = 0.95,
                      max_treedepth = 14,
                      k = NULL,
                      output_basename = NULL,
                      output_dir = ".",
                      save_model = TRUE,
                      overwrite = FALSE,
                      set_seed = NULL,
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
  check_data(model_data)
  check_logical(save_model, overwrite, quiet)
  check_numeric(refresh, chains, parallel_chains, iter_sampling, iter_warmup,
                adapt_delta, max_treedepth)

  meta_data <- model_data[["meta_data"]]
  raw_data <- model_data[["raw_data"]]
  meta_strata <- model_data[["meta_strata"]]
  init_values <- model_data[["init_values"]]
  folds <- model_data[["folds"]]
  model_data <- model_data[["model_data"]]

  # Files and directory
  check_dir(output_dir)
  output_basename <- check_file(output_basename,
                                meta_data[["model"]],
                                meta_data[["model_variant"]])

  # Keep track of data
  meta_data <- append(
    meta_data,
    list("run_date" = Sys.time(),
         "bbsBayes_version" = as.character(utils::packageVersion("bbsBayes")),
         "cmdstan_path" = cmdstanr::cmdstan_path(),
         "cmdstan_version" = cmdstanr::cmdstan_version()))

  # Check init values
  init_values <- check_init(init_values, chains)

  # Setup cross validation
  if(!is.null(k)) {
    check_cv(folds, k)

    model_data[["test"]] <- which(folds == k)
    model_data[["train"]] <- which(folds != k)
    model_data[["n_test"]] <- length(model_data[["test"]])
    model_data[["n_train"]] <- length(model_data[["train"]])
    model_data[["calc_CV"]] <- 1

    meta_data <- append(meta_data, list("k" = k))
    output_basename <- paste0(output_basename, "_k", k)

  } else {
    model_data[["test"]] <- 1L
    model_data[["train"]] <- as.integer(1:model_data[["n_counts"]])
    model_data[["n_test"]] <- 1L
    model_data[["n_train"]] <- model_data[["n_counts"]]
    model_data[["calc_CV"]] <- 0
  }

  # Check if overwriting
  if(save_model & !overwrite & file.exists(paste0(output_basename, ".rds"))){
    stop("File ", output_basename, ".rds already exists. Either choose a new ",
         "basename, or specify `overwrite = TRUE`", call. = FALSE)
  }

  # Compile model
  model <- cmdstanr::cmdstan_model(meta_data[["model_file"]], dir = bbs_dir())


  # Run model
  if(!is.null(set_seed)) withr::local_seed(set_seed)
  model_fit <- model$sample(
    data = model_data,
    refresh = refresh,
    chains = chains,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup,
    parallel_chains = parallel_chains,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    init = init_values,
    output_dir = output_dir,
    output_basename = output_basename,
    ...)

  model_output <- list("model_fit" = model_fit,
                       "meta_data" = meta_data,
                       "meta_strata" = meta_strata,
                       "raw_data" = raw_data)

  if(save_model) save_model_run(model_output)

  model_output
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
      unique() %>%
      paste0(".rds")


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
