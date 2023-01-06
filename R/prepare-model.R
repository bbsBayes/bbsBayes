
#' Prepare model parameters
#'
#'
#' @param model_file Character. Location of a custom Stan model file to use.
#' @param heavy_tailed Logical. Whether extra-Poisson error distributions should
#'   be modelled as a t-distribution, with heavier tails than the standard
#'   normal distribution. Default is `TRUE`. Recent results suggest this is best
#'   even though it requires much longer convergence times. Can only be set to
#'   FALSE with Poisson models (i.e. `use_pois = TRUE`).
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
#'
#' @inheritParams common_docs
#'
#' @details There are two ways you can customize
#' the model run. The first is to supply a custom `model_file` created with the
#' `copy_model_file()` function and then edited by hand.
#'
#' Second, you can edit or overwrite the initialization parameters
#' (`init_values`) in the output of `prepare_model()` to customize the `init`
#' supplied to `cmdstanr::sample()`. You can supply these parameters in anyway
#' that `cmdstanr::sample()` accepts the `init` argument.
#'
#' See the [models
#' article](https://steffilazerte.ca/bbsBayes/articles/models.html) for more
#' advanced examples and explanations.
#'
#' @return A list containing the pared model data (`model_data`), initialization
#'   parameters (`init_values`), meta data for the analysis (`meta_data`), meta
#'   data for the strata (`meta_strata`) and prepared data counts from
#'   `prepare_data()` (`raw_data`).
#' @export
#'
#' @examples
#'
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#' p <- prepare_data(s)
#' pm <- prepare_model(p, model = "first_diff", model_variant = "hier")

prepare_model <- function(prepped_data,
                          model,
                          model_variant = "hier",
                          model_file = NULL,
                          spatial_data = NULL,
                          use_pois = FALSE,
                          heavy_tailed = TRUE,
                          n_knots = NULL,
                          basis = "mgcv",
                          calculate_nu = FALSE,
                          calculate_log_lik = FALSE,
                          calculate_CV = FALSE,
                          set_seed = NULL,
                          quiet = FALSE) {

  # Check inputs
  check_data(prepped_data)
  model <- check_model(model, model_variant)
  model_file <- check_model_file(model, model_variant, model_file)
  basis <- check_basis(basis)

  check_logical(heavy_tailed, use_pois, calculate_nu, calculate_log_lik,
                calculate_CV, quiet)

  model_data <- prepped_data[["model_data"]]

  if(model_variant == "spatial") {
    check_spatial(spatial_data, unique(prepped_data[["raw_data"]]$strata_name))
  } else if(!is.null(spatial_data)) {
    if(!quiet) message("Model isn't spatial, ignoring `spatial_data` argument")
  }

  if(!heavy_tailed & !use_pois) {
    message("Negative Binomial models (`use_pois == FALSE`) must be ",
            "heavy-tailed (`heavy_tailed = TRUE`). ",
            "Setting `heavy_tailed = TRUE`.")
    heavy_tailed <- TRUE
  }

  # Add model settings as parameters
  params <- model_params(
    model,
    n_strata = model_data[["n_strata"]],
    year = model_data[["year"]],
    n_counts = model_data[["n_counts"]],
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
         "model_file" = model_file))

  # Get initial values
  if(!is.null(set_seed)) withr::local_seed(set_seed)
  init_values <- create_init(model, model_variant, model_data)

  list("model_data" = model_data,
       "init_values" = init_values,
       "meta_data" = meta_data,
       "meta_strata" = prepped_data[["meta_strata"]],
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
create_init <- function(model, model_variant, model_data) {

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
  append(init_generic, init_specific)
}
