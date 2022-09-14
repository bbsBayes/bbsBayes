test_that("JAGS models run", {

  skip("Do not test automatically")

  bbs_data <- stratify(by = "bbs_usgs", sample_data = TRUE)

  jags_data <- prepare_data(strat_data = bbs_data,
                            species_to_run = "Pacific Wren",
                            model = "slope",
                            min_max_route_years = 2,
                            heavy_tailed = TRUE)

  jags_fit <- run_model(jags_data = jags_data,
                        parameters_to_save = c("n","nslope",
                                               "BETA","beta","STRATA",
                                               "sdobs","sdbeta","eta"),
                        parallel = TRUE,
                        modules = "glm")

  save(list = c("jags_fit","jags_data","species"),
       file = file.path(system.file("model_outputs", package = "bbsBayes"),
                        "jags_bbs_usgs_slope_pacific_wren.RData"))
})

test_that("JAGS outputs", {

  skip("Do not test automatically")

  load(system.file("model_outputs", "jags_bbs_usgs_slope_pacific_wren.RData",
                   package = "bbsBayes"))

  # 'jags_fit","jags_data","species"

  #S <- ggmcmc::ggs(jagsfit$samples, family = "B.X") # "Column `Parameter` doesn't exists"

  # INDICES --------------------------
  indices <- generate_indices(jags_mod = jags_fit,
                              jags_data = jags_data,
                              regions = c("continental",
                                          "national",
                                          "prov_state",
                                          "stratum"))
  saveRDS(indices, file.path(system.file("model_outputs", package = "bbsBayes"),
                              "jags_bbs_usgs_slope_pacific_wren_indices.rds"))

  indices2 <- generate_indices_tidy(jags_mod = jags_fit,
                                    jags_data = jags_data,
                                    regions = c("continental",
                                                "national",
                                                "prov_state",
                                                "stratum"))

  saveRDS(indices2, file.path(system.file("model_outputs", package = "bbsBayes"),
                             "jags_bbs_usgs_slope_pacific_wren_indices2.rds"))

  indices <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_indices.rds",
    package = "bbsBayes"))

  waldo::compare(dplyr::arrange(indices$data_summary, Year) %>%
                   dplyr::filter(Region_type == "continental"),
                 dplyr::arrange(indices2$data_summary, Year) %>%
                   dplyr::filter(Region_type == "continental"),
                 tolerance = 0.0001)

  waldo::compare(dplyr::arrange(indices$data_summary, Year) %>%
                   dplyr::filter(Region_type == "national"),
                 dplyr::arrange(indices2$data_summary, Year) %>%
                   dplyr::filter(Region_type == "national"),
                 tolerance = 0.0001)

  # Problem with reversal of AK and BC...
  waldo::compare(dplyr::arrange(indices$data_summary[, 1:7], Year, Region) %>%
                   dplyr::filter(Region_type == "prov_state", !Region %in% c("AK", "BC")),
                 dplyr::arrange(indices2$data_summary[, 1:7], Year, Region) %>%
                   dplyr::filter(Region_type == "prov_state", !Region %in% c("AK", "BC")),
                 tolerance = 0.0001)

  waldo::compare(dplyr::arrange(indices$data_summary, Year, Region) %>%
                   dplyr::filter(Region_type == "stratum"),
                 dplyr::arrange(indices2$data_summary, Year, Region) %>%
                   dplyr::filter(Region_type == "stratum"),
                 tolerance = 0.0001)

  waldo::compare(dplyr::arrange(indices$data_summary, Region, Year)$backcast_flag,
                 dplyr::arrange(indices2$data_summary, Region, Year)$backcast_flag,
                 tolerance = 0.0001)



  # Only problems left with reversal of AK and BC in prov_stat
  waldo::compare(dplyr::filter(indices$data_summary, !Region %in% c("AK", "BC")) %>%
                   dplyr::arrange(Year, Region),
                 dplyr::filter(indices2$data_summary, !Region %in% c("AK", "BC")) %>%
                   dplyr::arrange(Year, Region),
                 tolerance = 0.0001)


  # All samples the same --- Hooray!
  waldo::compare(indices$samples[names(indices2$samples)],
                 indices2$samples, tolerance = 0.0001)

  # All area weights the same --- Hooray!
  waldo::compare(indices$area_weights %>% dplyr::arrange(region),
                 indices2$area_weights %>% dplyr::arrange(region), tolerance = 0.0001)

  # All raw data the same --- Hooray! (but new has extra, perhaps omit?)
  waldo::compare(indices$raw_data %>% dplyr::select(year, count, strat),
                 indices2$raw_data %>% dplyr::select(year, count, strat), tolerance = 0.0001)

  # All incidentals the same --- Hooray!
  waldo::compare(indices[c("y_min", "y_max", "startyear", "regions")],
                 indices[c("y_min", "y_max", "startyear", "regions")])


  # TRENDS ------------------------
  indices <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_indices.rds",
    package = "bbsBayes"))

  trends <- generate_trends(indices = indices)
  saveRDS(trends, file.path(system.file("model_outputs", package = "bbsBayes"),
                             "jags_bbs_usgs_slope_pacific_wren_trends.rds"))

  trends <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_trends.rds",
    package = "bbsBayes"))

  indices2 <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_indices2.rds",
    package = "bbsBayes"))
  trends2 <- generate_trends_tidy(indices = indices2)
  saveRDS(trends2, file.path(system.file("model_outputs", package = "bbsBayes"),
                             "jags_bbs_usgs_slope_pacific_wren_trends2.rds"))


  # All the same columns -- Hooray!
  waldo::compare(sort(names(trends)), sort(names(trends2)))

  # All the same values (except the above issues with AK and BC) -- Hooray!
  waldo::compare(dplyr::select(trends, names(trends2)) %>%
                   dplyr::arrange(Region_type, Region) %>%
                   dplyr::filter(!Region %in% c("AK", "BC")),
                 trends2 %>%
                   dplyr::filter(!Region %in% c("AK", "BC")),
                 tolerance = 0.0001)

  # plot_indices ------------------------
  indices <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_indices.rds",
    package = "bbsBayes"))

  tp <- plot_indices(indices = indices,
                     species = "Pacific Wren",
                     add_observed_means = TRUE)

  saveRDS(tp, file.path(system.file("model_outputs", package = "bbsBayes"),
                        "jags_bbs_usgs_slope_pacific_wren_trend_plots.rds"))
  tp[[1]]
  tp[[2]]


  indices2 <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_indices2.rds",
    package = "bbsBayes"))

  tp2 <- plot_indices(indices = indices2,
                      species = "Pacific Wren",
                      add_observed_means = TRUE)

  saveRDS(tp2, file.path(system.file("model_outputs", package = "bbsBayes"),
                         "jags_bbs_usgs_slope_pacific_wren_trend_plots2.rds"))
  tp2[[1]]
  tp2[[2]]

  # All good -- Hooray!


  # generate_map ------------------------
  trends <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_trends.rds",
    package = "bbsBayes"))

  generate_map(trends,
               select = TRUE,
               stratify_by = "bbs_cws",
               species = "Pacific Wren")

  trends2 <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_trends2.rds",
    package = "bbsBayes"))

  generate_map(trends,
               select = TRUE,
               stratify_by = "bbs_cws",
               species = "Pacific Wren")

  # All good -- Hooray!

  # geofacet_plot ------------------------

  indices <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_indices.rds",
    package = "bbsBayes"))

  geofacet_plot(indices_list = indices,
                select = TRUE,
                stratify_by = "bbs_cws",
                multiple = TRUE,
                trends = trends,
                slope = FALSE,
                species = "Pacfic Wren")

  indices2 <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_indices2.rds",
    package = "bbsBayes"))

  geofacet_plot(indices_list = indices2,
                select = TRUE,
                stratify_by = "bbs_cws",
                multiple = TRUE,
                trends = trends,
                slope = FALSE,
                species = "Pacfic Wren")
})

test_that("STAN models run", {

  skip("Do not test automatically")

  bbs_data <- stratify(by = "bbs_usgs", sample_data = TRUE)

  stan_data <- prepare_data_stan(bbs_data,
                                 species_to_run = "Pacific Wren",
                                 model = "slope",
                                 min_max_route_years = 2)

  tmp_stratify_by <- stan_data[["stratify_by"]]
  tmp_model <- stan_data[["model"]]
  tmp_alt_data <- stan_data[["alt_data"]]

  stan_data[["stratify_by"]] <- NULL
  stan_data[["model"]] <- NULL
  stan_data[["alt_data"]] <- NULL

  mod.file = system.file("models", "slope_bbs_CV.stan", package = "bbsBayes")
  out_base <- paste("pacific_wren", tmp_model, "BBS", sep = "_")

  # compiles Stan model (this is only necessary if the model has been changed since it was last run on this machine)
  stan_model <- cmdstanr::cmdstan_model(mod.file)

  init_def <- function() {
    list(noise_raw = rnorm(stan_data$ncounts*stan_data$use_pois,0,0.1),
         strata_raw = rnorm(stan_data$nstrata,0,0.1),
         STRATA = 0,
         nu = 10,
         sdstrata = runif(1,0.01,0.1),
         eta = 0,
         yeareffect_raw = matrix(rnorm(stan_data$nstrata*stan_data$nyears,0,0.1),nrow = stan_data$nstrata,ncol = stan_data$nyears),
         obs_raw = rnorm(stan_data$nobservers,0,0.1),
         ste_raw = rnorm(stan_data$nsites,0,0.1),
         sdnoise = runif(1,0.3,1.3),
         sdobs = runif(1,0.01,0.1),
         sdste = runif(1,0.01,0.2),
         sdbeta = runif(1,0.01,0.1),
         sdyear = runif(stan_data$nstrata,0.01,0.1),
         BETA = rnorm(1,0,0.1),
         beta_raw = rnorm(stan_data$nstrata,0,0.1))}

  stan_fit <- stan_model$sample(
    data=stan_data,
    refresh=1,
    chains=3,
    iter_sampling=1000,
    iter_warmup=1000,
    parallel_chains = 3,
    #pars = parms,
    adapt_delta = 0.95,
    max_treedepth = 14,
    seed = 123,
    init = init_def,
    output_dir = system.file("model_outputs", package = "bbsBayes"),
    output_basename = out_base)

  message("Calculating run summary")
  fit_summary <- stan_fit$summary()

  meta_data <- list()
  meta_data[["stratify_by"]] <- tmp_stratify_by
  meta_data[["model"]] <- tmp_model
  meta_data[["alt_data"]] <- tmp_alt_data
  meta_data[["strat_name"]] <- tmp_alt_data$strat_name

  save(list = c("stan_fit","stan_data", "meta_data",
                "out_base", "fit_summary"),
       file = file.path(system.file("model_outputs", package = "bbsBayes"),
                        paste0(out_base, "_Stan_fit.RData")))
})

test_that("EXPLORE STAN outputs", {

  skip("Do not test automatically")

  load(system.file("model_outputs", "pacific_wren_slope_BBS_Stan_fit.RData",
                   package = "bbsBayes"))

  # "stan_fit","stan_data","out_base",  "fit_summary"

  # samples
  temp <- stan_fit$draws(variables = "strata_raw", format = "draws_df")

  # 3000 rows (1000 x 3 chains) for 51 year effects per strata.
  temp <- stan_fit$draws(variables = "yeareffect", format = "draws_df")

  temp <- stan_fit$draws(format = "draws_matrix")
  posterior::variables(temp) %>%
    stringr::str_remove("\\[(.)*$") %>%
    unique()

  n1 <- stan_fit$draws(variables = "nslope", format = "draws_matrix")


  stan_model <- cmdstanr::cmdstan_model(mod.file)
  stan_data[["stratify_by"]] <- NULL
  stan_data[["model"]] <- NULL
  stan_data[["alt_data"]] <- NULL
  stan_data[["strat_name"]] <- NULL
  stan_quant <- stan_model$generate_quantities(stan_fit, data = stan_data)


  temp <- stan_quant$draws()
  posterior::variables(temp) %>%
    stringr::str_remove("\\[(.)*$") %>%
    unique()

  n2 <- stan_quant$draws(variables = "nslope", format = "draws_matrix")

  stan_quant$draws(variables = "adj", format = "draws_matrix")

  waldo::compare(n1, n2, tolerance = 0.00001)

})

test_that("STAN outputs", {
  skip("Do not test automatically")

  load(system.file("model_outputs", "pacific_wren_slope_BBS_Stan_fit.RData",
                   package = "bbsBayes"))

  # INDICES --------------------------
  indices <- generate_indices_stan(model_fit = stan_fit,
                                   model_data = stan_data,
                                   regions = c("continental",
                                               "national",
                                               "prov_state",
                                               "stratum"))

})
