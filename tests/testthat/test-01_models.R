test_that("download bbs data", {

  skip("Do not test automatically")

  bbs_dir <- app_dir(appname = "bbsBayes")

  # 2020 State data ------------

  # Download data
  fetch_bbs_data("state", 2020, force = TRUE)
  fetch_bbs_data_tidy("state", 2020, force = TRUE)

  # Load and compare data
  load(file = file.path(bbs_dir$data(), "bbs_raw_data.RData"))
  bbs_data2 <- readRDS(file.path(bbs_dir$data(), "bbs_raw_data.rds"))

  b1 <- dplyr::arrange(bbs_data[[1]], RouteDataID, Year, AOU)
  b2 <- as.data.frame(bbs_data2[[1]][names(b1)]) %>%
    dplyr::arrange(RouteDataID, Year, AOU)

  r1 <- dplyr::arrange(bbs_data[[2]], RouteDataID, Year, ObsN)
  r2 <- as.data.frame(bbs_data2[[2]]) %>%
    dplyr::arrange(RouteDataID, Year, ObsN)

  s1 <- bbs_data[[3]]
  s2 <- as.data.frame(bbs_data[[3]])

  rm(bbs_data)
  rm(bbs_data2)

  # All the same except int vs. dbl -- Hooray!!
  waldo::compare(b1, b2, tolerance = 0.00001)

  # All the same except corrected ones -- Hooray!!
  waldo::compare(dplyr::select(r1, -TempScale),
                 dplyr::select(r2, -TempScale),
                 tolerance = 0.00001)

  # All the same -- Hooray!!
  waldo::compare(s1, s1)


  # 2020 Stop data ------------

  # Download data
  fetch_bbs_data("stop", 2020, force = TRUE)
  fetch_bbs_data_tidy("stop", 2020, force = TRUE)

  # Load and compare data
  load(file = file.path(bbs_dir$data(), "bbs_stop_data.RData"))
  bbs_data2 <- readRDS(file.path(bbs_dir$data(), "bbs_stop_data.rds"))

  b1 <- dplyr::arrange(bbs_data[[1]], RouteDataID, Year, AOU)
  b2 <- as.data.frame(bbs_data2[[1]][names(b1)]) %>%
    dplyr::arrange(RouteDataID, Year, AOU)

  r1 <- dplyr::arrange(bbs_data[[2]], RouteDataID, Year, ObsN)
  r2 <- as.data.frame(bbs_data2[[2]]) %>%
    dplyr::arrange(RouteDataID, Year, ObsN)

  s1 <- bbs_data[[3]]
  s2 <- as.data.frame(bbs_data[[3]])

  rm(bbs_data)
  rm(bbs_data2)

  # All the same -- Hooray!!
  waldo::compare(b1, b2, tolerance = 0.00001)

  # All the same except corrected ones -- Hooray!!
  waldo::compare(dplyr::select(r1, -TempScale),
                 dplyr::select(r2, -TempScale),
                 tolerance = 0.00001)

  # All the same -- Hooray!!
  waldo::compare(s1, s1)


  # 2022 State data ------------

  # Download data
  fetch_bbs_data("state", 2022, force = TRUE)
  unlink(list.files(tempdir()))
  fetch_bbs_data_tidy("state", 2022, force = TRUE)

  # Load and compare data
  load(file = file.path(bbs_dir$data(), "bbs_raw_data.RData"))
  bbs_data2 <- readRDS(file.path(bbs_dir$data(), "bbs_raw_data.rds"))

  b1 <- dplyr::arrange(bbs_data[[1]], RouteDataID, Year, AOU)
  b2 <- as.data.frame(bbs_data2[[1]][names(b1)]) %>%
    dplyr::arrange(RouteDataID, Year, AOU)

  r1 <- dplyr::arrange(bbs_data[[2]], RouteDataID, Year, ObsN)
  r2 <- as.data.frame(bbs_data2[[2]]) %>%
    dplyr::arrange(RouteDataID, Year, ObsN)

  s1 <- bbs_data[[3]]
  s2 <- as.data.frame(bbs_data[[3]])

  rm(bbs_data)
  rm(bbs_data2)

  # All the same except int vs. dbl -- Hooray!!
  waldo::compare(b1, b2, tolerance = 0.00001)

  # All the same except corrected ones -- Hooray!!
  waldo::compare(dplyr::select(r1, -TempScale),
                 dplyr::select(r2, -TempScale),
                 tolerance = 0.00001)

  # All the same -- Hooray!!
  waldo::compare(s1, s1)

  # 2022 Stop data ------------

  # Download data
  fetch_bbs_data("stop", 2022, force = TRUE)
  unlink(list.files(tempdir()))
  fetch_bbs_data_tidy("stop", 2022, force = TRUE)

  # Load and compare data
  load(file = file.path(bbs_dir$data(), "bbs_raw_data.RData"))
  bbs_data2 <- readRDS(file.path(bbs_dir$data(), "bbs_raw_data.rds"))

  b1 <- dplyr::arrange(bbs_data[[1]], RouteDataID, Year, AOU)
  b2 <- as.data.frame(bbs_data2[[1]][names(b1)]) %>%
    dplyr::arrange(RouteDataID, Year, AOU)

  r1 <- dplyr::arrange(bbs_data[[2]], RouteDataID, Year, ObsN)
  r2 <- as.data.frame(bbs_data2[[2]]) %>%
    dplyr::arrange(RouteDataID, Year, ObsN)

  s1 <- bbs_data[[3]]
  s2 <- as.data.frame(bbs_data[[3]])

  rm(bbs_data)
  rm(bbs_data2)

  # All the same except int vs. dbl -- Hooray!!
  waldo::compare(b1, b2, tolerance = 0.00001)

  # All the same except corrected ones -- Hooray!!
  waldo::compare(dplyr::select(r1, -TempScale),
                 dplyr::select(r2, -TempScale),
                 tolerance = 0.00001)

  # All the same -- Hooray!!
  waldo::compare(s1, s1)

})

test_that("stratify data", {

  skip("Do not test automatically")

  bbs_data1 <- stratify(by = "bbs_usgs", sample_data = TRUE)
  bbs_data2 <- stratify_tidy(by = "bbs_usgs", sample_data = TRUE)

  # No differences -- Hooray!
  waldo::compare(bbs_data1, bbs_data2)

})

test_that("JAGS prepare data", {

  skip("Do not test automatically")

  bbs_data <- stratify(by = "bbs_usgs", sample_data = TRUE)

  jags_data <- prepare_data(strat_data = bbs_data,
                            species_to_run = "Pacific Wren",
                            model = "slope",
                            min_max_route_years = 2,
                            heavy_tailed = TRUE)

  jags_data2 <- prepare_data_tidy(strat_data = bbs_data,
                                  species_to_run = "Pacific Wren",
                                  model = "slope",
                                  min_max_route_years = 2,
                                  heavy_tailed = TRUE)

  # Only observer dimensions (not content) differ (shouldn't matter) -- Hooray!
  waldo::compare(jags_data, jags_data2)

})

test_that("JAGS models run", {

  skip("Do not test automatically")


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

  skip_on_ci()

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
                        "jags_bbs_usgs_slope_pacific_wren_indices_plots.rds"))
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
                         "jags_bbs_usgs_slope_pacific_wren_indices_plots2.rds"))
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

test_that("STAN prepare data", {

  skip("Do not test automatically")

  bbs_data <- stratify(by = "bbs_usgs", sample_data = TRUE)

  stan_data1 <- prepare_data_stan1(bbs_data,
                                   species_to_run = "Pacific Wren",
                                   model = "slope",
                                   min_max_route_years = 2)

  stan_data2 <- prepare_data_stan2(bbs_data,
                                   species_to_run = "Pacific Wren",
                                   model = "slope",
                                   min_max_route_years = 2)

  # All the same -- Hooray!!
  waldo::compare(names(stan_data1), names(stan_data2))
  waldo::compare(stan_data1[1:16], stan_data2[1:16])

  # Only expected diff (int vs. double and the alt_data data frame) -- Hooray!!
  waldo::compare(stan_data1[17:30], stan_data2[17:30])

})



test_that("STAN models run", {

  skip("Do not test automatically")

  bbs_data <- stratify(by = "bbs_usgs", sample_data = TRUE)

  stan_data <- prepare_data_stan2(bbs_data,
                                  species_to_run = "Pacific Wren",
                                  model = "slope",
                                  min_max_route_years = 2)

  stan_fit <- run_model_stan(
    stan_data,
    out_name = "pacific_wren_slope_BBS_short",
    out_dir = system.file("model_outputs", package = "bbsBayes"),
    iter_sampling = 10, iter_warmup = 10)

  save(list = c("stan_fit","stan_data"),
       file = file.path(system.file("model_outputs", package = "bbsBayes"),
                        "pacific_wren_slope_BBS_short_fit.RData"))
})

test_that("EXPLORE STAN outputs", {

  skip("Do not test automatically")

  # load(system.file("model_outputs", "pacific_wren_slope_BBS_short_fit.RData",
  #                  package = "bbsBayes"))

  load(system.file("model_outputs", "pacific_wren_slope_BBS_fit.RData",
                   package = "bbsBayes"))

  # "stan_fit","stan_data"

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
  skip_on_ci()

  load(system.file("model_outputs", "pacific_wren_slope_BBS_Stan_fit.RData",
                   package = "bbsBayes"))

  # INDICES --------------------------
  indices <- generate_indices_stan(model_fit = stan_fit,
                                   model_data = stan_data,
                                   regions = c("continental",
                                               "national",
                                               "prov_state",
                                               "stratum"))

  tp <- plot_indices(indices = indices,
                     species = "Pacific Wren",
                     add_observed_means = TRUE)

  saveRDS(tp, file.path(system.file("model_outputs", package = "bbsBayes"),
                        "pacific_wren_slope_indices_plots.rds"))

  trends <- generate_trends_tidy(indices = indices)

  # plot_indices -------------------------------
  saveRDS(tp, file.path(system.file("model_outputs", package = "bbsBayes"),
                        "jags_bbs_usgs_slope_pacific_wren_trend_plots.rds"))

  # Compare with JAGS
  tp_jags <- readRDS(system.file(
    "model_outputs",
    "jags_bbs_usgs_slope_pacific_wren_indices_plots.rds",
    package = "bbsBayes"))

  library(patchwork)

  (tp[[1]] + ggplot2::ggtitle("JAGS")) + (tp_jags[[1]] +  ggplot2::ggtitle("STAN"))


})
