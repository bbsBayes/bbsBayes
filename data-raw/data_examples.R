# Example data
d <- load_bbs_data(level = "state", release = 2022)

birds_sample <- dplyr::filter(d$birds, aou == 7221)
routes_sample <- dplyr::semi_join(d$routes, birds_sample,
                                  by = c("country_num", "state_num", "route",
                                         "route_data_id", "rpid", "year", "bcr"))
species_sample <- dplyr::filter(d$species, aou == 7221)

usethis::use_data(birds_sample, routes_sample, species_sample,
                  overwrite = TRUE)


# Mini Stan models to use in examples and testing

bbs_data <- stratify(by = "bbs_usgs", sample_data = TRUE)

stan_data <- prepare_data(bbs_data,
                          species_to_run = "Pacific Wren",
                          model = "slope",
                          min_max_route_years = 2)

pawr_fit <- run_model(
  stan_data,
  out_name = "pacific_wren_slope",
  out_dir = system.file("extdata", package = "bbsBayes"),
  iter_sampling = 10, iter_warmup = 10)


readr::write_rds(list = c("stan_fit","stan_data"),
                 file = file.path(system.file("extdata", package = "bbsBayes"),
                                  "pacific_wren_slope_fit.rds"))
