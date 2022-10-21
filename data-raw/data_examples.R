# Example data
d <- load_bbs_data(level = "state", release = 2020)

birds_sample <- dplyr::filter(d$birds, aou == 7221)
routes_sample <- dplyr::semi_join(d$routes, birds_sample,
                                  by = c("country_num", "state_num", "route",
                                         "route_data_id", "rpid", "year", "bcr"))
species_sample <- dplyr::filter(d$species, aou == 7221)

usethis::use_data(birds_sample, routes_sample, species_sample,
                  overwrite = TRUE)


# Mini Stan models to use in examples and testing

s <- stratify(by = "bbs_usgs", sample_data = TRUE)
p <- prepare_data(s, species = "Pacific Wren", min_max_route_years = 2)

pawr_fit <- run_model(p,
                      model = "slope",
                      out_name = "pacific_wren_slope",
                      out_dir = system.file("extdata", package = "bbsBayes"),
                      iter_sampling = 10, iter_warmup = 10)


readr::write_rds(pawr_fit,
                 file = file.path(system.file("extdata", package = "bbsBayes"),
                                  "pacific_wren_slope_fit.rds"))
