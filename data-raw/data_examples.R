# Example data - NEW -----------------------------------------------------
d <- load_bbs_data()

bbs_data_sample <- list(
  birds = dplyr::filter(d$birds, aou == 7221),
  routes = dplyr::semi_join(d$routes, dplyr::filter(d$birds, aou == 7221),
                            by = c("country_num", "state_num", "route",
                                   "route_data_id", "rpid", "year", "bcr")),
  species = dplyr::filter(d$species, aou == 7221))

# Example data - ORIG
# bbs_data_sample <- list(
#   birds = dplyr::rename_with(bird_sample, snakecase::to_snake_case) %>%
#     dplyr::rename(country_num = countrynum, state_num = statenum),
#   routes = dplyr::rename_with(route_sample, snakecase::to_snake_case) %>%
#     dplyr::rename(country_num = countrynum, state_num = statenum),
#   species = dplyr::rename_with(species_sample, snakecase::to_snake_case))


# Choose which to keep (for now ORIG useful for ensuring nothing has changed)
usethis::use_data(bbs_data_sample, overwrite = TRUE)


# Example model --------------------------------------------------------
pacific_wren_model <- stratify(by = "bbs_cws", sample_data = TRUE) %>%
  prepare_data() %>%
  prepare_model(model = "first_diff", set_seed = 111) %>%
  run_model(chains = 2, iter_sampling = 20, iter_warmup = 20, set_seed = 111)

usethis::use_data(pacific_wren_model, overwrite = TRUE)
unlink(list.files(pattern = paste0("BBS_STAN_first_diff_hier_", Sys.Date())))

# Testing models (internal data)
slope_test_model <- p %>%
  prepare_model(model = "slope", set_seed = 111) %>%
  run_model(chains = 2, iter_sampling = 20, iter_warmup = 20, set_seed = 111)

usethis::use_data(slope_test_model, internal = TRUE, overwrite = TRUE)
unlink(list.files(pattern = paste0("BBS_STAN_slope_hier_", Sys.Date())))
