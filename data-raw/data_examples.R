# Example data - NEW
d <- load_bbs_data(level = "state", release = 2020)

bbs_data_sample <- list(
  birds = dplyr::filter(d$birds, aou == 7221),
  routes = dplyr::semi_join(d$routes, birds_sample,
                            by = c("country_num", "state_num", "route",
                                   "route_data_id", "rpid", "year", "bcr")),
  species = dplyr::filter(d$species, aou == 7221))

# Example data - ORIG
bbs_data_sample <- list(
  birds = dplyr::rename_with(bird_sample, snakecase::to_snake_case) %>%
    dplyr::rename(country_num = countrynum, state_num = statenum),
  routes = dplyr::rename_with(route_sample, snakecase::to_snake_case) %>%
    dplyr::rename(country_num = countrynum, state_num = statenum),
  species = dplyr::rename_with(species_sample, snakecase::to_snake_case))

# Choose which to keep (for now ORIG useful for ensuring nothing has changed)
usethis::use_data(bbs_data_sample, overwrite = TRUE)
