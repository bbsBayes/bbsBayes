# Example data - NEW -----------------------------------------------------
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


# Example model --------------------------------------------------------
# First, stratify the sample data
s <- stratify(by = "bbs_cws", sample_data = TRUE)

# Prepare the stratified data for use in modelling
d <- prepare_data(s,
                  min_year = 2009,
                  max_year = 2018)

# Now run the model (fast but not good, just for illustration)
pacific_wren_model <- run_model(d, model = "first_diff",
                                iter_sampling = 20, iter_warmup = 20,
                                chains = 2)
usethis::use_data(pacific_wren_model, overwrite = TRUE)
