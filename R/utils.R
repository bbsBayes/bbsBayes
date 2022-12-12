ext <- function(file) {
 stringr::str_extract(file, "(?<=\\.)[[:alnum:]]+$")
}

load_internal_file <- function(name, stratify_by = NULL) {
  system.file(name, paste0(stratify_by, ".csv"), package = "bbsBayes") %>%
    readr::read_csv(show_col_types = FALSE, progress = FALSE)
}

get_geo_types <- function(strata_map) {
  sf::st_geometry_type(strata_map) %>%
    stringr::str_remove("MULTI") %>%
    unique()
}


format_ne_states <- function() {
  check_rnaturalearth()

  rnaturalearth::ne_states(
    country = c("United States of America", "Canada"), returnclass = "sf") %>%
    dplyr::select("province_state" = "name", "code_hasc",
                  "country" = "admin") %>%
    tidyr::separate(.data$code_hasc, sep = "\\.",
                    into = c("country_code", "prov_state")) %>%
    dplyr::mutate(prov_state = dplyr::if_else(
      .data$prov_state == "NF", "NL", .data$prov_state))
}
