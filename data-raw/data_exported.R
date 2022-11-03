
species_forms <- readr::read_csv("data-raw/species_forms.csv",
  col_types = "ncccnnnnnn", locale = readr::locale(encoding = "latin1"),
  progress = FALSE) %>%
  dplyr::mutate(aou_add = purrr::pmap(list(aou1, aou2, aou3, aou4, aou5),
                                      ~c(...)),
                aou_add = purrr::map(aou_add, ~.x[!is.na(.x)])) %>%
  dplyr::select(-dplyr::matches("aou[1-5]{1}"), -n_add) %>%
  dplyr::rename(aou_unid = aou_original,
                aou_id = aou_add,
                english_combined = english_out,
                french_combined = french_out) %>%
  as.data.frame()

usethis::use_data(species_forms, overwrite = TRUE)


bbs_models <- dplyr::tribble(
  ~model,       ~variant,           ~file,
  "first_diff", "nonhier", "first_diff_nonhier_bbs_CV.stan",
  "first_diff", "hier",    "first_diff_hier_bbs_CV.stan",
  "first_diff", "spatial", "first_diff_spatial_bbs_CV.stan",
  "gam",        "hier",    "gam_hier_bbs_CV.stan",
  "gam",        "spatial", "gam_spatial_bbs_CV.stan",
  "gamye",      "hier",    "gamye_hier_bbs_CV.stan",
  "gamye",      "spatial", "gamye_spatial_bbs_CV.stan",
  "slope",      "hier",    "slope_hier_bbs_CV.stan",
  "slope",      "spatial", "slope_spatial_bbs_CV.stan")

usethis::use_data(bbs_models, overwrite = TRUE)



