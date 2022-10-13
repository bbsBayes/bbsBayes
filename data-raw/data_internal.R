# Model choices for STAN hierarchical heirarchial

bbs_models <- dplyr::tribble(
  ~model,       ~variant,           ~file,
  "first_diff", "nonhier", "first_diff_nonhier_bbs_CV.stan",
  "first_diff", "hier",    "first_diff_hier_bbs_CV.stan",
  "first_diff", "spatial", "first_diff_spatial_bbs_CV.stan",
  "gam",        "hier",    "gam_hier_bbs_CV.stan",
  "gam",        "spatial", "gam_spatial_bbs_CV.stan",
  #"gamye",      "O1",     "gamye_O1_bbs_CV.stan",
  #"gamye",      "Oexp",   "gamye_Oexp_bbs_CV.stan",
  #"gamye",      "simple", "gamye_simple_bbs_CV.stan",
  "gamye",      "hier",    "gamye_hier_bbs_CV.stan",
  "gamye",      "spatial", "gamye_spatial_bbs_CV.stan",
  "slope",      "hier",    "slope_hier_bbs_CV.stan",
  "slope",      "spatial", "slope_spatial_bbs_CV.stan")



usethis::use_data(bbs_models, overwrite = TRUE, internal = TRUE)

