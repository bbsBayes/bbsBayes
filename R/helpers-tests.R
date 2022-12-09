load_test_model <- function(model = "first_diff", model_variant = "hier") {
  f <- testthat::test_path() %>%
    list.files(pattern = paste0("BBS_STAN_", model, "_", model_variant,
                                "[_0-9-]+.rds"),
               full.names = TRUE) %>%
    dplyr::last()

  if(is.na(f) || !file.exists(f)) {
    stop("Cannot find test model RDS file", call. = FALSE)
  }

  readr::read_rds(f)
}
