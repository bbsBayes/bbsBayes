load_test_model <- function(model = "first_diff", model_variant = "hier") {
  testthat::test_path() %>%
    list.files(pattern = paste0("BBS_STAN_", model, "_", model_variant,
                                "[_0-9-]+.rds"),
               full.names = TRUE) %>%
    dplyr::last() %>%
    readr::read_rds()
}
