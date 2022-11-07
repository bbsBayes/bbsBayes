
test_that("generate_indices()", {

  r <- readr::read_rds(test_path(paste0("BBS_STAN_first_diff_hier_", Sys.Date(), "_01.rds")))

  expect_message(generate_indices(r), "Processing region stratum") %>%
    expect_message("Processing region continent")

})
