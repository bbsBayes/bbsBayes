
test_that("generate_trends()", {

  r <- readr::read_rds(test_path(paste0("BBS_STAN_first_diff_hier_", Sys.Date(), "_01.rds")))

  i <- generate_indices(r, quiet = TRUE)

  expect_silent(t <- generate_trends(i))
  expect_silent(t <- generate_trends(i, slope = TRUE))
})
