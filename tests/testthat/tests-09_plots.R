
test_that("plot_indices()", {

  withr::local_seed(111)

  r <- readr::read_rds(test_path(paste0("BBS_STAN_first_diff_hier_", Sys.Date(), "_01.rds")))
  i <- generate_indices(r, quiet = TRUE)

  #t1 <- generate_trends(i)
  #t2 <- generate_trends(i, slope = TRUE)

  expect_silent(plot_indices(i))
  expect_silent(plot_indices(i, add_observed_means = TRUE))
  expect_silent(plot_indices(i, add_number_routes = TRUE))
  expect_silent(plot_indices(i, add_observed_means = TRUE,
                             add_number_routes = TRUE))
})

