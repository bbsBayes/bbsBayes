
expect_silent({
  r <- pacific_wren_model
  i1 <- generate_indices(r, quiet = TRUE)
  i2 <- generate_indices(r, start_year = 2007, quiet = TRUE)
})

test_that("plot_indices()", {

  expect_silent(p <- plot_indices(i1))
  expect_named(
    p, stringr::str_replace_all(unique(i1[["indices"]]$region), "-", "_"),
    ignore.order = TRUE)
  expect_s3_class(p[[1]], "ggplot")

  expect_silent(p1 <- plot_indices(i1, add_observed_means = TRUE))
  expect_silent(p2 <- plot_indices(i1, add_number_routes = TRUE))
  expect_silent(p3 <- plot_indices(i1, add_observed_means = TRUE,
                                   add_number_routes = TRUE))
  expect_equal(p[[1]]$data, p1[[1]]$data)
  expect_equal(p1[[1]]$data, p2[[1]]$data)
  expect_equal(p2[[1]]$data, p3[[1]]$data)

  expect_lt(length(p[[1]]$layers), length(p1[[1]]$layers))
  expect_lt(length(p1[[1]]$layers), length(p2[[1]]$layers))
  expect_lt(length(p1[[1]]$layers), length(p3[[1]]$layers))

  expect_silent(plot_indices(i2))
})


expect_silent({
  r <- pacific_wren_model
  i1 <- generate_indices(r, regions = c("stratum", "prov_state"), quiet = TRUE)
  t1 <- generate_trends(i1)
  t2 <- generate_trends(i1, slope = TRUE)
  t3 <- generate_trends(i1, min_year = 2000, max_year = 2005)

  i2 <- generate_indices(r, regions = c("stratum", "prov_state"),
                         start_year = 2007, quiet = TRUE)
  t4 <- generate_trends(i2)
})

test_that("plot_geofacet() diff trends", {
  # Error/message
  expect_error(plot_geofacet(i1, trends = t4), "created from the same")
  expect_error(plot_geofacet(i1, trends = t1, slope = TRUE),
               "also run with `slope = TRUE`")

  # Plot combos
  expect_silent(plot_geofacet(i1, trends = t1))
  expect_silent(plot_geofacet(i1, trends = t2, slope = TRUE))
  expect_silent(plot_geofacet(i1, trends = t3))
  expect_silent(plot_geofacet(i1, trends = t1, multiple = TRUE))
  expect_silent(plot_geofacet(i1, trends = t1, multiple = TRUE,
                              col_viridis = TRUE))
  expect_silent(plot_geofacet(i1, multiple = TRUE))
  expect_silent(plot_geofacet(i1, trends = t1, multiple = FALSE))
  expect_silent(plot_geofacet(i1, multiple = FALSE))

  # PLot combos, diff years
  expect_silent(plot_geofacet(i2, trends = t4))
  expect_silent(plot_geofacet(i2, trends = t4, multiple = TRUE))
  expect_silent(plot_geofacet(i2, trends = t4, multiple = TRUE,
                              col_viridis = TRUE))
  expect_silent(plot_geofacet(i2, multiple = TRUE))
  expect_silent(plot_geofacet(i2, trends = t4, multiple = FALSE))
  expect_silent(plot_geofacet(i2, multiple = FALSE))
})

test_that("plot_map() diff trends", {
  # Error/message
  expect_error(plot_map(t1, slope = TRUE), "also run with `slope = TRUE`")

  # Plot combos
  expect_silent(plot_map(t1))
  expect_silent(plot_map(t2, slope = TRUE))
  expect_silent(plot_map(t1, title = FALSE))
  expect_silent(plot_map(t1, col_viridis = TRUE))

  expect_silent(plot_map(t3)) # diff year range
})

