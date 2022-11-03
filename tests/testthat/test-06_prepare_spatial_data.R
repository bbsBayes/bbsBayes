test_that("nb_fmt()", {

  # Eg nb_weights created from example data in ?prepare_spatial
  # (limited to Pacific Wren)
  nb_weights <- list(
    num = c(2, 7, 3, 6, 4, 1, 2, 3, 2, 2, 3, 6, 3, 3, 4, 5, 6, 3, 7),
    adj = c(2, 13, 1, 3, 4, 5, 12, 13, 17, 2, 4, 8, 2, 3, 5, 8, 18, 19, 2, 4,
            17, 19, 7, 6, 8, 3, 4, 7, 10, 11, 9, 11, 9, 10, 15, 2, 13, 14, 16,
            17, 19, 1, 2, 12, 12, 16, 17, 11, 16, 18, 19, 12, 14, 15, 17,
            19, 2, 5, 12, 14, 16, 19, 4, 15, 19, 4, 5, 12, 15, 16, 17, 18))

  # Tests
  expect_silent(n <- nb_fmt(nb_weights)) %>%
    expect_type("list") %>%
    expect_named(c("n", "n_edges", "node1", "node2"))

  expect_equal(n$n, length(nb_weights$num))
  expect_equal(n$n_edges, length(nb_weights$adj)/2)
  expect_equal(length(n$node1), length(nb_weights$adj)/2)
  expect_equal(length(n$node2), length(nb_weights$adj)/2)

  # Snapshots can't be run interactively
  expect_snapshot_value(n$node1, "json2")
  expect_snapshot_value(n$node2, "json2")

})

test_that("fix_no_neighbours()", {



})

test_that("fix_islands()", {



})

test_that("prepare_spatial() defaults", {

  map <- load_map("bbs_cws")

  expect_message(n <- prepare_spatial(map),
                 "Preparing") %>%
    expect_message("Identifying neighbours \\(non-Voronoi method\\)") %>%
    expect_message("  Isolated") %>%
    expect_message("Formating") %>%
    expect_message("Plotting")

  n %>%
    expect_type("list") %>%
    expect_named(c("n", "n_edges", "node1", "node2", "adj_matrix", "map",
                   "strata_meta"))

  expect_true(is.matrix(n$adj_matrix))
  expect_true(all(dim(n$adj_matrix) == nrow(map)))
  expect_s3_class(n$map, "ggplot")
  expect_equal(n$strata_meta, sf::st_drop_geometry(map[, "strata_name"]))
})

test_that("prepare_spatial() vironoi", {

  map <- load_map("bbs_cws")

  expect_message(n <- prepare_spatial(map, voronoi = TRUE),
                 "Preparing") %>%
    expect_message("Identifying neighbours \\(Voronoi method\\)") %>%
    expect_message("Formating") %>%
    expect_message("Plotting")

  n %>%
    expect_type("list") %>%
    expect_named(c("n", "n_edges", "node1", "node2", "adj_matrix", "map",
                   "strata_meta"))

  expect_true(is.matrix(n$adj_matrix))
  expect_true(all(dim(n$adj_matrix) == nrow(map)))
  expect_s3_class(n$map, "ggplot")
  expect_equal(n$strata_meta, sf::st_drop_geometry(map[, "strata_name"]))
  expect_equal(n$n, nrow(map))

  # Snapshots can't be run interactively
  expect_snapshot_value(n$n_edges, "json2")
  expect_snapshot_value(n$node1, "json2")
  expect_snapshot_value(n$node2, "json2")
  expect_snapshot_value(n$adj_matrix, "json2")

})
