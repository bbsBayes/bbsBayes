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

  expect_equal(n[["n"]], length(nb_weights[["num"]]))
  expect_equal(n[["n_edges"]], length(nb_weights[["adj"]])/2)
  expect_equal(length(n[["node1"]]), length(nb_weights[["adj"]])/2)
  expect_equal(length(n[["node2"]]), length(nb_weights[["adj"]])/2)

  # Snapshots can't be run interactively
  expect_snapshot_value_safe(n[["node1"]], "json2")
  expect_snapshot_value_safe(n[["node2"]], "json2")

})

test_that("fix_no_neighbours()", {
  m <- load_map("latlong") %>%
    dplyr::filter(stringr::str_detect(strata_name, "(^51)|(^38)"))

  nb_db <- spdep::poly2nb(m, queen = FALSE)
  sf::st_agr(m) <- "constant"
  centres <- sf::st_centroid(m)
  nb_weights <- spdep::nb2WB(nb_db)
  nn <- spdep::knearneigh(centres, k = 2)[[1]]
  n <- which(nb_weights[["num"]] == 0)

  expect_silent(nb_db2 <- fix_no_neighbours(n[1], nb_db, nn))
  expect_true(spdep::card(nb_db)[n[1]] == 0)     # No neighbours
  expect_false(spdep::card(nb_db2)[n[1]] == 0)   # Neighbours
  expect_true(all(spdep::card(nb_db)[nn[n[1],]] == 1))   # some neighbours
  expect_true(all(spdep::card(nb_db2)[nn[n[1],]] == 2))  # more neighbours
})

test_that("fix_islands()", {
  m <- load_map("latlong") %>%
    dplyr::filter(stringr::str_detect(strata_name, "(^51)|(^38)"))

  nb_db <- spdep::poly2nb(m, queen = FALSE)
  sf::st_agr(m) <- "constant"
  centres <- sf::st_centroid(m)

  expect_silent(nb_db2 <- fix_islands(nb_db, centres, 1.2, TRUE))

  expect_true(sum(spdep::card(nb_db) == 0) > 0)
  expect_false(sum(spdep::card(nb_db2) == 0) > 0)

  # Snapshots can't be run interactively
  expect_snapshot_value_safe(nb_db2, "json2")
})

test_that("prepare_spatial() defaults", {

  map <- load_map("bbs_cws")
  p <- stratify(by = "bbs_cws", sample_data = TRUE) %>%
    prepare_data() %>%
    suppressMessages()

  expect_message(n <- prepare_spatial(p, map),
                 "Preparing") %>%
    expect_message("Identifying neighbours \\(non-Voronoi method\\)") %>%
    expect_message("Formating") %>%
    expect_message("Plotting")

  n %>%
    expect_type("list") %>%
    expect_named(c("spatial_data", "model_data", "meta_data", "meta_strata",
                   "raw_data"))

  n <- n[["spatial_data"]]
  expect_named(n, c("n", "n_edges", "node1", "node2", "adj_matrix", "map"))

  # Expect matches subset of map
  map_sub <- dplyr::filter(map, strata_name %in% p[["meta_strata"]]$strata_name)
  expect_true(is.matrix(n[["adj_matrix"]]))
  expect_true(all(dim(n[["adj_matrix"]]) == nrow(map_sub)))
  expect_s3_class(n[["map"]], "ggplot")

  # Snapshots can't be run interactively
  expect_snapshot_value_safe(n[["n_edges"]], "json2")
  expect_snapshot_value_safe(n[["node1"]], "json2")
  expect_snapshot_value_safe(n[["node2"]], "json2")
  expect_snapshot_value_safe(n[["adj_matrix"]], "json2")
})

test_that("prepare_spatial(nearest_fill = TRUE)", {
  map <- load_map("latlong")
  p <- stratify(by = "latlong", sample_data = TRUE) %>%
    prepare_data() %>%
    suppressMessages()

  expect_message(n <- prepare_spatial(p, map, nearest_fill = TRUE),
                 "Preparing") %>%
    expect_message("Identifying neighbours \\(non-Voronoi method\\)") %>%
    expect_message("Some strata have no neighbours") %>%
    expect_message("Linking islands") %>%
    expect_message("  Islands found") %>%
    expect_message("  Islands found") %>%
    expect_message("  Islands found") %>%
    expect_message("Formating") %>%
    expect_message("Plotting")

  n <- n[["spatial_data"]]

  # Snapshots can't be run interactively
  expect_snapshot_value_safe(n[["n_edges"]], "json2")
  expect_snapshot_value_safe(n[["node1"]], "json2")
  expect_snapshot_value_safe(n[["node2"]], "json2")
  expect_snapshot_value_safe(n[["adj_matrix"]], "json2")

})

test_that("prepare_spatial(voronoi = TRUE)", {

  map <- load_map("bbs_cws")
  p <- stratify(by = "bbs_cws", sample_data = TRUE) %>%
    prepare_data() %>%
    suppressMessages()

  expect_message(n <- prepare_spatial(p, map, voronoi = TRUE),
                 "Preparing") %>%
    expect_message("Identifying neighbours \\(Voronoi method\\)") %>%
    expect_message("Formating") %>%
    expect_message("Plotting")

  n <- n[["spatial_data"]]

  n %>%
    expect_type("list") %>%
    expect_named(c("n", "n_edges", "node1", "node2", "adj_matrix", "map"))

  # Expect matches subset of map
  map_sub <- dplyr::filter(map, strata_name %in% p[["meta_strata"]]$strata_name)
  expect_true(is.matrix(n[["adj_matrix"]]))
  expect_true(all(dim(n[["adj_matrix"]]) == nrow(map_sub)))
  expect_s3_class(n[["map"]], "ggplot")

  expect_equal(n[["n"]], nrow(map_sub))

  # Snapshots can't be run interactively
  expect_snapshot_value_safe(n[["n_edges"]], "json2")
  expect_snapshot_value_safe(n[["node1"]], "json2")
  expect_snapshot_value_safe(n[["node2"]], "json2")
  expect_snapshot_value_safe(n[["adj_matrix"]], "json2")

})
