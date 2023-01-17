test_that("stratify_map()", {
  routes <- load_bbs_data(sample = TRUE)$routes
  map <- load_map("bbs_cws")

  expect_message(s <- stratify_map(map, routes), "Preparing") %>%
    expect_message("  Calculating area weights") %>%
    expect_message("  Joining routes")
  expect_type(s, "list")
  expect_named(s, c("meta_strata", "routes"))
  expect_equal(routes, dplyr::select(s[["routes"]], -"strata_name"))
  expect_named(s[["meta_strata"]], c("strata_name", "area_sq_km"))
  expect_equal(nrow(s[["meta_strata"]]), nrow(map))
})

test_that("stratify_map() with messy map", {
  d <- test_path("../../../WBPHS_Stratum_Boundaries_2019/")
  skip_if(!dir.exists(d),
          "External data not present (see 'custom strat' vignette for data)")

  routes <- load_bbs_data(sample = TRUE)$routes
  map <- sf::st_read(d, quiet = TRUE) %>%
    dplyr::rename(strata_name = STRAT)

  expect_message(s <- stratify_map(map, routes), "Preparing") %>%
    expect_message("  Summarizing strata") %>%
    expect_message("  Calculating area weights") %>%
    expect_message("  Joining routes")
  expect_type(s, "list")
  expect_named(s, c("meta_strata", "routes"))
  expect_equal(routes, dplyr::select(s[["routes"]], -"strata_name"))
  expect_named(s[["meta_strata"]], c("strata_name", "area_sq_km"))

  # Summary step collapse polygons
  expect_lt(nrow(s[["meta_strata"]]), nrow(map))
})


test_that("stratify - bbs_cws", {

  expect_message(stratify(by = "bbs_cws", sample_data = TRUE),
                 "Using 'bbs_cws' \\(standard\\) stratification") %>%
    suppressMessages()

  expect_message(s <- stratify(by = "bbs_cws", species = "Snowy Owl"),
                 "Using 'bbs_cws' \\(standard\\) stratification") %>%
    expect_message("Loading BBS data...") %>%
    expect_message("Filtering to species Snowy Owl") %>%
    expect_message("Stratifying") %>%
    expect_message("Combining BCR") %>%
    expect_message("Renaming")

  # Check region combining (CWS)
  expect_false(any(c("NS","PE") %in% s$routes_strat$st_abrev))
  expect_true(any("NSPE" %in% s$routes_strat$st_abrev))

  # Check strata
  all(s$routes_strata$strata_name %in% bbs_strata[["bbs_cws"]]$strata_name) %>%
    expect_true()

  # Check meta
  expect_named(s, c("meta_data", "meta_strata", "birds_strata",
                    "routes_strata"))
  expect_equal(s$meta_data, list(stratify_by = "bbs_cws",
                                 stratify_type = "standard",
                                 species = "Snowy Owl"))

  comp <- bbs_strata[["bbs_cws"]] %>%
    dplyr::filter(strata_name %in% s$meta_strata$strata_name)
  expect_equal(s$meta_strata, comp)
})

test_that("stratify - bbs_usgs", {

  expect_message(stratify(by = "bbs_usgs", sample_data = TRUE),
                 "Using 'bbs_usgs' \\(standard\\) stratification") %>%
    suppressMessages()

  expect_message(s <- stratify(by = "bbs_usgs", species = "Snowy Owl"),
                 "Using 'bbs_usgs' \\(standard\\) stratification") %>%
    suppressMessages()


  # Check region combining (USGS should have none)
  expect_true("NS" %in% s$routes_strat$st_abrev)
  expect_true("PE" %in% s$routes_strat$st_abrev)
  expect_false(any("NSPE" %in% s$routes_strat$st_abrev))

  all(s$routes_strata$strata_name %in% bbs_strata[["bbs_usgs"]]$strata_name) %>%
    expect_true()

  # Check meta
  expect_named(s, c("meta_data", "meta_strata", "birds_strata",
                    "routes_strata"))
  expect_equal(s$meta_data, list(stratify_by = "bbs_usgs",
                                 stratify_type = "standard",
                                 species = "Snowy Owl"))

  comp <- bbs_strata[["bbs_usgs"]] %>%
    dplyr::filter(strata_name %in% s$meta_strata$strata_name)
  expect_equal(s$meta_strata, comp)

})

test_that("stratify - bcr", {

  expect_message(stratify(by = "bcr", sample_data = TRUE),
                 "Using 'bcr' \\(standard\\) stratification") %>%
    suppressMessages()

  expect_message(s <- stratify(by = "bcr", species = "Snowy Owl"),
                 "Using 'bcr' \\(standard\\) stratification") %>%
    suppressMessages()

  # Check strata
  all(s$routes_strata$strata_name %in% bbs_strata[["bcr"]]$strata_name) %>%
    expect_true()

  # Check meta
  expect_named(s, c("meta_data", "meta_strata", "birds_strata",
                    "routes_strata"))
  expect_equal(s$meta_data, list(stratify_by = "bcr",
                                 stratify_type = "standard",
                                 species = "Snowy Owl"))

  comp <- bbs_strata[["bcr"]] %>%
    dplyr::filter(strata_name %in% s$meta_strata$strata_name)
  expect_equal(s$meta_strata, comp)

})

test_that("stratify - latlong & return_omitted", {

  expect_message(stratify(by = "latlong", sample_data = TRUE),
                 "Using 'latlong' \\(standard\\) stratification") %>%
    suppressMessages()

  expect_message(s <- stratify(by = "latlong", species = "Snowy Owl"),
                 "Using 'latlong' \\(standard\\) stratification") %>%
    expect_message("To see omitted routes") %>%
    suppressMessages()

  expect_false("routes_omitted" %in% names(s))

  expect_message(s <- stratify(by = "latlong", species = "Snowy Owl",
                               return_omitted = TRUE),
                 "Using 'latlong' \\(standard\\) stratification") %>%
    expect_message("Returning omitted routes") %>%
    suppressMessages()

  expect_true("routes_omitted" %in% names(s))
  expect_true(all(names(s[["routes_omitted"]]) %in% names(s[["routes_strata"]])))
  expect_true(all(is.na(s[["routes_omitted"]]$strata_name)))
  expect_equal(nrow(dplyr::semi_join(s[["routes_strata"]],
                                    s[["routes_omitted"]],
                                    by = c("year", "route"))),
               0) # No overlap

  # Check meta
  expect_named(s, c("meta_data", "meta_strata", "birds_strata",
                    "routes_strata", "routes_omitted"))
  expect_equal(s$meta_data, list(stratify_by = "latlong",
                                 stratify_type = "standard",
                                 species = "Snowy Owl"))

  comp <- bbs_strata[["latlong"]] %>%
    dplyr::filter(strata_name %in% s$meta_strata$strata_name)
  expect_equal(s$meta_strata, comp)

})

test_that("stratify - prov_state", {

  expect_message(stratify(by = "prov_state", sample_data = TRUE),
                 "Using 'prov_state' \\(standard\\) stratification") %>%
    suppressMessages()

  expect_message(s <- stratify(by = "prov_state", species = "Snowy Owl"),
                 "Using 'prov_state' \\(standard\\) stratification") %>%
    suppressMessages()

  # Check strata
  all(s$routes_strata$strata_name %in%
        bbs_strata[["prov_state"]]$strata_name) %>%
    expect_true()

  # Check meta
  expect_named(s, c("meta_data", "meta_strata", "birds_strata",
                    "routes_strata"))
  expect_equal(s$meta_data, list(stratify_by = "prov_state",
                                 stratify_type = "standard",
                                 species = "Snowy Owl"))

  comp <- bbs_strata[["prov_state"]] %>%
    dplyr::filter(strata_name %in% s$meta_strata$strata_name)
  expect_equal(s$meta_strata, comp)

})


test_that("stratify - subset", {

  sub <- dplyr::filter(bbs_strata[["bbs_cws"]], country == "Canada")

  expect_message(s <- stratify(by = "bbs_cws", strata_custom = sub,
                               species = "Snowy Owl"),
                 "Using 'bbs_cws' \\(subset\\) stratification") %>%
    expect_message("Loading BBS data...") %>%
    expect_message("Filtering to species Snowy Owl") %>%
    expect_message("Stratifying") %>%
    expect_message("  Combining BCR") %>%
    expect_message("  Omitting") %>% # Because subset!
    expect_message("  Renaming")

  # Check strata
  all(s$routes_strata$strata_name %in% bbs_strata[["bbs_cws"]]$strata_name) %>%
    expect_true()

  # Check meta
  expect_named(s, c("meta_data", "meta_strata", "birds_strata",
                    "routes_strata"))
  expect_equal(s$meta_data, list(stratify_by = "bbs_cws",
                                 stratify_type = "subset",
                                 species = "Snowy Owl"))

  comp <- bbs_strata[["bbs_cws"]] %>%
    dplyr::filter(strata_name %in% s$meta_strata$strata_name)
  expect_equal(s$meta_strata, comp)

})

test_that("stratify - custom", {

  map <- load_map(stratify_by = "bbs_cws")

  expect_error(stratify(by = "my_cws"), "Invalid")

  expect_message(s <- stratify(by = "my_cws", strata_custom = map,
                               species = "Snowy Owl"),
                 "Using 'my_cws' \\(custom\\) stratification") %>%
    expect_message("Loading BBS data...") %>%
    expect_message("Filtering to species Snowy Owl") %>%
    expect_message("Stratifying") %>%
    expect_message("Preparing") %>%
    expect_message("  Calculating") %>%
    expect_message("  Joining") %>%
    expect_message("  Omitting") %>%  # Because subset!
    expect_message("  Renaming")

  # Check meta
  expect_named(s, c("meta_data", "meta_strata", "birds_strata",
                    "routes_strata"))
  expect_equal(s$meta_data, list(stratify_by = "my_cws",
                                 stratify_type = "custom",
                                 species = "Snowy Owl"))

  # (should match because same map)
  comp <- bbs_strata[["bbs_cws"]] %>%
    dplyr::filter(strata_name %in% s$meta_strata$strata_name) %>%
    dplyr::select(strata_name, area_sq_km)
  expect_equal(s$meta_strata, comp)

})
