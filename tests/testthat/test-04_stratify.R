test_that("stratify(by = 'bbs_usgs')", {

  expect_message(s <- stratify(by = "bbs_usgs"),
                 "Loading BBS data...") %>%
    expect_message("Lumping") %>%
    expect_message("Stratifying") %>%
    expect_message("Renaming")

  expect_named(s, c("birds_strata", "routes_strata", "species_strata", "stratify_by"))

  # Check region combining (USGS should have none)
  expect_true("NS" %in% s$routes_strat$st_abrev)
  expect_true("PE" %in% s$routes_strat$st_abrev)
  expect_false(any("NSPE" %in% s$routes_strat$st_abrev))

  # Check species lumping
  bbs_data <- load_bbs_data()
  d1 <- dplyr::filter(bbs_data$birds, aou %in% c(2970, 2971, 2973))
  s1 <- dplyr::filter(s$birds_strat, aou %in% c(2970, 2971, 2973))

  # Expect lumping to have occurred
  expect_gt(nrow(s1), nrow(d1))
  dc1 <- dplyr::count(d1, .data$aou)
  sc1 <- dplyr::count(s1, .data$aou)

  # Final AOU cat (2973) should have all AOU counts in stratified
  expect_equal(sum(dc1$n), sc1$n[3])

  # But others do not change
  expect_equal(dc1$n[1], sc1$n[1])
  expect_equal(dc1$n[2], sc1$n[2])
})


test_that("stratify(by = 'bbs_cws')", {

  expect_message(s <- stratify(by = "bbs_cws", sample_data = TRUE),
                 "Stratifying") %>%
    expect_message("Combining") %>%
    expect_message("Renaming")

  expect_message(s <- stratify(by = "bbs_cws"),
                 "Loading BBS data...") %>%
    expect_message("Lumping") %>%
    expect_message("Stratifying") %>%
    expect_message("Combining") %>%
    expect_message("Renaming")

  expect_named(s, c("birds_strat", "routes_strat", "species_strat", "stratify_by"))

  # Check region combining (CWS)
  expect_false(any(c("NS","PE") %in% s$routes_strat$St_Abrev))
  expect_true(any("NSPE" %in% s$routes_strat$St_Abrev))

  # Check species lumping
  bbs_data <- load_bbs_data()
  d1 <- dplyr::filter(bbs_data$birds, AOU  %in% c(2970, 2971, 2973))
  s1 <- dplyr::filter(s$birds_strat, AOU %in% c(2970, 2971, 2973))

  # Expect lumping to have occurred
  expect_gt(nrow(s1), nrow(d1))
  dc1 <- dplyr::count(d1, .data$AOU)
  sc1 <- dplyr::count(s1, .data$AOU)

  # Final AOU cat (2973) should have all AOU counts in stratified
  expect_equal(sum(dc1$n), sc1$n[3])

  # But others do not change
  expect_equal(dc1$n[1], sc1$n[1])
  expect_equal(dc1$n[2], sc1$n[2])
})
