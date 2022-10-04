test_that("stratify()", {

  expect_message(s <- stratify(by = "bbs_usgs"),
                 "Loading BBS data...") %>%
    expect_message("Lumping") %>%
    expect_message("Stratifying") %>%
    expect_message("Renaming")

  expect_named(s, c("birds_strat", "routes_strat", "species_strat", "stratify_by"))

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
