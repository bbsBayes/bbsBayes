test_that("get_XXXX()", {

  skip("Long running, only run as needed")

  connection <- sbtools::item_get(sb_id = get_sb_id(rel_date = 2020))

  expect_message(b <- get_birds("state", quiet = FALSE, connection, TRUE))
  expect_s3_class(b, "tbl")
  expect_message(b <- get_birds("stop", quiet = FALSE, connection, TRUE))
  expect_s3_class(b, "tbl")

  expect_message(r <- get_routes(2020, quiet = FALSE, connection, TRUE))
  expect_s3_class(r, "tbl")

  expect_message(w <- get_weather(connection, TRUE))
  expect_s3_class(w, "tbl")


  connection <- sbtools::item_get(sb_id = get_sb_id(rel_date = 2022))

  expect_message(b <- get_birds("state", quiet = FALSE, connection, TRUE))
  expect_s3_class(b, "tbl")
  expect_message(b <- get_birds("stop", quiet = FALSE, connection, TRUE))
  expect_s3_class(b, "tbl")

  expect_message(r <- get_routes(2022, quiet = FALSE, connection, TRUE))
  expect_s3_class(r, "tbl")

  expect_message(w <- get_weather(connection, TRUE))
  expect_s3_class(w, "tbl")

})

test_that("fetch bbs data", {

 skip("Long running, run by hand as needed")

  # Clear all
  expect_message(remove_bbs_data(cache_dir = TRUE), "Removing all")

  for(i in 1:2) {
    for(j in 1:2) {
    level <- c("stop", "state")[i]
    release <- c(2020, 2022)[j]
    f <- file.path(bbs_dir(), paste0("bbs_", level, "_data_", release, ".rds"))
    expect_false(file.exists(f))
    expect_message(fetch_bbs_data(level = level, release = release))
    expect_true(file.exists(f))

    expect_silent(b <- load_bbs_data(level, release)) %>%
      expect_type("list") %>%
      expect_named(c("birds", "routes", "species", "meta"))

    expect_s3_class(b$birds, "data.frame")
    expect_s3_class(b$routes, "data.frame")
    expect_s3_class(b$species, "data.frame")
    expect_s3_class(b$meta, "data.frame")

    expect_equal(b$meta$release, release)
    expect_equal(b$meta$download_date, Sys.Date())
    }
  }
})


test_that("combine_species()", {

  bbs_data <- load_bbs_data()
  birds <- bbs_data$birds %>% dplyr::filter(!unid_combined)
  species <- bbs_data$species %>% dplyr::filter(!unid_combined)

  expect_message(b <- combine_species(birds, species),
                 "Combining species forms")

  expect_type(b, "list") %>%
    expect_named(c("birds", "species"))

  s <- b$species
  b <- b$birds

  # Check bird counts

  expect_gt(nrow(b), nrow(birds))
  expect_true(all(birds$aou == b$aou[seq_along(birds$aou)]))

  # Check specific combo
  d1 <- dplyr::filter(birds, aou %in% c(2970, 2971, 2973))
  b1 <- dplyr::filter(b, aou %in% c(2970, 2971, 2973))

  # Expect combining to have occurred
  dc1 <- dplyr::count(d1, .data$aou)
  bc1 <- dplyr::count(b1, .data$aou)

  # Final AOU cat (2973) should have all AOU counts in stratified
  expect_equal(sum(dc1$n), bc1$n[3])

  # But others do not change
  expect_equal(dc1$n[1], bc1$n[1])
  expect_equal(dc1$n[2], bc1$n[2])


  # Check species list

  expect_equal(nrow(s), nrow(species) * 2)
  expect_true(all(species$aou == s$aou[seq_along(species$aou)]))

  # Check specific combo
  d1 <- dplyr::filter(species, aou %in% c(2970, 2971, 2973)) %>%
    dplyr::select(-unid_combined)
  s1 <- dplyr::filter(s, aou %in% c(2970, 2971, 2973), unid_combined) %>%
    dplyr::select(-unid_combined)

  # Final AOU cat (2973) should be the only one really different
  # and only when combined
  expect_equal(d1[1:2,], s1[1:2,])
  expect_true(all(d1[3, c("english", "french")] !=
                    s1[3, c("english", "french")]))

})
