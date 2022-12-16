test_that("bbs_dir() location", {
  skip_on_ci()
  expect_silent(d <- bbs_dir()) %>%
    expect_type("character")
  expect_true(dir.exists(d))
})

# These tests assume that you are starting with already having the most recent
# BBS data downloaded
test_that("have_bbs_data() / remove_cache()", {
  skip_on_ci()
  expect_message(h <- have_bbs_data(), "Expected BBS state data 2022")
  expect_true(h)
  expect_message(remove_cache(level = "state", release = 2022),
                 paste0("Removing ", bbs_dir(), "/bbs_state_data_2022.rds"))
  expect_message(remove_cache(level = "state", release = 2022),
                 "No data files to remove")
  expect_message(h <- have_bbs_data(), "Expected BBS state data 2022")
  expect_false(h)

  # Models
  model <- cmdstanr::cmdstan_model(check_model_file("first_diff", "hier"),
                                   dir = bbs_dir())
  expect_true("first_diff_hier_bbs_CV" %in% list.files(bbs_dir()))
  expect_message(remove_cache(type = "models"), "Removing")
  expect_message(remove_cache(type = "models"), "No data files to remove")
  expect_false("first_diff_hier_bbs_CV" %in% list.files(bbs_dir()))

  # Everything
  expect_true(dir.exists(d <- bbs_dir()))
  expect_message(remove_cache(type = "all"), "Removing all data files")
  expect_false(dir.exists(d))
})

test_that("bbs_dir() creation", {
  skip_on_ci()
  expect_message(d <- bbs_dir(quiet = FALSE), "Creating data director")
  expect_true(dir.exists(d))
})

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

test_that("fetch_bbs_data()", {

  skip_on_ci()

  # Adjust level and release to run all as required

  # Clear all
  #expect_message(remove_bbs_data(cache_dir = TRUE), "Removing all")

  level <- c("stop", "state")[2] # Add [2] to test only "state"
  release <- c(2020, 2022)[2]    # Add [2] to test only 2022

  for(l in level) {
    for(r in release) {
    f <- file.path(bbs_dir(), paste0("bbs_", l, "_data_", r, ".rds"))
    expect_false(file.exists(f))
    expect_message(fetch_bbs_data_internal(
      level = l, release = r, check_bbs_data(l, r, force = FALSE, quiet = FALSE),
      force = FALSE, quiet = FALSE), "Using data director") %>%
      expect_message("Connecting to USGS") %>%
      expect_message("Connected!") %>%
      expect_message("Downloading count data") %>%
      expect_message("Downloading route data") %>%
      expect_message("routes") %>%
      expect_message("weather") %>%
      expect_message("Downloading species data") %>%
      expect_message("Combining species forms") %>%
      expect_message("Saving BBS data") %>%
      expect_message("Removing temp files")
    expect_true(file.exists(f))

    expect_silent(b <- load_bbs_data(l, r)) %>%
      expect_type("list") %>%
      expect_named(c("birds", "routes", "species", "meta"))

    expect_s3_class(b$birds, "data.frame")
    expect_s3_class(b$routes, "data.frame")
    expect_s3_class(b$species, "data.frame")
    expect_s3_class(b$meta, "data.frame")

    expect_equal(b$meta$release, r)
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
