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
