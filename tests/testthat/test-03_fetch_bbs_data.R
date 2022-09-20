test_that("fetch bbs data", {

  skip("Long running, only run as needed")

  connection <- sbtools::item_get(sb_id = get_sb_id(rel_date = 2020))

  expect_message(b <- get_counts_tidy("state", quiet = FALSE, connection, TRUE))
  expect_s3_class(b, "tbl")

  expect_message(b <- get_routes(2020, quiet = FALSE, connection, TRUE))
  expect_s3_class(b, "tbl")

  expect_message(w <- get_weather(connection, TRUE))
  expect_s3_class(w, "tbl")


  connection <- sbtools::item_get(sb_id = get_sb_id(rel_date = 2022))

  expect_message(b <- get_counts_tidy("state", quiet = FALSE, connection, TRUE))
  expect_s3_class(b, "tbl")

  expect_message(r <- get_routes(2022, quiet = FALSE, connection, TRUE))
  expect_s3_class(r, "tbl")

  expect_message(w <- get_weather(connection, TRUE))
  expect_s3_class(w, "tbl")

})
