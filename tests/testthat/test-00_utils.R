test_that("ext()", {
  expect_equal(ext("hello"), NA_character_)
  expect_equal(ext("hello.txt"), "txt")
  expect_equal(ext("hello.tiff"), "tiff")
})

test_that("load_internal_file()", {
  for(b in c("bbs_cws", "bbs_usgs")) {
    expect_silent(f <- load_internal_file("geofacet-grids", b))
    expect_s3_class(f, "data.frame")
  }
})

test_that("get_geo_types()", {
  m <- load_map("bbs_cws")
  expect_silent(get_geo_types(m)) %>%
    expect_equal("POLYGON")

  pts <- load_bbs_data(sample = TRUE)$routes %>%
    sf::st_as_sf(coords = c("longitude", "latitude"))
  expect_silent(get_geo_types(pts)) %>%
    expect_equal("POINT")
})


test_that("format_ne_states()", {
  expect_silent(ps <- format_ne_states()) %>%
    expect_s3_class("sf")
})
