test_that("get_species_aou()", {
  species <- load_bbs_data()$species

  expect_silent(get_species_aou(species, "Pacific Wren")) %>%
    expect_equal(7221)

  expect_silent(get_species_aou(species, "Wren")) %>%
    expect_equal(-1)

})


test_that("ext()", {
  expect_equal(ext("hello"), NA_character_)
  expect_equal(ext("hello.txt"), "txt")
  expect_equal(ext("hello.tiff"), "tiff")
})

