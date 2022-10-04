test_that("get_species_aou()", {
  species <- load_bbs_data_tidy()$species

  expect_silent(get_species_aou(species, "Pacific Wren")) %>%
    expect_equal(7221)

  expect_silent(get_species_aou(species, "Wren")) %>%
    expect_equal(-1)

})
