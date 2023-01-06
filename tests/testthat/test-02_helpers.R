test_that("search_species()", {

  d <- data.frame(
    term = c("Paridae", "chickadee", "mésang", "Poecile"),
    match = c("(c|C)hickadee|Titmouse", "(c|C)hickadee",
              "(c|C)hickadee|Jay|(t|T)it", "(c|C)hickadee"))

  for(i in seq_len(nrow(d))) {
    expect_silent(s <- search_species(d$term[i])) %>%
      expect_s3_class("data.frame")
    expect_gt(nrow(s), 0)
    expect_named(s, names(bbs_data_sample$species)[-1])
    expect_true(all(stringr::str_detect(s$english, d$match[i])))
  }

  # Combinable
  expect_silent(s <- search_species("blue grouse"))
  expect_equal(nrow(s), 1)

  expect_silent(s <- search_species("sooty grouse"))
  expect_equal(nrow(s), 2)

  # To combine or not
  expect_silent(s <- search_species("blue grouse",
                                    combine_species_forms = FALSE))
  expect_equal(nrow(s), 0)

  expect_silent(s <- search_species("northern flicker \\(all forms\\)"))
  expect_equal(nrow(s), 1)
  expect_silent(s <- search_species("northern flicker \\(all",
                                    combine_species_forms = FALSE))
  expect_equal(nrow(s), 0)
})

test_that("load_map()", {
  for(b in names(bbs_strata)) {
    expect_silent(m <- load_map(b))
    expect_s3_class(m, "sf")
  }
})


test_that("assign_prov_state()", {

  map <- load_map("prov_state")

  expect_silent(d <- assign_prov_state(map))
  expect_s3_class(d, "sf")

  expect_silent(d <- assign_prov_state(map, plot = TRUE))
  expect_s3_class(d, "sf")

  expect_silent(d <- assign_prov_state(map, keep_spatial = FALSE))
  expect_s3_class(d, "data.frame")
  expect_false(inherits(d, "sf"))


  map <- load_map("bcr")
  expect_error(d <- assign_prov_state(map, min_overlap = 0.4))
  expect_warning(d <- assign_prov_state(map, min_overlap = 0.51))
  expect_s3_class(d, "sf")

  map <- load_map("bbs_usgs")
  expect_warning(d <- assign_prov_state(map))
  expect_silent(d <- assign_prov_state(map, min_overlap = 0.51))

  unlink(test_path("Rplots.pdf")) # Created when device opened for examples
})

