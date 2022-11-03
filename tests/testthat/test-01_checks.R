test_that("check_species()", {

  s_list <- load_bbs_data()$species

  # No combining possible
  s <- "American Kestrel"
  expect_message(a <- check_species(s, s_list, combine_species_forms = TRUE),
                 "Filtering to species American Kestrel \\(3600\\)")
  expect_equal(a, 3600)
  expect_message(a <- check_species(s, s_list, combine_species_forms = FALSE),
                 "Filtering to species American Kestrel \\(3600\\)")

  # French
  s <- "Hirondelle noire"
  expect_message(a <- check_species(s, s_list, combine_species_forms = TRUE),
                 "Filtering to species Purple Martin \\(6110\\)")
  expect_equal(a, 6110)
  expect_message(a <- check_species(s, s_list, combine_species_forms = FALSE),
                 "Filtering to species Purple Martin \\(6110\\)")

  # Scientific
  s <- "Poecile gambeli"
  expect_message(a <- check_species(s, s_list, combine_species_forms = TRUE),
                 "Filtering to species Mountain Chickadee \\(7380\\)")
  expect_equal(a, 7380)
  expect_message(a <- check_species(s, s_list, combine_species_forms = FALSE),
                 "Filtering to species Mountain Chickadee \\(7380\\)")

  # Combined form
  s <- "Blue grouse (dusky/sooty)"
  expect_message(a <- check_species(s, s_list, combine_species_forms = TRUE),
                 "Filtering to species Blue Grouse \\(Dusky/Sooty\\) \\(2973\\)")
  expect_equal(a, 2973)
  expect_error(check_species(s, s_list, combine_species_forms = FALSE),
               "is a combined form...")

  # Non-Combined form
  s <- "unid. Dusky Grouse / Sooty Grouse"
  expect_message(a <- check_species(s, s_list, combine_species_forms = FALSE),
                 "Filtering to species unid. Dusky Grouse / Sooty Grouse \\(2973\\)")
  expect_equal(a, 2973)
  expect_error(check_species(s, s_list, combine_species_forms = TRUE),
               "is an unidentified,\nnon-combined form...")

  # Scientific works either way (but get different name)
  s <- "Dendragapus obscurus / fuliginosus"
  expect_message(a <- check_species(s, s_list, combine_species_forms = TRUE),
                 "Filtering to species Blue Grouse \\(Dusky/Sooty\\) \\(2973\\)")
  expect_equal(a, 2973)
  expect_message(a <- check_species(s, s_list, combine_species_forms = FALSE),
                 "Filtering to species unid. Dusky Grouse / Sooty Grouse \\(2973\\)")
  expect_equal(a, 2973)

})


test_that("check_sf()", {

  map <- load_map("bbs_cws")

  expect_silent(check_sf(NULL))
  expect_error(check_sf("Hi"), "must be an 'sf' spatial data frame")
  expect_error(check_sf(map[0,]), "Empty spatial data frame")

  expect_silent(check_sf(map))

})

test_that("check_strata()", {

  for(i in c("prov_state", "bcr", "latlong", "bbs_cws", "bbs_usgs", "BBS_USGS")) {
    expect_silent(check_strata(!!i, simple = TRUE)) %>%
      expect_equal(!!tolower(i))
  }

  for(i in c("prov_state", "bcr", "latlong", "bbs_cws", "bbs_usgs", "BBS_USGS")) {
    expect_silent(check_strata(!!i)) %>%
      expect_equal(c(tolower(!!i), "standard"))
  }

  expect_error(check_strata("bb"), "or provide an sf") # Long
  expect_error(check_strata("bb", simple = TRUE), "'prov_state'$") # Short


  # Subset
  expect_error(check_strata("bbs_cws", custom = "hi"), "either empty")
  expect_error(check_strata("bbs_cws", custom = data.frame()), "not a subset")
  expect_silent(check_strata("bbs_cws", custom = bbs_strata[["bbs_cws"]]))

  b <- dplyr::select(bbs_strata[["bbs_cws"]], -strata_name)
  expect_error(check_strata("bbs_cws", custom = b), "not a subset")

  b <- bbs_strata[["bbs_usgs"]]
  expect_error(check_strata("bbs_cws", custom = b), "not a subset")

  #   No change if the exact same
  b <- bbs_strata[["bbs_cws"]]
  expect_message(s <- check_strata("bbs_cws", custom = b, quiet = FALSE), "\\'bbs_cws\\'")
  expect_equal(s, c("bbs_cws", "standard"))

  #   Change if actually a subset
  b <- dplyr::filter(bbs_strata[["bbs_cws"]], country_code == "CA")
  expect_message(s <- check_strata("bbs_cws", custom = b, quiet = FALSE),
                 "\\(subset\\)")
  expect_equal(s, c("bbs_cws", "subset"))

  # Custom
  expect_error(check_strata("custom", custom = "hi"), "provide an sf spatial")
  expect_error(check_strata("custom", custom = data.frame()),
               "provide an sf spatial")
  expect_message(s <- check_strata("my_custom",
                                   custom = load_map("bbs_cws"), quiet = FALSE),
                 "\\(custom\\)")
  expect_equal(s, c("my_custom", "custom"))

  expect_silent(s <- check_strata("My_CustOm", custom = load_map("bbs_cws")))
  expect_equal(s, c("my_custom", "custom"))

})

test_that("check_logical()", {

  expect_silent(check_logical(TRUE, FALSE, FALSE))
  expect_silent(check_logical(TRUE))
  expect_error(check_logical(1), "`1` must be logical")
  expect_error(check_logical(true = TRUE, number = 4),
               "`number` must be logical")
  expect_error(check_logical(true = TRUE, number = 4, character = "hi"),
               "`number`, `character` must be logical")
  expect_error(check_logical(NULL))
  expect_error(check_logical(NULL, TRUE, FALSE))
  expect_silent(check_logical(NULL, allow_null = TRUE))
  expect_silent(check_logical(TRUE, TRUE, FALSE, NULL, allow_null = TRUE))

  test_fun <- function(x, y = FALSE) check_logical(x, y)

  msg <- "`x` must be logical"
  expect_silent(test_fun(x = TRUE))
  expect_error(test_fun(x = "hi"), msg)
  expect_error(test_fun(x = mtcars), msg)
  expect_error(test_fun(x = 4), msg)
  expect_error(test_fun(x = 1), msg)
  expect_error(test_fun(x = NULL), msg)
  expect_error(test_fun(x = NULL, y = FALSE), msg)
  expect_error(test_fun(x = TRUE, y = NULL), "`y` must be logical")
  expect_error(test_fun(x = NULL, y = NULL), "`x`, `y` must be logical")

})

test_that("check_numeric()", {

  expect_silent(check_numeric(1, 5, 9.1))
  expect_silent(check_numeric(1))
  expect_error(check_numeric(TRUE), "`TRUE` must be a number")
  expect_error(check_numeric(true = TRUE, number = 4),
               "`true` must be a number")
  expect_error(check_numeric(true = TRUE, number = 4, character = "hi"),
               "`true`, `character` must be a number")
  expect_error(check_numeric(NULL))
  expect_error(check_numeric(NULL, 1, 15))
  expect_silent(check_numeric(NULL, allow_null = TRUE))
  expect_silent(check_numeric(25, 5.6, 2.345, NULL, allow_null = TRUE))

  test_fun <- function(x, y = 1) check_numeric(x, y)

  msg <- "`x` must be a number"
  expect_silent(test_fun(x = 10))
  expect_error(test_fun(x = "hi"), msg)
  expect_error(test_fun(x = mtcars), msg)
  expect_error(test_fun(x = TRUE), msg)
  expect_error(test_fun(x = NULL), msg)
  expect_error(test_fun(x = NULL, y = 3), msg)
  expect_error(test_fun(x = 3, y = NULL), "`y` must be a number")
  expect_error(test_fun(x = NULL, y = NULL), "`x`, `y` must be a number")

})

