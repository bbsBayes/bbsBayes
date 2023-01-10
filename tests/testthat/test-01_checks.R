test_that("check_bbs_data()", {
  expect_error(check_bbs_data(release = 2022, level = "state"), "force") %>%
    expect_message("Using data directory")
  expect_silent(check_bbs_data(release = 2022, level = "state",
                               force = TRUE, quiet = TRUE))

})

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
  expect_message(
    a <- check_species(s, s_list, combine_species_forms = TRUE),
    "Filtering to species Blue Grouse \\(Dusky/Sooty\\) \\(2973\\)")
  expect_equal(a, 2973)
  expect_error(check_species(s, s_list, combine_species_forms = FALSE),
               "is a combined form...")

  # Non-Combined form
  s <- "unid. Dusky Grouse / Sooty Grouse"
  expect_message(
    a <- check_species(s, s_list, combine_species_forms = FALSE),
    "Filtering to species unid. Dusky Grouse / Sooty Grouse \\(2973\\)")
  expect_equal(a, 2973)
  expect_error(check_species(s, s_list, combine_species_forms = TRUE),
               "is an unidentified,\nnon-combined form...")

  # Scientific works either way (but get different name)
  s <- "Dendragapus obscurus / fuliginosus"
  expect_message(
    a <- check_species(s, s_list, combine_species_forms = TRUE),
    "Filtering to species Blue Grouse \\(Dusky/Sooty\\) \\(2973\\)")
  expect_equal(a, 2973)
  expect_message(
    a <- check_species(s, s_list, combine_species_forms = FALSE),
    "Filtering to species unid. Dusky Grouse / Sooty Grouse \\(2973\\)")
  expect_equal(a, 2973)

  expect_error(check_species("test", s_list), "Invalid species")

})


test_that("check_sf()", {

  map <- load_map("bbs_cws")
  pts <- load_bbs_data(sample = TRUE)$routes %>%
    sf::st_as_sf(coords = c("longitude", "latitude"))

  expect_silent(check_sf(NULL))
  expect_silent(check_sf(map))

  expect_error(check_sf("Hi"), "must be an 'sf' spatial data frame")
  expect_error(check_sf(map[0,]), "Empty spatial data frame")

  expect_silent(check_sf(dplyr::select(map, -"strata_name")))
  expect_error(check_sf(dplyr::select(map, -"strata_name"), col = TRUE),
               "missing column")

  expect_silent(check_sf(pts))
  expect_error(check_sf(pts, check_poly = TRUE), "must be comprised of")
})

test_that("check_strata()", {

  for(i in c("prov_state", "bcr", "latlong", "bbs_cws", "bbs_usgs",
             "BBS_USGS")) {
    expect_silent(check_strata(!!i, simple = TRUE)) %>%
      expect_equal(!!tolower(i))
  }

  for(i in c("prov_state", "bcr", "latlong", "bbs_cws", "bbs_usgs",
             "BBS_USGS")) {
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
  expect_message(s <- check_strata("bbs_cws", custom = b, quiet = FALSE),
                 "\\'bbs_cws\\'")
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

test_that("check_regions()", {

  expect_silent(check_regions("country", "bbs_cws", "standard"))

  bad <- dplyr::rename(bbs_strata[["bbs_cws"]], "strata" = "strata_name")
  expect_error(check_regions("country", "bbs_cws", "standard",
                             regions_index = bad), "must have a `strata_name`")

  expect_error(check_regions("test", "bbs_cws", "standard"),
               "must be any of")

  expect_error(check_regions("country", "bcr", "standard"),
               "cannot be divided into regions with political boundaries")
  expect_error(check_regions("bcr", "prov_state", "standard"),
               "cannot be divided into BCR regions")
  expect_error(check_regions("bcr", "custom", "custom"),
               "can only be divided into 'stratum' and 'continent'")

})

test_that("check_spatial()", {
  p <- stratify(by = "bbs_cws", sample_data = TRUE, quiet = TRUE) %>%
    prepare_data() %>%
    prepare_spatial(load_map("bbs_cws"), quiet = TRUE)

  p2 <- p
  p2 <- p2[-1]

  expect_silent(check_spatial(p))
  expect_error(check_spatial(p2), "include the neighbour nodes")
})

test_that("model/model_file/model_variant checks", {

  expect_error(check_basis(NULL), "No `basis` specified")
  expect_error(check_basis("test"), "Invalid")
  expect_error(check_model(model = NULL), "No `model` specified")
  expect_error(check_model(model = "slope", model_variant = NULL),
               "No `model_variant` specified")


  for(i in unique(bbs_models$variant)) expect_silent(check_model_variant(i))
  for(i in seq_len(nrow(bbs_models))) {
    if(bbs_models$variant[i] == "nonhier") {
      expect_warning(check_model(bbs_models$model[i], bbs_models$variant[i]))
    } else {
      expect_silent(check_model(bbs_models$model[i], bbs_models$variant[i]))
    }
    expect_silent(check_model_file(bbs_models$model[i], bbs_models$variant[i],
                                   model_file = NULL))

  }

  expect_error(check_model_variant("test"))
  expect_error(check_model("slope", "nonhier"))
  expect_error(check_model("slope", "test"))
  expect_error(check_model("test", "spatial"))
  expect_error(check_model_file("slope", "hier", "test"))

  expect_message(copy_model_file("slope", "hier", "."))
  expect_error(copy_model_file("slope", "hier", "."))  # Can't overwrite
  expect_message(copy_model_file("slope", "hier", ".", overwrite = TRUE))

  f <- "slope_hier_bbs_CV_COPY.stan"
  expect_true(file.exists(f))
  expect_silent(check_model_file("slope", "hier", f)) %>%
    expect_equal(f)

  unlink(f)
})

test_that("check_init()", {
  expect_silent(check_init(list(a = 1:5, b = 1:5), chains = 2))
})

test_that("check_cv()", {
  f <- NULL
  expect_error(check_cv(f, k = 10), "Missing K-folds specification")

  f <- "Hi"
  expect_error(check_cv(f, k = 10), "Incorrect K-folds specification")

  f <- rep(1:4, 10)
  expect_error(check_cv(f, k = 10), "Higher `k` than the number of K-fold")
})


test_that("check_dir(), check_file()", {
  dir <- "example_test_dir"

  expect_error(check_dir(dir))
  dir.create(dir)
  expect_silent(check_dir(dir))
  unlink(dir, recursive = TRUE)

  expect_silent(check_file("test_name", "slope", "spatial")) %>%
    expect_equal("test_name")
  expect_silent(check_file(NULL, "slope", "spatial")) %>%
    expect_equal(paste0("BBS_STAN_slope_spatial_", Sys.Date()))
  expect_error(check_file("test_name.rds"), "should not have a file extension")
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

test_that("check_in()", {
  expect_silent(check_in(1, 1:10))
  expect_silent(check_in("bbs_usgs", names(bbs_strata)))
  expect_error(check_in(1, 20:30), "must be one of")
  expect_error(check_in("test", LETTERS), "must be one of")
})
