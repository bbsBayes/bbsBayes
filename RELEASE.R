# Get local BBS data ---------------------------------------------------------
fetch_bbs_data_internal() # Run to ensure BBS data for examples

# Update exported data -------------------------------------------------------
# To run in the background

source("data-raw/data_exported.R")
source("data-raw/data_strata.R")

source("data-raw/data_examples.R") # Creates example data and models (takes time to run)


# Checks ---------------------------------------------------------------------

# Check package
devtools::check()
devtools::check(vignettes = FALSE) # Quicker without vignettes

# Check examples only
# Run all examples, can start = "function_name" to start at a specific spot
# Note: fetch_bbs_data() will still require the "yes"
#devtools::run_examples()
#devtools::run_examples(start = "run_model")

# Check tests only
#devtools::test()

# Build documentation --------------------------------------------------------
devtools::build_readme()

# Check/update URLS
urlchecker::url_check()

# Precompile Vignettes - MUST BUILD/INSTALL PACKAGE FIRST!
devtools::install(quick = TRUE, build = TRUE, upgrade = "never")
unlink("vignettes/articles/figures/", recursive = TRUE)
source("vignettes/_PRECOMPILE.R")

# Preview precompiled vignettes (click on link and hit enter)
pkgdown::build_article("articles/models_first_diff_nonhier")



# Run advanced long-running model and upload --------------------------------
m <- stratify(by = "bbs_cws", species = "Barn Swallow") %>%
  prepare_data(min_max_route_years = 5) %>%
  prepare_model(model = "gamye") %>%
  run_model(output_basename = "cws_basw_gamye")

# Create Tag on GitHub Repo
piggyback::pb_new_release("steffilazerte/bbsBayes", "v3.0.0")

# Save model output

piggyback::pb_upload("cws_basw_gamye",
                     repo = "steffilazerte/bbsBayes",
                     tag = "v3.0.0")


# Local checks of website, etc. ------------------------------------------
# These steps aren't required unless you're troubleshooting or want to
# re-evaluate good practices, etc.

# Compile articles and website
# (only for checking! GitHub Actions will build automatically)
pkgdown::build_site(lazy = TRUE)
pkgdown::build_article("articles/models_first_diff_nonhier")

# Good practice checks -------------------------------------------------------
checks <- goodpractice::all_checks()
checks <- checks[!stringr::str_detect(checks, "covr|cyclocomp|rcmdcheck")]
g <- goodpractice::goodpractice(checks = checks)
goodpractice::results(g)
g

# The `init_def()` function has some lines which are too long, but I think
# that's important given the complexity and the need for descriptions.


# Workflow creation ----------------------------------------------------------
# To create workflows (don't need to run these again)
# usethis::use_coverage()
# usethis::use_github_action_check_standard()
