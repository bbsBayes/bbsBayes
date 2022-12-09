# Get local BBS data ---------------------------------------------------------
fetch_bbs_data_internal() # Run to ensure BBS data for examples

# Update exported data -------------------------------------------------------
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
devtools::run_examples()
devtools::run_examples(start = "run_model")

# Check tests only
devtools::test()

# Build documentation --------------------------------------------------------
devtools::build_readme()

# Compile articles and website
# (only for checking! GitHub Actions will build automatically
pkgdown::build_site(lazy = TRUE)
pkgdown::build_article("articles/models_first_diff_nonhier")

# Good practice --------------------------------------------------------------
goodpractice::goodpractice()




# Workflow creation ----------------------------------------------------------
# To create workflows (don't need to run these again)
# usethis::use_coverage()
# usethis::use_github_action_check_standard()
