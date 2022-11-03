
# Run all examples, can start = "function_name" to start at a specific spot
# Note: fetch_bbs_data() will still require the "yes"
devtools::run_examples()


devtools::build_readme()


# Compile articles and website
# BUILD PACKAGE FIRST!!! Ctrl-Shift-B
pkgdown::build_site(lazy = TRUE)
pkgdown::build_article("articles/models_first_diff_nonhier")
