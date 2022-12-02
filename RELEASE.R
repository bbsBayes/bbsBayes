
# Run all examples, can start = "function_name" to start at a specific spot
# Note: fetch_bbs_data() will still require the "yes"
fetch_bbs_data_internal() # Run to ensure BBS data for examples
devtools::run_examples()

devtools::build_readme()




# Compile articles and website
pkgdown::build_site(lazy = TRUE)
pkgdown::build_article("articles/models_first_diff_nonhier")


# To create workflows (don't need to run these again)
# usethis::use_coverage()
# usethis::use_github_action_check_standard()
