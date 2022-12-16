# Code Design

This file contains notes about code design and conventions with the aim of
making collaboration and future modifications easier.

## Naming
- Snake case is used wherever possible
- Functions are generally named `verb_noun()` i.e. `run_model()` or `prepare_data()`
- R script files are named `foo-bar.R` whereas the main function of the file is
  `foo_bar.R`

## Documentation
- Descriptions for parameter arguments should follow:
  - `@param arg_name Type. Description`
  - e.g., `@param min_year Numeric. Minimum year to plot`
  - e.g., `@param add_map sf spatial object. Spatial data to add to map output`
  - e.g., `@param quantiles Numeric vector. Quantiles to be sampled ...`

- Document repetitive arguments (ones found in more than one function) in the
  `R/aa_common_docs.R` file (named to sort to the top of the folder), and use
  `@inheritParams common_docs` in the function documentation. This way 
  duplicate documentation stays consistent. 

- Use `@noRd` to document internal functions (documentation for developers that
  isn't compiled into the docs)
  
- Some articles are pre-compiled to avoid super long CI building or other 
  fragile issues
  - Setup: https://ropensci.org/blog/2019/12/08/precompute-vignettes/
    - Precompiled vignettes have ext `.Rmd.orig`, then are compiled to `.Rmd`
      (all R code already run, so actually Vignette to HTML is super fast)
  - In RELEASE.R there is a step for re-pre-compiling vignettes

## Storing data
- `bbs_dir()` figures out the folder in which to store data and creates it if 
  needed. Use this function wherever that location is required (e.g., BBS data 
  and model executables)
- CRAN doesn't like rappdirs any more
- As of R 4.0.0 CRAN wants packages to use `tools::R_user_dir()`, but this isn't
  available to older versions of R.
- Here we use the backports package to get access to this function
  - We import backports package
  - We specify which function to import (see `.onLoad()` in
  `R/bbsBayes-package.R`
  - We use `R_user_dir()` directly (no `pkg::`, see `bbs_dir()` in
  `R/fetch-bbs-data.R`

## Checks
- Check functions are named as `check_XXX()` and stored in the R/checks.R file
- Generally check functions should be created for any check that is
  a) really big (e.g., `check_species()` or `check_strata()`), or,
  b) used more than once (e.g., `check_sf()`, `check_logical()`). 
  It's also justifiable to create check functions for ease of reading the code
    (e.g., the list of checks at the start of `run_model()`).
- `deparse(substitute(name))` is a way of getting the original argument name 
  submitted to this function from the parent function. That way `check_XXX()` 
  functions can reference the correct argument name in the error messages.


## Tidyeval and NSE (non-standard evaluation)
- In general, see https://ggplot2.tidyverse.org/reference/tidyeval.html, and
  https://ggplot2.tidyverse.org/reference/tidyeval.html
- To avoid CRAN checks, use `.data[["col_name"]]` or .data$col_name to reference
  column names on the right-hand side of = inside tidyverse functions. Use
  `.env$var` to reference environmental variables. e.g.,
  `dplyr::mutate(mtcars, cyl = .data$hp * .data$am)` or
  `dplyr::mutate(mtcars, mpg = .data$mpg / .env$ratio)`
- This also allows more complex programmatic expressions, e.g., in 
  `generate_indices()` we iterate over regions in a for loop.
    - `dplyr::group_by(.data[[rr]])` (note the lack of quotes, as we're calling
      the *value* or `rr`)
    - `dplyr::mutate("{rr}" := as.character(.data[[rr]])` (more complex see 
      above references)

## sf related
- Use `sf::st_agr(sf) <- "constant"` to tell sf that the variables are spatially 
  constant (prevents the warning "attribute variables are assumed to be
  spatially constant throughout all geometries").

## Example data
- If possible, all examples and required data are created by `data-raw/data_XXX.R`
- Currently, all data are exported, so usable by the user as well as functions.
- For (stupid) reasons, exported data (e.g., `species_forms`) need to be
referenced via `bbsBayes::data` (e.g., `bbsBayes::species_forms`) *inside*
bbsBayes functions (See here for more details/other options:
https://coolbutuseless.github.io/2018/12/10/r-packages-internal-and-external-data/)

## Testing and examples
- When in doubt `skip_on_cran()` for tests (never run fragile tests that *might* fail)
- Use @examplesIf interactive() to skip examples that can be run but shouldn't
  be tested (e.g., `fetch_bbs_data()`)
- use `\dontest` for examples that should *never* be run (try not to have too many)
- use `\dontrun` for examples that won't fail, but will take a while to run.
- use first_diff model output by `test-XX_run_model.R` to ensure an up-to-date model
  example for testing `test-XX_generate_indices.R`, `test-XX_generate_trends.R`
  and `test-XX_plots.R`
- use `pacific_wren_slope_model` example model for tests needing to check for
  alternate n. Don't create it in `test-XX_run_model.R` because it takes too long.

## Continuous Integration (CI)
- CI is setup with GitHub actions. The workflows (all in `.github/workflows/`
  were created using:
    - Package Checks - `usethis::use_github_action_check_standard()` (file is
    `R-CMD-check.yaml`)
    - Code Coverage - `usethis::use_coverage()` (file is `test-coverage.yaml`)
    - Documentation website - `usethis::use_pkgdown_github_pages()` (file is
    `pkgdown.yaml`)
- Note that Package Checks don't build vignettes because a) this the download in
  the `stratification.Rmd` vignette fails on Windows, and b) these are already
  built (and therefore tested) in the pkgdown workflow.

## Random Notes
- Use "grey60" rather than `grDevices::grey(0.6)` to avoid another dependency

## Deprecating, Defunct-ing, and other big changes
- See `R/bbsBayes-defunct.R` for listing all defunct packages in a document page
- See `R/bbsBayes-deprectated.R` for listing all deprecated packages in a document page
- `dep_stop()` and `dep_warn()` are created in `bbsBayes-defunct.R` and can be used to 
  stop or warn on use of functions or arguments, depending. See `?plot_map`
  vs `?generate_map`
- Because v3.0.0 of this package entails such a massive overhaul, I suggest 
  using a start up message (in `R/bbsBayes-package.R`) to share this with users
- To deprecate a function
  - Add `dep_warn()` to the start of the function call and either continue, or
    switch to new function (e.g., `R/bbsBayes-deprectated.R`)
  - Add to documentation for `bbsBayes-deprecated`
- To make a function defunct
  - Add `dep_stop()` to the start of the function call and remove the rest of
    the code (for clarity) (e.g., `R/bbsBayes-defunct.R`)
  - Add to documentation for `bbsBayes-defunct`
- To deprecate/defunct an argument
  - Move the argument in the `function()` call to the end of the list, without
    a default
  - Move the documentation for that argument to the end of the list and mark
   "Deprecated. Use X instead", or just "Deprecated." (or Defunct)
  - If deprecated with a replacement, add `if(!missing(arg)) {dep_warn(...)}` to
    the start of the function and use arg_new <- arg_old inside the condition
    (e.g., `plot_indices()`)
  - If defunct or without a replacement, add `if(!missing(arg)) dep_warn(...)`
    or `if(!missing(arg)) dep_stop(...)`
  

## Useful tips
- Use Ctrl-Shift-L to load all package functions via `devtools::load_all()`
  see https://r-pkgs.org/workflow101.html#sec-load-all
- Use Ctrl-Shift / to automatically text-wrap 
- Use Ctrl Click on any function to jump to that function's definition
