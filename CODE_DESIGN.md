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
  column names on the right-hand side of = inside tidyverse functions. Use `.env$var` to reference
  environmental variables. e.g.,
  `dplyr::mutate(mtcars, cyl = .data$hp * .data$am)` or
  `dplyr::mutate(mtcars, mpg = .data$mpg / .env$ratio)`
- This also allows more complex programmatic expressions, e.g., in 
  `generate_indices()` we iterate over regions in a for loop.
    - `dplyr::group_by(.data[[rr]])` (note the lack of quotes, as we're calling
      the *value* or `rr`)
    - `dplyr::mutate("{rr}" := as.character(.data[[rr]])` (more complex see above references)

## sf related
- Use `sf::st_agr(sf) <- "constant"` to tell sf that the variables are spatially 
  constant (prevents the warning "attribute variables are assumed to be
  spatially constant throughout all geometries").


## Testing and examples
- When in doubt `skip_on_cran()` for tests (never run fragile tests that *might* fail)
- Use @examplesIf interactive() to skip examples that can be run but shouldn't
  be tested (e.g., `fetch_bbs_data()`)
- use `\dontest` for examples that should *never* be run (try not to have too many)
- use `\dontrun` for examples that won't fail, but will take a while to run.

## Random Notes
- Use "grey60" rather than `grDevices::grey(0.6)` to avoid another dependency
- `species_forms` is a *exported* data frame, which means that for (stupid)
  reasons, bbsBayes functions (like `fetch_bbs_data()`) need to reference it via
  `bbsBayes::species_forms`
  (See here for more details/other options:
  https://coolbutuseless.github.io/2018/12/10/r-packages-internal-and-external-data/)

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
