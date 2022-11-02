# Code Design

This file contains notes about code design and conventions with the aim of
making collaboration and future modifications easier.

## Naming
- Snake case is used wherever possible
- Functions are generally named `verb_noun()` i.e. `run_model()` or `prepare_data()`

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


## Random Notes
