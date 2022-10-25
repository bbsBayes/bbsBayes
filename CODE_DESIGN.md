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

- Document repetitive arugments (ones found in more than one function) in the
  `R/aa_common_docs.R` file (named to sort to the top of the folder), and use
  `@inheritParams common_docs` in the function documentation. This way 
  duplicate documentation stays consistent. 
