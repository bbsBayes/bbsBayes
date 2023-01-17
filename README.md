
> ⚠️This work has moved to [bbsBayes/ bbsBayes2](https://github.com/bbsBayes/bbsBayes2)⚠️
> 
> This fork was originally designed to do an update/overhaul of bbsBayes, but
> it was decided that these modifications were extensive enough to warrant a new
> package.





<!-- badges: start -->




[![R-CMD-check](https://github.com/steffilazerte/bbsBayes/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/steffilazerte/bbsBayes/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/steffilazerte/bbsBayes/branch/master/graph/badge.svg)](https://app.codecov.io/gh/steffilazerte/bbsBayes?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/bbsBayes)](https://cran.r-project.org/package=bbsBayes)
<!-- badges: end -->

# bbsBayes <img src="man/figures/logo.png" align="right"/>

bbsBayes is a package for performing hierarchical Bayesian analysis of
North American Breeding Bird Survey (BBS) data. ‘bbsBayes’ will run a
full model analysis for one or more species that you choose, or you can
take more control and specify how the data should be stratified,
prepared for Stan, or modelled.

Installation instructions are below.

See the [documentation](https://steffilazerte.ca/bbsBayes) for an
overview of how to use bbsBayes.

Additional resources:

- [Introductory bbsBayes
  Workshop](https://github.com/AdamCSmithCWS/bbsBayes_Intro_Workshop)
- [Journal Article with worked
  example](https://doi.org/10.5334/jors.329)

<img src="man/figures/BARS_Continental_Trajectory.png"/>
<img src="man/figures/BARS_trendmap.png"/>

## Installation

Option 1: Stable release from CRAN

``` r
# To install from CRAN:
install.packages("bbsBayes")
```

Option 2: Less-stable development version

``` r
# To install the development version from GitHub:
install.packages("devtools")
library(devtools)
devtools::install_github("BrandonEdwards/bbsBayes")
```
