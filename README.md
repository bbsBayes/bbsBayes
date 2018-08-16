# bbsBayes <img src="man/figures/logo.png" align="right" />

[![Build Status](https://travis-ci.org/BrandonEdwards/bbsBayes.svg?branch=master)](https://travis-ci.org/BrandonEdwards/bbsBayes)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/bbsBayes)](https://cran.r-project.org/package=bbsBayes)


## Overview
bbsBayes is a package to perform hierarchical Bayesian analysis of North American Breeding Bird Survey (BBS) data. 'bbsBayes' will run a full model analysis for one or more species that you choose, or you can take more control and specify how the data should be stratified, prepared for JAGS, or modelled. 

## Installation

The most recent alpha release is v0.3.0-alpha.
``` r
# To install v.0.3.0-alpha from Github:
install.packages("devtools")
library(devtools)
devtools::install_github("BrandonEdwards/bbsBayes", ref = "v0.3.0-alpha")
```

Alternatively, you could install the less-stable development version.
``` r
# To install the development version from GitHub:
install.packages("devtools")
library(devtools)
devtools::install_github("BrandonEdwards/bbsBayes")
```

## Lifecycle
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

bbsBayes is currently going through multiple internal code
reviews, tests, and API updates. Most likely, the API you use
today will be slightly different than the one you use tomorrow.

The near-future goal is to publish a maturing version on CRAN.
Be sure to check back for updates!
