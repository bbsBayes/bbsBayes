# bbsBayes <img src="man/figures/logo.png" align="right" />

[![Build Status](https://travis-ci.org/BrandonEdwards/bbsBayes.svg?branch=master)](https://travis-ci.org/BrandonEdwards/bbsBayes)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/bbsBayes)](https://cran.r-project.org/package=bbsBayes)


## Overview
bbsBayes is a package to perform hierarchical Bayesian analysis of North American Breeding Bird Survey (BBS) data. 'bbsBayes' will run a full model analysis for one or more species that you choose, or you can take more control and specify how the data should be stratified, prepared for JAGS, or modelled. 

## Installation

The most recent alpha release is v0.2.0-alpha.
``` r
# To install v.0.2.0-alpha from Github:
install.packages("devtools")
library(devtools)
devtools::install_github("BrandonEdwards/bbsBayes", ref = "v0.2.0-alpha")
```

Alternatively, you could install the less-stable development version.
``` r
# To install the development version from GitHub:
install.packages("devtools")
library(devtools)
devtools::install_github("BrandonEdwards/bbsBayes")
```
