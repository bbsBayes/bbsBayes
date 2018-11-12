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

## Usage

bbsBayes provides functions for every stage of Breeding Bird Survey data analysis.

### Data Retrieval 
You can download BBS data by running `fetch_bbs_data` and saving it to a variable. You must agree to the terms and conditions of the data usage before downloading.

``` r
bbs_data <- fetch_bbs_data()
```

### Data Preparation
#### Stratification
Stratification plays an important role in trend analysis. Use the `stratify()` function on the `bbs_data` you downloaded, and specify how you would like to stratify your data by. Set `stratify_by` by choosing one of the following:
* bbs_cws -- Political region X Bird Conservation region intersection (CWS method)
* bbs_usgs -- Political region X Bird Conservation region intersection (USGS method)
* bcr -- Bird Conservation Region only
* state -- Political Region only
* latlong -- Degree blocks (1 degree of latitude X 1 degree of longitude)

``` r
strat_data <- stratify(bbs_data, stratify_by = "bcr")
```

#### Jags Data
JAGS models require the data to be sent as a data frame depending on how the model is set up. `prepare_jags_data` subsets the stratified data based on species and wrangles relevent data to use for JAGS models.

``` r
jags_data <- prepare_jags_data(strat_data, 
                               species_to_run = "Spruce Grouse", 
                               model = "slope")
```

### MCMC
Once the data has been prepared for JAGS, the model can be run. The following will run MCMC with default number of iterations.

``` r
mod <- run_model(jags_data = jags_data)
```

Alternatively, you can set how many iterations, burn-in steps, or adapt steps to use
``` r
mod <- run_model(jags_data = jags_data,
                 n_burnin = 1000,
                 n_iter=1000,
                 n_adapt = 500)
```

### Model Analysis
There are a number of tools available to analyse the posterior chain output from the MCMC model. The main metric is annual trend, which can be calculated for each stratum:
``` r
strat_indices <- generate_strata_indices(mod)
strat_trend <- generate_strata_trends(indices = strat_indices)
```

These trends can be mapped
``` r
generate_map(strat_trend, stratify_by = "bcr")
```

Which produces

<img src="man/figures/map_example.png" />

There are numerous other functions available for analysis of the data.


## Lifecycle
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

bbsBayes is currently going through multiple internal code
reviews, tests, and API updates. Most likely, the API you use
today will be slightly different than the one you use tomorrow.

The near-future goal is to publish a maturing version on CRAN.
Be sure to check back for updates!
