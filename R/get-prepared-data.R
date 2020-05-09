#' Get the prepared species dataset used for JAGS
#'
#' \code{get_prepared_data} returns a data frame of the data that
#'   was used for JAGS. This is the subsetted data based on the
#'   selected species to model, with zero counts filled in
#'   and any other route/strata filter applied.
#'
#' @param jags_data List of JAGS input data produced by
#'   \code{prepare_jags_data}
#'
#' @return Data frame of 9 variables:
#'   \item{count}{Number of species observed for this route run}
#'   \item{strat}{Numerical factors of the stratum}
#'   \item{obser}{Numerical factor of the observer}
#'   \item{year}{Numerical factor of the year}
#'   \item{firstyr}{1 if this was the observer's first year, 0 otherwise}
#'   \item{strat_name}{Name of the stratum}
#'   \item{route}{Route that this count was taken on}
#'   \item{rYear}{Year this count was conducted}
#'   \item{yearscale}{Scaled year}
#'
#' @export
#'
#' @examples
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#'
#' strat_data <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in a JAGS model. In this
#' #   toy example, we will set the minimum year as 2009 and
#' #   maximum year as 2018, effectively only setting up to
#' #   model 10 years of data. We will use the "first difference
#' #   model.
#' jags_data <- prepare_jags_data(strat_data = strat_data,
#'                                species_to_run = "Pacific Wren",
#'                                model = "firstdiff",
#'                                min_year = 2009,
#'                                max_year = 2018)
#'
#' # Obtain the reassembled data frame for the data sent to JAGS
#' prepped_data <- get_prepared_data(jags_data = jags_data)
#'
#'

get_prepared_data <- function(jags_data = NULL)
{
  to_return <- data.frame(Year = jags_data$r_year,
                          Year_Factored = jags_data$year,
                          Count = jags_data$count,
                          Stratum = jags_data$strat_name,
                          Stratum_Factored = jags_data$strat,
                          Observer_Factored = jags_data$obser,
                          Route = jags_data$route,
                          First_Year = jags_data$firstyr)

  return(to_return)
}
