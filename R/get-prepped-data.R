#' Get the prepped species dataset used for JAGS
#'
#' \code{get_prepped_data} returns a dataframe of the data that
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
#'   \dontrun{
#'
#'     # Prepare the data for a given species
#'     jags_prep <- prepare_jags_data(strat_data = stratified_data,
#'                                    species_to_run = "Barn Swallow",
#'                                    model = "gam")
#'
#'     # Obtain the reassembled data frame for the data sent to JAGS
#'     prepped_data <- get_prepped_data(jags_data = jags_prep)
#'   }
#'

get_prepped_data <- function(jags_data)
{
  return(jags_data$prepped_data)
}
