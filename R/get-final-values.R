#' Get final values of JAGS model
#'
#' \code{get_final_values} returns the final values of all parameters of
#'   the model created by \code{run_model} as a list. This function would mostly be used
#'   in conjunction with \code{run_model} to provide initial values.
#'
#' @param model Model object returned by \code{run_model}
#'
#' @return List of final values of monitored parameters.
#'
#' @examples
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#'
#' strat_data <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in a JAGS model.
#' jags_data <- prepare_jags_data(strat_data = strat_data,
#'                                species_to_run = "Pacific Wren",
#'                                model = "firstdiff",
#'                                min_year = 2009,
#'                                max_year = 2018)
#'
#' # Now run a JAGS model. For the sake of speed, we've adjusted
#' #   some arguments so that the JAGS model will not run any
#' #   adaptation steps (n_adapt = 0), no burnin steps (n_burnin = 0),
#' #   only 50 iterations per chain (n_iter = 50), and will not
#' #   thin the chain (n_thin = 1). This will produce several convergence
#' #   warnings, but we can ignore them for the sake of this toy example.
#'
#' jags_mod <- run_model(jags_data = jags_data,
#'                       n_adapt = 0,
#'                       n_burnin = 0,
#'                       n_iter = 10,
#'                       n_thin = 1)
#'
#' # Get the final values
#' final_values <- get_final_values(model = jags_mod)
#'
#' # Then, we can use these final values as input for another model run
#' jags_mod2 <- run_model(jags_data = jags_data,
#'                        n_adapt = 0,
#'                        n_burnin = 0,
#'                        n_iter = 50,
#'                        n_thin = 1,
#'                        inits = final_values)
#'
#' @export
#'
#'

get_final_values <- function(model = NULL)
{
  if (is.null(model))
  {
    return(NULL)
  }else if (model$parallel){
    to_return <- vector("list", length(model$model))
    for (i in 1:length(model$model))
    {
      to_return[[i]] <- eval(parse(text = (paste0("as.list(model$model$cluster",
                                                        i,
                                                        "$state()[[1]])"))))
    }
    return(to_return)
  }else{
    return(as.list(model$model$state()))
  }


}
