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
