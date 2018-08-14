#' Save model to text file
#'
#' \code{model_to_file} allows you to save any of the preloaded hierarchical
#'   Bayesian models to a text file.
#'
#' @param model Model to be saved. Options are "slope", "firstdiff", "gam", "gamye"
#' @param filename File name to create on disk.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Save the Slope model to a file called "slope.txt"
#' model_to_file(model = "slope", filename = "slope.txt")
#'
#' # Save the First Difference model to a file called "fd.txt"
#' model_to_file(model = "firstdiff", filename = "fd.txt")
#'
#' # Save the GAM model to a file called "gam.txt"
#' model_to_file(model = "gam", filename = "gam.txt")
#'
#' # Save the GAM year effects model to a file called "gamye.txt"
#' model_to_file(model = "gamye", filename = "gamye.txt")
#' }
#'

model_to_file <- function(model = NULL,
                          filename = NULL)
{
  model_file <- system.file("models",models[[model]],package="bbsBayes")
  write(readChar(model_file, file.info(model_file)$size), file = filename)
}
