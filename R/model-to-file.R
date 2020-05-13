#' Save model to text file
#'
#' \code{model_to_file} allows you to save any of the preloaded hierarchical
#'   Bayesian models to a text file.
#'
#' @param model Model to be saved. Options are "slope", "firstdiff", "gam", "gamye"
#' @param filename File name to create on disk.
#' @param heavy_tailed Logical indicating whether the extra-Poisson error distribution should be modeled as a t-distribution, with heavier tails than the standard normal distribution. Default is currently FALSE, but recent results suggest users should strongly consider setting this to TRUE, even though it requires much longer convergence times
#'
#' @return None
#'
#' @export
#'
#' @examples
#'
#' # Save the Slope model to a file called "slope.txt" in temp directory
#' model_to_file(model = "slope",
#'               filename = file.path(tempdir(), "slope.txt"))
#'
#' # Save the First Difference model to a file called "fd.txt" in temp directory
#' model_to_file(model = "firstdiff",
#'               filename = file.path(tempdir(), "fd.txt"))
#'
#' # Save the GAM model to a file called "gam.txt" in temp directory
#' model_to_file(model = "gam",
#'               filename = file.path(tempdir(), "gam.txt"))
#'
#' # Save the GAM year effects model to a file called "gamye.txt" in temp directory
#' model_to_file(model = "gamye",
#'               filename = file.path(tempdir(), "gamye.txt"))
#'

model_to_file <- function(model = NULL,
                          filename = NULL,
                          heavy_tailed = FALSE)
{
  if(heavy_tailed){
    model <- paste0(model,"_heavy")
  }
  model_file <- system.file("models",models[[model]],package="bbsBayes")
  write(readChar(model_file, file.info(model_file)$size), file = filename)
}
