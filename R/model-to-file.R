#' Save model to text file
#'
#' \code{model_to_file} allows you to save any of the preloaded hierarchical
#'   Bayesian models to a text file.
#'
#' @param filename File name to create on disk.
#' @param model Model to be saved
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Save the Slope model to a file called "slope.txt"
#' model_to_file(model = "slope", filename = "slope.txt")
#' }
#'

model_to_file <- function(model = NULL,
                          filename = NULL)
{
  model_file <- system.file("models",models[[model]],package="bbsBayes")
  write(readChar(model_file, file.info(model_file)$size), file = filename)
}
