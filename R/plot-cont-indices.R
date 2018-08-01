#' Generate continental plot of index trajectories.
#'
#' Generates the trajectory plot of continental indices.
#'
#' @param indices Dataframe of yearly indices produced by
#'   \code{generate_cont_indices}
#' @param y_min Minimum year to plot
#' @param y_max Maximum year to plot
#'
#' @return ggplot of continental indices
#'
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes
#'
#' @examples
#'
#' \dontrun{
#' # After generating continental indicess, plot them
#' c_plot <- plot_cont_indices(indices = cont_index)
#' print(c_plot)
#'
#' # You can specify to only plot a subset of years using y_min and y_max
#' # Plots indices from 1990 onward
#' c_plot <- plot_cont_indices(indices = cont_indices, y_min = 1990)
#' #Plot up indicess up to the year 2000
#' c_plot <- plot_cont_indices(indices = cont_indices, y_max = 2000)
#' #Plot indicess between 1970 and 2010
#' c_plot <- plot_cont_indices(indices = cont_indices, y_min = 1970, y_max = 2010)
#'
#' }
#'
#' @export
#'
plot_cont_indices <- function(indices = NULL,
                            y_min = NULL,
                            y_max = NULL)
{
  if (!is.null(y_min))
  {
    indices <- indices[which(indices$Year >= y_min), ]
  }

  if(!is.null(y_max))
  {
    indices <- indices[which(indices$Year <= y_max), ]
  }

  p <- ggplot() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(title = paste("Annual indices: Continental"), x = "Year", y = "Index") +
    geom_line(data = indices, aes(x = Year, y = Index)) +
    geom_ribbon(data = indices, aes(x = Year, ymin = Q25, ymax = Q975), alpha = 0.12)

  return(p)
}
