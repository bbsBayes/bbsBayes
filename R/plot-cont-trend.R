#' Generate continental trend plot.
#'
#' Generates the trend plot for continental indices.
#'
#' @param trend Dataframe of yearly indices produced by
#'   \code{generate_cont_trend}
#' @param y_min Minimum year to plot
#' @param y_max Maximum year to plot
#'
#' @return ggplot of continental trend
#'
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes
#'
#' @examples
#'
#' \dontrun{
#' # After generating continental trends, plot them
#' c_plot <- plot_cont_trend(trend = cont_trend)
#' print(c_plot)
#'
#' # You can specify to only plot a subset of years using y_min and y_max
#' # Plots trend from 1990 onward
#' c_plot <- plot_cont_trend(trend = cont_trend, y_min = 1990)
#' #Plot up trends up to the year 2000
#' c_plot <- plot_cont_trend(trend = cont_trend, y_max = 2000)
#' #Plot trends between 1970 and 2010
#' c_plot <- plot_cont_trend(trend = cont_trend, y_min = 1970, y_max = 2010)
#'
#' }
#'
#' @export
#'
plot_cont_trend <- function(trend = NULL,
                            y_min = NULL,
                            y_max = NULL)
{
  if (!is.null(y_min))
  {
    trend <- trend[which(trend$Year >= y_min), ]
  }

  if(!is.null(y_max))
  {
    trend <- trend[which(trend$Year <= y_max), ]
  }

  p <- ggplot() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(title = paste("Annual Trend: Continental"), x = "Year", y = "Index") +
    geom_line(data = trend, aes(x = Year, y = Index)) +
    geom_ribbon(data = trend, aes(x = Year, ymin = Q25, ymax = Q975), alpha = 0.12)

  return(p)
}
