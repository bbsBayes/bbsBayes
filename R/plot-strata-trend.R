#' Generate trend plot by stratum.
#'
#' Generates the trend plot for each stratum modelled.
#'
#' @param trend Dataframe of yearly indices produced by
#'   \code{generate_strata_trend}
#' @param y_min Minimum year to plot
#' @param y_max Maximum year to plot
#'
#' @return List of ggplot objects, each entry being a plot
#'   of a stratum trend
#'
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes
#'
#' @examples
#'
#' \dontrun{
#' # After generating strata trends, plot them
#' s_plot <- plot_strata_trend(trend = strata_trend)
#' # s_plot is just a list of ggplot objects, so you can access by index
#' print(s_plot[[1]])
#' # Or access by strata name, noting the underscores in place of special characters
#' print(s_plot[["US_FL_31"]])
#'
#' # You can specify to only plot a subset of years using y_min and y_max
#' # Plots trend from 1990 onward
#' s_plot <- plot_strata_trend(trend = strata_trend, y_min = 1990)
#' #Plot up trends up to the year 2000
#' s_plot <- plot_strata_trend(trend = strata_trend, y_max = 2000)
#' #Plot trends between 1970 and 2010
#' s_plot <- plot_strata_trend(trend = strata_trend, y_min = 1970, y_max = 2010)
#' }
#' @export
#'
plot_strata_trend <- function(trend = NULL,
                              y_min = NULL,
                              y_max = NULL)
{
  plot_list <- list()

  if (!is.null(y_min))
  {
    trend <- trend[which(trend$Year >= y_min), ]
  }

  if(!is.null(y_max))
  {
    trend <- trend[which(trend$Year <= y_max), ]
  }

  plot_index <- 1
  for (i in unique(trend$Stratum))
  {
    to_plot <- trend[which(trend$Stratum == i), ]

    p <- ggplot() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      labs(title = paste("Annual Trend for Strata", i), x = "Year", y = "Index") +
      geom_line(data = to_plot, aes(x = Year, y = Index)) +
      geom_ribbon(data = to_plot, aes(x = Year, ymin = Q25, ymax = Q975), alpha = 0.12)
    plot_list[[str_replace_all(paste(i),
                               "[[:punct:]\\s]+",
                               "_")]] <- p
    plot_index <- plot_index + 1
  }

  return(plot_list)
}
