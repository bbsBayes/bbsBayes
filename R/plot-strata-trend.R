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
