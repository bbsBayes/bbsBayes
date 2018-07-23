#' Generate continental trend plot.
#'
#' Generates the trend plot for continental indices.
#'
#' @param n Posterior samples of n
#' @param strata_to_plot Which strata to plot
#' @param area_weights Subsetted area weights, used more for the names of strata
#' @param y_min Minimum year
#' @param y_max Maximum year
#'
#' @return List of ggplots for each strata
#'
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes
#'
#' @keywords internal
#' @export
#'
plot_strata <- function(n = NULL,
                        strata_to_plot = NULL,
                        area_weights = NULL,
                        y_min = NULL,
                        y_max = NULL)
{
  if (length(strata_to_plot) > 0)
  {
    n_samples <- dim(n)[1]
    n_strata <- dim(n)[2]
    strata_indices <- NULL
    plot_list <- list()

    # Check which strata requested to be plotted, and get indices for names
    if ("all" %in% tolower(strata_to_plot))
    {
      strata_indices <- seq(1:n_strata)
    }else
    {
      for (i in strata_to_plot)
      {
        strata_indices <- c(strata_indices, which(area_weights$region == i))
      }
    }

    n_mean <- apply(n, c(2,3), mean)
    n_25 <- apply(n, c(2,3), quantile, probs = 0.025)
    n_975 <- apply(n, c(2,3), quantile, probs = 0.975)

    plot_index <- 1
    for (i in strata_indices)
    {
      data_summary <- data.frame(Year = seq(y_min:y_max),
                                 Index = as.numeric(as.vector(n_mean[i,])),
                                 Q25 = as.numeric(as.vector(n_25[i,])),
                                 Q975 = as.numeric(as.vector(n_975[i,])))
      p <- ggplot() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black")) +
        labs(title = paste("Annual Trend for Strata", area_weights$region[i]), x = "Year", y = "Index") +
        geom_line(data = data_summary, aes(x = Year, y = Index)) +
        geom_ribbon(data = data_summary, aes(x = Year, ymin = Q25, ymax = Q975), alpha = 0.12)
      plot_list[[str_replace_all(paste(area_weights$region[i]),
                                 "[[:punct:]\\s]+",
                                 "_")]] <- p
      plot_index <- plot_index + 1
    }

    return(plot_list)

  }else
  {
    return(NULL)
  }
}
