#' Generate continental trend plot.
#'
#' Generates the trend plot for continental indices.
#'
#' @param n Posterior samples of n
#' @param area_weights Area weights of strata that were used
#' @param y_min Minimum year
#' @param y_max Maximum year
#' @param r_year Actual years that were used
#'
#' @return ggplot2 object of continental trend plot.
#'
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes
#'
#' @keywords internal
#' @export
#'
plot_continental <- function(n = NULL,
                             area_weights = NULL,
                             y_min = NULL,
                             y_max = NULL,
                             r_year = NULL)
{
  n_samples <- dim(n)[1]
  n_strata <- dim(n)[2]
  n_weight <- n

  # Weight each sampled n
  for (i in 1:n_samples)
  {
    for (j in 1:n_strata)
    {
      n_weight[i,j,] <- (n_weight[i,j,] * area_weights$area_sq_km[which(area_weights$num == j)])/ sum(area_weights$area_sq_km)
    }
  }

  n_mean <- apply(n_weight, c(2,3), mean)
  n_25 <- apply(n_weight, c(2,3), quantile, probs = 0.025)
  n_975 <- apply(n_weight, c(2,3), quantile, probs = 0.975)

  data_summary <- data.frame(Year = seq(y_min:y_max),
                             Index = colSums(n_mean),
                             Q25 = colSums(n_25),
                             Q975 = colSums(n_975))
  data_summary$Year <- (data_summary$Year - 1) + min(r_year)

  p <- ggplot() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(title = paste("Annual Trend: Continental"), x = "Year", y = "Index") +
    geom_line(data = data_summary, aes(x = Year, y = Index)) +
    geom_ribbon(data = data_summary, aes(x = Year, ymin = Q25, ymax = Q975), alpha = 0.12)

  return(p)

}
