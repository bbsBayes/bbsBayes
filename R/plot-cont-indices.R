#' Generate continental plot of index trajectories.
#'
#' Generates the trajectory plot of continental indices.
#'
#' @param indices Dataframe of yearly indices produced by
#'   \code{generate_cont_indices}
#' @param min_year Minimum year to plot
#' @param max_year Maximum year to plot
#'
#' @return ggplot of continental indices
#'
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes
#'
#' @examples
#'
#' \dontrun{
#'
#' # Run a JAGS model analysis on a species
#' stratified_data <- stratify(bbs_data = fetch_bbs_data(), stratify_by = "bcr")
#' prepped_data <- prepare_jags_data(strat_data = stratified_data,
#'                                   species_to_run = "Barn Swallow",
#'                                   model = "slope")
#' mod <- run_model(jags_data = prepped_data)
#'
#' #Generate the continental indices weighted by each strata
#' cont_indices <- generate_cont_indices(jags_mod = mod)
#'
#'
#' # After generating continental indices, plot them
#' plot_cont_indices(indices = cont_indices)

#'
#' # You can specify to only plot a subset of years using min_year and max_year
#' # Plots indices from 1990 onward
#' c_plot <- plot_cont_indices(indices = cont_indices, min_year = 1990)
#' #Plot up indicess up to the year 2000
#' c_plot <- plot_cont_indices(indices = cont_indices, max_year = 2000)
#' #Plot indicess between 1970 and 2010
#' c_plot <- plot_cont_indices(indices = cont_indices, min_year = 1970, max_year = 2010)
#'
#' }
#'
#' @export
#'
plot_cont_indices <- function(indices = NULL,
                            min_year = NULL,
                            max_year = NULL)
{
  if (!is.null(min_year))
  {
    indices <- indices[which(indices$Year >= min_year), ]
  }

  if(!is.null(max_year))
  {
    indices <- indices[which(indices$Year <= max_year), ]
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
