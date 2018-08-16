#' Generate plots of index trajectories by stratum
#'
#' Generates the indices plot for each stratum modelled.
#'
#' @param indices Dataframe of yearly indices produced by
#'   \code{generate_strata_indices}
#' @param min_year Minimum year to plot
#' @param max_year Maximum year to plot
#' @param species Species name to be added onto the plot
#'
#' @return List of ggplot objects, each entry being a plot
#'   of a stratum indices
#'
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes
#' @importFrom stringr str_replace_all
#'
#' @examples
#'
#' \dontrun{
#' # Run a JAGS model analysis on a species
#' stratified_data <- stratify(bbs_data = fetch_bbs_data(), stratify_by = "bcr")
#' prepped_data <- prepare_jags_data(strat_data = stratified_data,
#'                                   species_to_run = "Barn Swallow",
#'                                   model = "slope")
#' mod <- run_model(jags_data = prepped_data)
#'
#' #Generate the indices for each strata
#' strata_indices <- generate_strata_indices(jags_mod = mod)
#'
#'
#' # After generating strata indices, plot them
#' s_plot <- plot_strata_indices(indices = strata_indices,
#'                               species = "Barn Swallow")
#'
#' # s_plot is just a list of ggplot objects, so you can access by index
#' s_plot[[1]]
#'
#' # Or access by strata name, noting the underscores in place of special characters
#' s_plot[["US_FL_31"]]
#'
#' # You can specify to only plot a subset of years using min_year and max_year
#' # Plots indices from 1990 onward
#' s_plot <- plot_strata_indices(indices = strata_indices,
#'                               min_year = 1990,
#'                               species = "Barn Swallow")
#' #Plot up indices up to the year 2000
#' s_plot <- plot_strata_indices(indices = strata_indices,
#'                               max_year = 2000,
#'                               species = "Barn Swallow")
#' #Plot indices between 1970 and 2010
#' s_plot <- plot_strata_indices(indices = strata_indices,
#'                               min_year = 1970,
#'                               max_year = 2010,
#'                               species = "Barn Swallow")
#' }
#' @export
#'
plot_strata_indices <- function(indices = NULL,
                              min_year = NULL,
                              max_year = NULL,
                              species = "")
{
  Year <- NULL
  rm(Year)
  Index <- NULL
  rm(Index)
  Q25 <- NULL
  rm(Q25)
  Q975 <- NULL
  rm(Q975)
  Stratum <- NULL
  rm(Stratum)

  plot_list <- list()

  if (!is.null(min_year))
  {
    indices <- indices[which(indices$Year >= min_year), ]
  }

  if(!is.null(max_year))
  {
    indices <- indices[which(indices$Year <= max_year), ]
  }

  plot_index <- 1
  for (i in unique(indices$Stratum))
  {
    to_plot <- indices[which(indices$Stratum == i), ]

    p <- ggplot2::ggplot() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "black")) +
      ggplot2::labs(title = paste(species, " Annual indices for Stratum ", i, sep = ""),
           x = "Year",
           y = "Index") +
      ggplot2::geom_line(data = to_plot, ggplot2::aes(x = Year, y = Index)) +
      ggplot2::geom_ribbon(data = to_plot, ggplot2::aes(x = Year, ymin = Q25, ymax = Q975), alpha = 0.12)

    plot_list[[stringr::str_replace_all(paste(i),
                               "[[:punct:]\\s]+",
                               "_")]] <- p
    plot_index <- plot_index + 1
  }

  return(plot_list)
}
