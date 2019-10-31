#' Generate plots of index trajectories by stratum
#'
#' Generates the indices plot for each stratum modelled.
#'
#' @param indices_list List of indices of annual abundance and other results produced by
#'   \code{generate_strata_indices}
#' @param ci_width quantile to define the width of the plotted credible interval. Defaults to 0.95, lower = 0.025 and upper = 0.975
#' @param min_year Minimum year to plot
#' @param max_year Maximum year to plot
#' @param species Species name to be added onto the plot
#' @param title_size Specify font size of plot title. Defaults to 20
#' @param axis_title_size Specify font size of axis titles. Defaults to 18
#' @param axis_text_size Specify font size of axis text. Defaults to 16
#' @param add_observed_means Should the plot include points indicated the observed mean counts. Defaults to FALSE. Note: scale of observed means and annual indices may not match due to imbalanced sampling among routes
#'
#' @return List of ggplot objects, each entry being a plot
#'   of a stratum indices
#'
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes element_text
#' @importFrom stringr str_replace_all
#' @importFrom grDevices grey
#'
#'
#' @examples
#'
#' \dontrun{
#' # Run a JAGS model analysis on a species
#' stratified_data <- stratify(bbs_data = fetch_bbs_data(), stratify_by = "bcr")
#' prepped_data <- prepare_jags_data(strat_data = stratified_data,
#'                                   species_to_run = "Wood Thrush",
#'                                   model = "slope")
#' mod <- run_model(jags_data = prepped_data)
#'
#' #Generate the indices for each strata
#' strata_indices <- generate_strata_indices(jags_mod = mod)
#'
#'
#' # After generating strata indices, plot them
#' s_plot <- plot_strata_indices(indices = strata_indices,
#'                               species = "Wood Thrush")
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
#'                               species = "Wood Thrush")
#' #Plot up indices up to the year 2000
#' s_plot <- plot_strata_indices(indices = strata_indices,
#'                               max_year = 2000,
#'                               species = "Wood Thrush")
#' #Plot indices between 1970 and 2010
#' s_plot <- plot_strata_indices(indices = strata_indices,
#'                               min_year = 1970,
#'                               max_year = 2010,
#'                               species = "Wood Thrush")
#' }
#' @export
#'
plot_strata_indices <- function(indices_list = NULL,
                                ci_width = 0.95,
                              min_year = NULL,
                              max_year = NULL,
                              species = "",
                              title_size = 20,
                              axis_title_size = 18,
                              axis_text_size = 16,
                              add_observed_means = F)
{
  Year <- NULL
  rm(Year)
  Index <- NULL
  rm(Index)

  Stratum <- NULL
  rm(Stratum)

  indices = indices_list$data_summary

  lq = (1-ci_width)/2
  uq = ci_width+lq
  lqc = paste0("Index_q_",lq)
  uqc = paste0("Index_q_",uq)


  indices$lci = indices[,lqc]
  indices$uci = indices[,uqc]



  plot_list <- list()

  if (!is.null(min_year))
  {
    indices <- indices[which(indices$Year >= min_year), ]
  }

  if(!is.null(max_year))
  {
    indices <- indices[which(indices$Year <= max_year), ]
  }

  mny = min(indices$Year)
  mxy = max(indices$Year)
  yys = pretty(seq(mny, mxy))
  yys = c(yys[-length(yys)],mxy)


  plot_index <- 1
  for (i in unique(indices$Region))
  {
    to_plot <- indices[which(indices$Region == i), ]

    if(add_observed_means){


    p <- ggplot2::ggplot() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "black"),
            plot.title = ggplot2::element_text(size = title_size),
            axis.title = ggplot2::element_text(size = axis_title_size),
            axis.text = ggplot2::element_text(size = axis_text_size)) +
      ggplot2::labs(title = paste(species, " Annual indices for Stratum ", i, sep = ""),
           x = "Year",
           y = "Index",
           subtitle = paste("Note: scale of observed means and annual indices may not match")) +
      ggplot2::geom_point(data = to_plot,ggplot2::aes(x = Year,y = obs_mean),colour = grDevices::grey(0.6))+
      ggplot2::geom_line(data = to_plot, ggplot2::aes(x = Year, y = Index)) +
      ggplot2::geom_ribbon(data = to_plot, ggplot2::aes(x = Year, ymin = lci, ymax = uci), alpha = 0.12)+
      ggplot2::scale_x_continuous(breaks = yys)+
      ggplot2::scale_y_continuous(limits = c(0,NA))

    }else{

      p <- ggplot2::ggplot() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       axis.line = element_line(colour = "black"),
                       plot.title = ggplot2::element_text(size = title_size),
                       axis.title = ggplot2::element_text(size = axis_title_size),
                       axis.text = ggplot2::element_text(size = axis_text_size)) +
        ggplot2::labs(title = paste(species, " Annual indices for Stratum ", i, sep = ""),
                      x = "Year",
                      y = "Index") +
        ggplot2::geom_line(data = to_plot, ggplot2::aes(x = Year, y = Index)) +
        ggplot2::geom_ribbon(data = to_plot, ggplot2::aes(x = Year, ymin = lci, ymax = uci), alpha = 0.12)+
        ggplot2::scale_x_continuous(breaks = yys)+
        ggplot2::scale_y_continuous(limits = c(0,NA))


    }
    plot_list[[stringr::str_replace_all(paste(i),
                               "[[:punct:]\\s]+",
                               "_")]] <- p
    plot_index <- plot_index + 1
  }

  return(plot_list)
}
