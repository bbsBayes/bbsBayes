#' Generate continental plot of index trajectories.
#'
#' Generates the trajectory plot of continental indices.
#'
#' @param indices_list List of indices of annual abundance and other results produced by
#'   \code{generate_cont_indices}
#' @param ci_width quantile to define the width of the plotted credible interval. Defaults to 0.95, lower = 0.025 and upper = 0.975
#' @param select logical flag to indicate if the continental data need to be selected out of an indices_list object that includes stratum, national, or other region-types. Default is FALSE
#' @param min_year Minimum year to plot
#' @param max_year Maximum year to plot
#' @param species Species name to be added onto the plot
#' @param title_size Specify font size of plot title. Defaults to 20
#' @param axis_title_size Specify font size of axis titles. Defaults to 18
#' @param axis_text_size Specify font size of axis text. Defaults to 16
#' @param add_observed_means Should the plot include points indicated the observed mean counts. Defaults to FALSE.  Note: scale of observed means and annual indices may not match due to imbalanced sampling among strata
#'
#' @return ggplot of continental indices
#'
#' @importFrom ggplot2 ggplot theme element_blank element_line
#' labs geom_line geom_ribbon aes element_text
#' @importFrom grDevices grey
#'
#'
#' @name bbsBayes-deprecated
#' @seealso \code{\link{plot_cont_indices}}
#' @keywords internal
NULL

#' @rdname bbsBayes-deprecated
#' @section \code{plot_cont_indices}:
#'   For \code{plot_cont_indices()}, use \code{plot_indices()}.
#'
#' @export
#'
#'
plot_cont_indices <- function(indices_list = NULL,
                              select = F,
                              ci_width = 0.95,
                              min_year = NULL,
                              max_year = NULL,
                              species = "",
                              title_size = 20,
                              axis_title_size = 18,
                              axis_text_size = 16,
                              add_observed_means = F)
{

  .Deprecated(new = "plot_indices",
              msg = "plot_cont_indices() is deprecated in favour of plot_indices()")

  Year <- NULL; rm(Year)
  Index <- NULL; rm(Index)
  obs_mean <- NULL; rm(obs_mean)
  lci <- NULL; rm(lci)
  uci <- NULL; rm(uci)

  indices = indices_list$data_summary

  if(select){
  indices = indices[which(indices$Region_Type == "continental"),]
}

  lq = (1-ci_width)/2
  uq = ci_width+lq
  lqc = paste0("Index_q_",lq)
  uqc = paste0("Index_q_",uq)


  indices$lci = indices[,lqc]
  indices$uci = indices[,uqc]


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

  if(add_observed_means){


    p <- ggplot2::ggplot() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     plot.title = ggplot2::element_text(size = title_size),
                     axis.title = ggplot2::element_text(size = axis_title_size),
                     axis.text = ggplot2::element_text(size = axis_text_size)) +
      ggplot2::labs(title = paste(species, " Continental Trajectory and raw mean counts", sep = ""),
                    x = "Year",
                    y = "Annual index of abundance (mean count)") +
      ggplot2::geom_point(data = indices,ggplot2::aes(x = Year,y = obs_mean),colour = grDevices::grey(0.6))+
      ggplot2::geom_line(data = indices, ggplot2::aes(x = Year, y = Index)) +
      ggplot2::geom_ribbon(data = indices, ggplot2::aes(x = Year, ymin = lci, ymax = uci), alpha = 0.12)+
      ggplot2::scale_x_continuous(breaks = yys)+
      ggplot2::scale_y_continuous(limits = c(0,NA))
  }else{
    p <- ggplot2::ggplot() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     plot.title = ggplot2::element_text(size = title_size),
                     axis.title = ggplot2::element_text(size = axis_title_size),
                     axis.text = ggplot2::element_text(size = axis_text_size)) +
      ggplot2::labs(title = paste(species, " Continental Trajectory", sep = ""),
                    x = "Year",
                    y = "Annual index of abundance (mean count)") +
      ggplot2::geom_line(data = indices, ggplot2::aes(x = Year, y = Index)) +
      ggplot2::geom_ribbon(data = indices, ggplot2::aes(x = Year, ymin = lci, ymax = uci), alpha = 0.12)+
      ggplot2::scale_x_continuous(breaks = yys)+
      ggplot2::scale_y_continuous(limits = c(0,NA))
    }
  return(p)
}
