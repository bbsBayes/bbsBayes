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
#' @param line_width Specify the size of the trajectory line. Defaults to 1
#' @param add_observed_means Should the plot include points indicated the observed mean counts. Defaults to FALSE. Note: scale of observed means and annual indices may not match due to imbalanced sampling among routes
#' @param add_number_routes Should the plot be superimposed over a dotplot showing the number of BBS routes included in each year. This is useful as a visual check on the relative data-density through time because in most cases the number of observations increases over time
#'
#'
#' @return List of ggplot objects, each entry being a plot
#'   of a stratum indices
#'
#'
#'
#' @examples
#'
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#'
#' strat_data <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for use in a JAGS model.
#' jags_data <- prepare_jags_data(strat_data = strat_data,
#'                                species_to_run = "Pacific Wren",
#'                                model = "firstdiff",
#'                                min_year = 2009,
#'                                max_year = 2018)
#'
#' # Now run a JAGS model.
#' jags_mod <- run_model(jags_data = jags_data,
#'                       n_adapt = 0,
#'                       n_burnin = 0,
#'                       n_iter = 10,
#'                       n_thin = 1)
#'
#' # Generate only national, continental, and stratum indices
#' indices <- generate_indices(jags_mod = jags_mod,
#'                             jags_data = jags_data,
#'                             regions = c("national",
#'                                         "continental",
#'                                         "stratum"))
#'
#' # Now, plot_indices() will generate a list of plots for all regions
#' plot_list <- plot_indices(indices_list = indices,
#'                           species = "Pacific Wren")
#'
#' #Suppose we wanted to access the continental plot. We could do so with
#' cont_plot <- plot_list$continental
#'
#' # You can specify to only plot a subset of years using min_year and max_year
#' # Plots indices from 2015 onward
#' plot_list_2015_on <- plot_indices(indices_list = indices,
#'                                   min_year = 2015,
#'                                   species = "Pacific Wren")
#'
#' #Plot up indices up to the year 2017
#' plot_list_max_2017 <- plot_indices(indices_list = indices,
#'                                    max_year = 2017,
#'                                    species = "Pacific Wren")
#'
#' #Plot indices between 2011 and 2016
#' plot_list_2011_2015 <- plot_indices(indices_list = indices,
#'                                     min_year = 2011,
#'                                     max_year = 2016,
#'                                     species = "Pacific Wren")
#' @noRd

plot_indices_orig <- function(indices_list = NULL,
                         ci_width = 0.95,
                         min_year = NULL,
                         max_year = NULL,
                         species = "",
                         title_size = 20,
                         axis_title_size = 18,
                         axis_text_size = 16,
                         line_width = 1,
                         add_observed_means = FALSE,
                         add_number_routes = FALSE) {


  indices <- indices_list$data_summary

  lq = (1-ci_width)/2
  uq = ci_width+lq
  lqc = paste0("Index_q_",lq)
  uqc = paste0("Index_q_",uq)


  indices$lci = indices[[lqc]]
  indices$uci = indices[[uqc]]

  cl = "#39568c"

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
  for (i in unique(indices$Region_alt))
  {
    to_plot <- indices[which(indices$Region_alt == i), ]

    if(add_number_routes){

      if(max(to_plot$nrts) > 200){
        ncby_y = ceiling(to_plot$nrts/50)
        annot = c("each dot ~ 50 routes")
      }else{
        if(max(to_plot$nrts) > 100){
          ncby_y = ceiling(to_plot$nrts/10)
          annot = c("each dot ~ 10 routes")
        }else{
          ncby_y = to_plot$nrts
          annot = c("each dot = 1 route")
        }
      }

      names(ncby_y) <- to_plot$Year
      dattc = data.frame(Year = rep(as.integer(names(ncby_y)),times = ncby_y))
    }


    if(add_observed_means){

      annotobs = to_plot[4,c("obs_mean","Year")]

      p <- ggplot2::ggplot() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       axis.line = element_line(colour = "black"),
                       plot.title = ggplot2::element_text(size = title_size),
                       axis.title = ggplot2::element_text(size = axis_title_size),
                       axis.text = ggplot2::element_text(size = axis_text_size)) +
        ggplot2::labs(title = paste(species, " ", i, sep = ""),
                      x = "Year",
                      y = "Annual index of abundance (mean count)") +
        ggplot2::geom_point(data = to_plot,ggplot2::aes(x = Year,y = obs_mean),colour = grDevices::grey(0.6))+
        ggplot2::geom_line(data = to_plot, ggplot2::aes(x = Year, y = Index), colour = cl, size = line_width) +
        ggplot2::geom_ribbon(data = to_plot, ggplot2::aes(x = Year, ymin = lci, ymax = uci),fill = cl,alpha = 0.3)+
        ggplot2::scale_x_continuous(breaks = yys)+
        ggplot2::scale_y_continuous(limits = c(0,NA))+
        ggplot2::annotate(geom = "text",x = annotobs$Year,y = annotobs$obs_mean,label = "",colour = grDevices::grey(0.6))

      if(add_number_routes){

        p <- p + ggplot2::geom_dotplot(data = dattc,mapping = ggplot2::aes(x = Year),drop = TRUE,binaxis = "x", stackdir = "up",method = "histodot",binwidth = 1,width = 0.2,inherit.aes = FALSE,fill = grDevices::grey(0.6),colour = grDevices::grey(0.6),alpha = 0.2,dotsize = 0.3)+
          ggplot2::annotate(geom = "text",x = min(dattc$Year)+5,y = 0,label = annot,alpha = 0.4,colour = grDevices::grey(0.6))

      }

    }else{

      p <- ggplot2::ggplot() +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       axis.line = element_line(colour = "black"),
                       plot.title = ggplot2::element_text(size = title_size),
                       axis.title = ggplot2::element_text(size = axis_title_size),
                       axis.text = ggplot2::element_text(size = axis_text_size)) +
        ggplot2::labs(title = paste(species, " ", i, sep = ""),
                      x = "Year",
                      y = "Annual index of abundance (mean count)") +
        ggplot2::geom_line(data = to_plot, ggplot2::aes(x = Year, y = Index), colour = cl, size = line_width) +
        ggplot2::geom_ribbon(data = to_plot, ggplot2::aes(x = Year, ymin = lci, ymax = uci),fill = cl, alpha = 0.3)+
        ggplot2::scale_x_continuous(breaks = yys)+
        ggplot2::scale_y_continuous(limits = c(0,NA))
      if(add_number_routes){

        p <- p + ggplot2::geom_dotplot(data = dattc,mapping = ggplot2::aes(x = Year),drop = TRUE,binaxis = "x", stackdir = "up",method = "histodot",binwidth = 1,width = 0.2,inherit.aes = FALSE,fill = grDevices::grey(0.6),colour = grDevices::grey(0.6),alpha = 0.2,dotsize = 0.3)+
          ggplot2::annotate(geom = "text",x = min(dattc$Year)+5,y = 0,label = annot,alpha = 0.4,colour = grDevices::grey(0.6))
      }

    }
    plot_list[[stringr::str_replace_all(paste(i),
                                        "[[:punct:]\\s]+",
                                        "_")]] <- p
    plot_index <- plot_index + 1
  }

  return(plot_list)
}


#' Generate plots of index trajectories by stratum
#'
#' Generates the indices plot for each stratum modelled.
#'
#' @param ci_width Numeric. Quantile defining the width of the plotted credible
#'   interval. Defaults to 0.95 (lower = 0.025 and upper = 0.975)
#' @param min_year Numeric. Minimum year to plot
#' @param max_year Numeric. Maximum year to plot
#' @param title Logical. Whether to include a title on the plot.
#' @param title_size Numeric. Font size of plot title. Defaults to 20
#' @param axis_title_size Numeric. Font size of axis titles. Defaults to 18
#' @param axis_text_size Numeric. Font size of axis text. Defaults to 16
#' @param line_width Numeric. Size of the trajectory line. Defaults to 1
#' @param add_observed_means Logical. Whether to include points indicating the
#'   observed mean counts. Default `FALSE`. Note: scale of observed means and
#'   annual indices may not match due to imbalanced sampling among routes
#' @param add_number_routes Logical. Whether to superimpose plot over a dotplot
#'   showing the number of BBS routes included in each year. This is useful as a
#'   visual check on the relative data-density through time because in most
#'   cases the number of observations increases over time
#'
#' @inheritParams common_docs
#'
#' @return List of ggplot objects, each entry being a plot of a stratum indices
#'
#' @examples
#'
#' # Toy example with Pacific Wren sample data
#' # First, stratify the sample data
#' s <- stratify(by = "bbs_cws", sample_data = TRUE)
#'
#' # Prepare the stratified data for modelling
#' d <- prepare_data(s,
#'                   min_year = 2009,
#'                   max_year = 2018)
#'
#' # Now run the model (fast but not good, just for illustration)
#' m <- run_model(d, model = "first_diff",
#'                iter_sampling = 5, iter_warmup = 5, chains = 2)
#'
#' # Generate country, continent, and stratum indices
#' i <- generate_indices(model_output = m,
#'                       regions = c("country", "continent", "stratum"))
#'
#' # Now, plot_indices() will generate a list of plots for all regions
#' plots <- plot_indices(i)
#'
#' # To view any plot, use [[i]]
#' plots[[1]]
#'
#' names(plots)
#'
#' # Suppose we wanted to access the continental plot. We could do so with
#' cont_plot <- plots[["continental"]]
#'
#' # You can specify to only plot a subset of years using min_year and max_year
#'
#' # Plots indices from 2015 onward
#' p_2015_min <- plot_indices(i, min_year = 2015)
#'
#' #Plot up indices up to the year 2017
#' p_2017_max <- plot_indices(i, max_year = 2017)
#'
#' #Plot indices between 2011 and 2016
#' p_2011_2016 <- plot_indices(i, min_year = 2011, max_year = 2016)
#'
#' @export
#'

plot_indices <- function(indices = NULL,
                         ci_width = 0.95,
                         min_year = NULL,
                         max_year = NULL,
                         title = TRUE,
                         title_size = 20,
                         axis_title_size = 18,
                         axis_text_size = 16,
                         line_width = 1,
                         add_observed_means = FALSE,
                         add_number_routes = FALSE) {

  # CHECKS

  species <- indices$meta_data$species
  indices <- indices$data_summary %>%
    calc_luq(ci_width)

  cl <- "#39568c"

  plot_list <- list()

  if(!is.null(min_year)) indices <- indices[indices$year >= min_year, ]
  if(!is.null(max_year)) indices <- indices[indices$year <= max_year, ]

  for (i in unique(indices$region)) {

    to_plot <- indices[which(indices$region == i), ]
    if(title) t <- paste0(species, " - ", i) else t <- ""

    if(add_number_routes){
      if(max(to_plot$n_routes) > 200) {
        ncby_y <- ceiling(to_plot$n_routes/50)
        annot <- c("each dot ~ 50 routes")
      } else {
        if(max(to_plot$n_routes) > 100) {
          ncby_y <- ceiling(to_plot$n_routes/10)
          annot <- c("each dot ~ 10 routes")
        } else {
          ncby_y <- to_plot$n_routes
          annot <- c("each dot = 1 route")
        }
      }

      names(ncby_y) <- to_plot$year
      dattc <- data.frame(year = rep(as.integer(names(ncby_y)), times = ncby_y))
    }

    p <- ggplot2::ggplot() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     plot.title = ggplot2::element_text(size = title_size),
                     axis.title = ggplot2::element_text(size = axis_title_size),
                     axis.text = ggplot2::element_text(size = axis_text_size)) +
      ggplot2::labs(title = t,
                    x = "Year",
                    y = "Annual index of abundance (mean count)") +
      ggplot2::scale_x_continuous(breaks = ~floor(pretty(.x))) +
      ggplot2::scale_y_continuous(limits = c(0, NA))



    if(add_observed_means) {
      p <- p +
        ggplot2::geom_point(data = to_plot,
                            ggplot2::aes(x = as.integer(.data$year),
                                         y = .data$obs_mean),
                            colour = "grey60")
    }

    p <- p +
      ggplot2::geom_line(
        data = to_plot, ggplot2::aes(x = .data$year, y = .data$index),
        colour = cl, size = line_width) +
      ggplot2::geom_ribbon(
        data = to_plot,
        ggplot2::aes(x = .data$year, ymin = .data$lci, ymax = .data$uci),
        fill = cl, alpha = 0.3)

    if(add_number_routes) {
      p <- p +
        ggplot2::geom_dotplot(
          data = dattc, mapping = ggplot2::aes(x = .data$year), drop = TRUE,
          binaxis = "x", stackdir = "up", method = "histodot", binwidth = 1,
          width = 0.2, inherit.aes = FALSE, fill = "grey60",
          colour = "grey60", alpha = 0.2, dotsize = 0.3) +
        ggplot2::annotate(
          geom = "text", x = min(dattc$year) + 5, y = 0, label = annot,
          alpha = 0.4, colour = "grey60")
    }

    plot_list[[stringr::str_replace_all(paste(i), "[[:punct:]\\s]+", "_")]] <- p
  }

  plot_list
}

calc_luq <- function(data, ci_width) {
  lq <- (1 - ci_width) / 2
  uq <- ci_width + lq

  n <- stringr::str_detect(names(data), paste0("index_q_(", lq, "|", uq, ")"))
  if(sum(n) < 2) {
    stop("A confidence interval of ", ci_width, " needs quantiles ",
         lq, " and ", uq, ". Re-run `generate_indices()` with the required ",
         "`quantiles`.", call. = FALSE)
  }

  data[["lci"]] <- data[[paste0("index_q_", lq)]]
  data[["uci"]] <- data[[paste0("index_q_", uq)]]

  data
}
