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
#' @param species Defunct. Use `title` instead
#' @param indices_list Deprecated. Use `indices` instead
#'
#' @inheritParams common_docs
#'
#' @return List of ggplot objects, each entry being a plot of a stratum indices
#'
#' @examples
#'
#' # Using the example model for Pacific Wrens...
#'
#' # Generate country, continent, and stratum indices
#' i <- generate_indices(model_output = pacific_wren_model,
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
                         add_number_routes = FALSE,
                         indices_list, species) {

  # Deprecated/Defunct args
  if(!missing(indices_list)) {
    dep_warn("3.0.0", "indices_list", "`indices`")
    indices <- indices_list
  }
  if(!missing(species)) dep_stop("3.0.0", "species", "`title`")


  # Checks
  check_data(indices)
  check_logical(title, add_observed_means, add_number_routes)
  check_numeric(ci_width, title_size, axis_title_size, axis_text_size, line_width)
  check_numeric(min_year, max_year, allow_null = TRUE)
  check_range(ci_width, c(0.001, 0.999))

  species <- indices$meta_data$species
  indices <- indices$indices %>%
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
                    y = "Annual index of abundance\n(mean count)") +
      ggplot2::scale_x_continuous(breaks = ~floor(pretty(.x))) +
      ggplot2::scale_y_continuous(limits = c(0, NA))



    if(add_observed_means) {
      p <- p +
        ggplot2::geom_point(data = to_plot,
                            ggplot2::aes(x = as.integer(.data$year),
                                         y = .data$obs_mean),
                            colour = "grey60", na.rm = TRUE)
    }

    p <- p +
      ggplot2::geom_line(
        data = to_plot, ggplot2::aes(x = .data$year, y = .data$index),
        colour = cl, linewidth = line_width, ) +
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
