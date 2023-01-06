#' Generate a map of trends by strata
#'
#' `plot_map()` allows you to generate a colour-coded map of there percent
#' change in species trends for each strata.
#'
#' @param slope Logical. Whether or not to map values of the alternative trend
#'   metric (slope of a log-linear regression) if `slope = TRUE` was used in
#'   `generate_trends()`,  through the annual indices. Default `FALSE`.
#' @param title Logical. Whether or not to include a title with species. Default
#'   `TRUE`.
#' @param col_viridis Logical flag to use "viridis" colour-blind friendly
#'   palette. Default is FALSE
#' @param species Defunct. Use `title` instead
#' @param stratify_by Defunct.
#' @param select Defunct.
#'
#' @inheritParams common_docs
#'
#' @return spplot object
#'
#' @examples
#' # Using the example model for Pacific Wrens...
#'
#' # Generate the continental and stratum indices
#' i <- generate_indices(pacific_wren_model)
#'
#' # Now generate trends
#' t <- generate_trends(i)
#'
#' # Generate the map
#' map <- plot_map(t)
#'
#' @export
#'

plot_map <- function(trends,
                     slope = FALSE,
                     title = TRUE,
                     col_viridis = FALSE,
                     species, stratify_by, select) {

  # Deprecated/Defunct args
  if(!missing(species)) dep_stop("3.0.0", "species", "`title`")
  if(!missing(stratify_by)) dep_stop("3.0.0", "stratify_by")
  if(!missing(select)) dep_stop("3.0.0", "select")

  # Checks
  check_data(trends)
  check_logical(slope, title, col_viridis)

  stratify_by <- trends[["meta_data"]]$stratify_by
  species <- trends[["meta_data"]]$species

  trends <- dplyr::filter(trends[["trends"]], .data$region_type == "stratum")
  start_year <- min(trends$start_year)
  end_year <- min(trends$end_year)


  map <- load_map(stratify_by)

  breaks <- c(-7, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 7)
  labls <- c(paste0("< ", breaks[1]),
             paste0(breaks[-c(length(breaks))],":", breaks[-c(1)]),
             paste0("> ",breaks[length(breaks)]))
  labls <- paste0(labls, " %")

  check_slope(trends, slope)
  if(slope) trend_col <- "slope_trend" else trend_col <- "trend"

  trends$t_plot <- as.numeric(as.character(trends[[trend_col]]))
  trends$t_plot <- cut(trends$t_plot, breaks = c(-Inf, breaks, Inf),
                      labels = labls)

  map <- dplyr::left_join(x = map, y = trends, by = c("strata_name" = "region"))

  if(title) {
    title <- paste(species, "trends", start_year, "-", end_year)
  } else title <- NULL

  m <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = map, ggplot2::aes(fill = .data$t_plot),
                     colour = "grey40", size = 0.1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title,
                  fill = paste0("Trend\n", start_year, "-", end_year)) +
    ggplot2::theme(legend.position = "right",
                   line = ggplot2::element_line(linewidth = 0.4),
                   rect = ggplot2::element_rect(linewidth = 0.1),
                   axis.text = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

  if(!col_viridis) {
    pal <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf",
             "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
    m <- m + ggplot2::scale_fill_manual(values = pal)
  } else {
    m <- m + ggplot2::scale_fill_viridis_d()
  }

  m
}
