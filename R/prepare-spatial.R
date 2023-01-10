#' Define neighbouring strata for spatial analyses
#'
#' Given the prepared data and a spatial data frame of polygons outlining
#' strata, identify a neighbourhood matrix for use in modelling.
#'
#' @param strata_map `sf` Data Frame. `sf` map of the strata in
#'   (MULTI)POLYGONs. Must have column "strata_name" matching strata output from
#'   `prepare_data()`.
#' @param voronoi Logical. Whether or not to use Voroni method. Default `FALSE`.
#' @param nearest_fill Logical. For strata with no neighbours, whether or not to
#'   fill in by centroids of the 2 nearest neighbours when **not** using the
#'   Voronoi method. Default `FALSE`.
#' @param island_link_dist_factor Numeric. Distances within a factor of this
#'   amount are considered nearest strata neighbours. Used when linking
#'   otherwise isolated islands of strata, when **not** using the
#'   Voronoi method. Default 1.2.
#' @param buffer_type Character. Which buffer type to use when using the Voronoi
#'   method. Must be one of `buffer` (default) or `convex_hull`. See Details for
#'   specifics.
#' @param buffer_dist Numeric. Distance to buffer and link the strata if not
#'   connected when using the Voronoi method. Units are that of
#'   `sf::st_crs(strata_map)`. This is the *starting* distance if `buffer_type =
#'   "buffer"` or the final distance if `buffer_type = "convex_hull"`. Default
#'   10000. See Details.
#' @param add_map `sf` object. Spatial data to add to map output.
#' @param label_size Numeric. Size of the labels on the map. For data with many
#'   different strata it can be useful to reduce the size of the labels. Default
#'   3.
#'
#' @inheritParams common_docs
#'
#' @details
#' When using the Voronoi method, a buffer is used to fill around and link
#' strata together. If the `buffer_type` is `buffer`, `buffer_dist` is the
#' starting distance over which to buffer. If not all strata are linked, this
#' distance is increased by 10% and applied again, repeating until all strata
#' are linked. If `buffer_type` is `convex_hull`, then a convex hull is used to
#' link up the strata before applying a buffer at a distance of `buffer_dist`.
#' Note that all distances are in the units of `sf::st_crs(strata_map)`.
#'
#' @return List of prepared (meta) data to be used for modelling and further
#'   steps.
#'   - `spatial_data` list of samples, nodes, adjacent matrix and map
#'      visualizing the matrix
#'   - `model_data` list of data formatted for use in Stan modelling
#'   - `meta_data` meta data defining the analysis
#'   - `meta_strata` data frame listing strata meta data
#'   - `raw_data` contains a data frame of summarized data used to create
#'     `model_data` (just formatted more nicely)
#'
#' @examples
#'
#' map <- load_map("bbs_cws")
#' s <- stratify(by = "bbs_cws", species = "Connecticut Warbler")
#' p <- prepare_data(s, min_max_route_years = 2)
#' sp <- prepare_spatial(p, map)
#'
#' # Visually explore the spatial linkages
#' sp$map
#'
#' # Overlay subset strata map on original mapping data
#' sp <- prepare_spatial(p, map, add_map = map)
#' sp$map
#'
#' @export
prepare_spatial <- function(prepared_data,
                            strata_map,
                            voronoi = FALSE,
                            nearest_fill = FALSE,
                            island_link_dist_factor = 1.2,
                            buffer_type = "buffer",
                            buffer_dist = 10000,
                            add_map = NULL,
                            label_size = 3,
                            quiet = FALSE) {

  # Checks
  check_sf(strata_map, check_poly = TRUE)
  check_sf(add_map)
  check_data(prepared_data)

  check_in(buffer_type, c("buffer", "convex_hull"))
  check_logical(voronoi, nearest_fill, quiet)
  check_numeric(island_link_dist_factor, buffer_dist)

  # Prepare spatial data
  if(!quiet) message("Preparing spatial data...")

  # Filter to only strata in prepared_data
  strata_map <- strata_map %>%
    dplyr::semi_join(prepared_data$raw_data, by = "strata_name")

  if(nrow(strata_map) == 0) {
    stop("There are no strata in `strata_map` that match strata in ",
         "`prepared_data`.\nDo the values in the `strata_name` columns match?",
         call. = FALSE)
  }

  # Only summarize if necessary
  if(dplyr::n_distinct(strata_map[["strata_name"]]) != nrow(strata_map)) {
    if(!quiet) message("    Summarizing polygons by strata...")
    strata_map <- strata_map %>%
      dplyr::group_by(.data[["strata_name"]]) %>%
      dplyr::summarise()
  }

  # Omit unnecessary columns and ensure strata order is the same
  strata_map <- strata_map %>%
    dplyr::select("strata_name") %>%
    dplyr::arrange(.data[["strata_name"]])

  # Set attributes as constant to avoid sf warnings
  # (cf. https://github.com/r-spatial/sf/issues/406)
  sf::st_agr(strata_map) <- "constant"

  # Calculate centres
  centres <- sf::st_centroid(strata_map)

  # Neighbours - NOT Voronoi -------------
  if(!voronoi) {
    if(!quiet) message("Identifying neighbours (non-Voronoi method)...")
    nb_db <- spdep::poly2nb(strata_map,
                            row.names = strata_map[["strata_name"]],
                            queen = FALSE)

    nb_weights <- spdep::nb2WB(nb_db)

    # Fix missing neighbours
    if(nearest_fill && min(nb_weights$num) == 0){
      if(!quiet) message("Some strata have no neighbours. ",
                         "Filling by 2 nearest neighbours by centroids...")

      nn <- spdep::knearneigh(centres, k = 2)[[1]]
      no_neighbour <- which(nb_weights$num == 0)

      for(w in no_neighbour) {
        nb_db <- fix_no_neighbours(w, nb_db, nn)
      }
    }

    # Fix islands
    nb_db <- fix_islands(nb_db, centres, island_link_dist_factor, quiet)

    # Get bounding box
    bbox <- sf::st_bbox(strata_map) %>%
      sf::st_as_sfc()

    vint <- NULL

  } else {
    # Neighbours - Voronoi -------------
    if(!quiet) message("Identifying neighbours (Voronoi method)...")
    centre_union <- sf::st_union(centres)

    if(buffer_type == "convex_hull") {

      cov_hull_buf <- centre_union %>%
        sf::st_convex_hull() %>%
        sf::st_buffer(dist = buffer_dist)

    } else if (buffer_type == "buffer") {

      cov_hull_buf <- sf::st_buffer(centre_union, dist = buffer_dist)

      # gradually increases buffer until all sites are linked
      while(length(cov_hull_buf[[1]]) > 1) {
        buffer_dist <- buffer_dist * 1.1
        cov_hull_buf <-  sf::st_buffer(centre_union, dist = buffer_dist)
      }
    }

    bbox <- sf::st_bbox(centres) %>%
      sf::st_as_sfc()

    v <- centre_union %>%
      sf::st_voronoi(envelope = bbox) %>%
      sf::st_cast()

    vint <- sf::st_intersection(v, cov_hull_buf) %>%
      sf::st_cast("POLYGON") %>%
      sf::st_sf() %>%
      sf::st_join(centres, join = sf::st_contains) %>%
      dplyr::arrange(.data[["strata_name"]])

    nb_db <- spdep::poly2nb(vint,
                            row.names = vint[["strata_name"]],
                            queen = FALSE) # polygon to neighbour definition
  }

  if(!quiet) message("Formating neighbourhood matrices...")

  # Calc/Update weights
  nb_weights <- spdep::nb2WB(nb_db)

  if(voronoi && min(nb_weights$num) == 0) {
    stop("Some strata have no neighbours in Voronoi method.", call. = FALSE)
  }

  # Binary adjacency matrix
  nb_mat <- spdep::nb2mat(nb_db, style = "B", zero.policy = TRUE)

  if(!quiet) message("Plotting neighbourhood matrices...")
  map <- plot_neighbours(strata_map, centres, nb_db, bbox, vint, add_map,
                         label_size)

  # Reformat nodes and edges
  nb <- append(
    nb_fmt(nb_weights),
    list("adj_matrix" = nb_mat,
         "map" = map))

  append(
    list("spatial_data" = nb),
    prepared_data)
}



nb_fmt <- function(nb_weights) {
  num <- nb_weights$num
  adj <- nb_weights$adj

  node1 <- vector()
  node2 <- vector()

  i_adj <- 0
  i_edge <- 0

  for (i in seq_along(num)) {
    for (j in seq_len(num[i])) {
      i_adj <- i_adj + 1
      if (i < adj[i_adj]) {
        i_edge <- i_edge + 1
        node1[i_edge] <- i
        node2[i_edge] <- adj[i_adj]
      }
    }
  }
  list("n" = length(num),
       "n_edges" = length(adj) / 2,
       "node1" = node1,
       "node2" = node2)
}


fix_no_neighbours <- function(which, nb_db, nn) {
  for(j in 1:2){
    n1 <- nn[which, j]

    nb_db[[which]] <- as.integer(unique(c(nb_db[[which]], n1)))
    nb_db[[which]] <- nb_db[[which]][nb_db[[which]] != 0]

    nb_db[[n1]] <- as.integer(unique(c(nb_db[[n1]], which)))
    nb_db[[n1]] <- nb_db[[n1]][nb_db[[n1]] != 0]

    # message(" Linking ", which, " to ", n1)
  }
  nb_db
}


fix_islands <- function(nb_db, centres, island_link_dist_factor, quiet) {
  dist_centres <- sf::st_distance(centres) %>%
    units::drop_units()
  islands <- spdep::n.comp.nb(nb_db)
  n_islands <- islands$nc

  if(n_islands > 1 & !quiet) {
    message("Linking islands (isolated groups of nodes)...")
  }

  while(n_islands > 1) {
    if(!quiet) message("    Islands found (", n_islands - 1, "). ",
                       "Linking by distance between centroids...")

    # Get sites in the first island and distances among them
    isld1 <- which(islands$comp.id == 1)
    dist <- dist_centres[isld1, -isld1, drop = FALSE]
    dist_min <- apply(dist, 1, min)              # min dist for each site (row)
    closest <- names(which.min(dist_min))  # min dist overall

    isld2 <- which(
      dist_centres[closest, ] == dist_min[closest] |
        (dist_centres[closest, ] > dist_min[closest] &
           dist_centres[closest, ] <
           dist_min[closest] * island_link_dist_factor)
    )

    # Omit strata already in the isolated group
    if(any(isld1 %in% isld2)) isld2 <- isld2[!isld2 %in% isld1]

    # isld2 are the strata that should be linked to the isolated group
    closest <- as.integer(closest)
    for(i in isld2){
      a <- union(nb_db[[i]], closest)
      nb_db[[i]] <- a[a != 0]

      b <- union(nb_db[[closest]], i)
      nb_db[[closest]] <- b[b != 0]
    }

    islands <- spdep::n.comp.nb(nb_db)
    n_islands <- islands$nc

  }

  nb_db
}


plot_neighbours <- function(strata_map, centres, nb_db, bbox, vint, add_map,
                            label_size) {

  # Coordinates
  coords <- as.data.frame(sf::st_coordinates(centres))

  nb_l <- spdep::nb2listw(nb_db, )
  nt <- length(attributes(nb_l$neighbours)$region.id)

  nb_connect <- data.frame(
    from = rep(1:nt, vapply(nb_l$neighbours, length, FUN.VALUE = 1L)),
    to = unlist(nb_l$neighbours)) %>%
    dplyr::bind_cols(
      stats::setNames(coords[.$from, c("X", "Y")], c("long", "lat")),
      stats::setNames(coords[.$to, c("X", "Y")], c("long_to", "lat_to")))

  # Basic map
  g <- ggplot2::ggplot(data = centres) +
    ggplot2::theme_minimal() +
    #ggplot2::coord_sf(xlim = xb, ylim = yb)+
    ggplot2::theme(legend.position = "none")

  # Add map layer
  if(!is.null(add_map)){
    g <- g +
      ggplot2::geom_sf(data = add_map, alpha = 0, colour = "grey90")
  }

  # Add strata map and centres
  g <- g +
    ggplot2::geom_sf(data = strata_map, alpha = 0, colour = "grey85") +
    ggplot2::geom_sf(ggplot2::aes(col = .data[["strata_name"]], alpha = 0.5)) +
    ggplot2::geom_sf_text(ggplot2::aes(label = .data[["strata_name"]]),
                          size = label_size, alpha = 0.7, colour = "black")

  # Add nb_connect
  g <- g +
    ggplot2::geom_segment(
      data = nb_connect,
      ggplot2::aes(x = .data$long, y = .data$lat,
                   xend = .data$long_to, yend = .data$lat_to),
      inherit.aes = FALSE, linewidth = 0.3, alpha = 0.4)

  if(!is.null(vint)) {
    g <- g + ggplot2::geom_sf(data = vint, alpha = 0, colour = "grey95")
  }

  g
}
