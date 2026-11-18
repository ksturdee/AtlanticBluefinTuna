# 07_BluefinTuna_FinalSpatialVisualisations.R

# Purpose:
# Create the final catch distribution and bathymetry figure for the
# Atlantic bluefin tuna project.

# Figure 1A: figure_one_regional_panel
# Regional reported catch distribution over GEBCO bathymetry.

# Figure 1BC: figure_one_hotspot_panels
# Two objectively selected high-catch clusters over detailed bathymetry.

# High-catch cells are defined using the 90th percentile of reported catch
# weight. DBSCAN groups neighbouring high-catch cell centroids within 30 km,
# with at least four cells required to form a cluster.

# The two clusters with the highest total reported catch are selected for
# the detailed inset maps. These are descriptive spatial clusters, not
# statistically significant hotspot tests.

# Before running:
# Run Scripts 02 and 03 so the required fisheries, land, map extent and
# bathymetry objects are available. This script previews figures only. 
# It does not export files.


# Load packages
library(sf)        # Vector geometry and spatial transformations
library(terra)     # Bathymetry raster processing
library(dplyr)     # Filtering and catch summaries
library(ggplot2)   # Custom bathymetry legends
library(here)      # Project-relative file paths
library(tmap)      # Spatial maps
library(dbscan)    # High-catch spatial clustering
library(cowplot)   # Final figure composition
library(grid)      # Figure grobs and units
library(gridExtra) # Panel headings and layouts
library(scales)    # Legend number formatting

tmap::tmap_mode("plot")


# Identify descriptive spatial clusters among the top-decile catch cells

# DBSCAN groups top-decile reporting-cell centroids located within 30 km.
# A minimum of four cells is required to distinguish spatial clusters from
# isolated high-catch cells. These parameters define descriptive catch
# concentrations and do not represent a statistical hotspot significance test.

bluefin_tuna_hotspot_candidates <-
  fisheries_species_data_projected$bluefin_tuna |>
  dplyr::filter(
    is.finite(roundWeight_kg),
    roundWeight_kg > 0
  )

hotspot_catch_threshold_kg <- unname(
  stats::quantile(
    bluefin_tuna_hotspot_candidates$roundWeight_kg,
    probs = 0.90,
    na.rm = TRUE
  )
)

bluefin_tuna_top_decile_cells <-
  bluefin_tuna_hotspot_candidates |>
  dplyr::filter(
    roundWeight_kg >= hotspot_catch_threshold_kg
  )


bluefin_tuna_top_decile_centroids <-
  sf::st_centroid(
    bluefin_tuna_top_decile_cells
  )

top_decile_coordinates <- sf::st_coordinates(
  bluefin_tuna_top_decile_centroids
)

# Group neighbouring high-catch cells within 30 km.
hotspot_cluster_result <- dbscan::dbscan(
  top_decile_coordinates,
  eps = 30000,
  minPts = 4
)

bluefin_tuna_top_decile_cells$hotspot_cluster <-
  hotspot_cluster_result$cluster

# Cluster zero contains isolated cells.
clustered_hotspot_cells <-
  bluefin_tuna_top_decile_cells |>
  dplyr::filter(
    hotspot_cluster > 0
  )

hotspot_cluster_summary <- clustered_hotspot_cells |>
  sf::st_drop_geometry() |>
  dplyr::group_by(hotspot_cluster) |>
  dplyr::summarise(
    catch_cells = dplyr::n(),
    total_catch_kg = sum(roundWeight_kg),
    median_catch_kg = median(roundWeight_kg),
    maximum_cell_catch_kg = max(roundWeight_kg),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(total_catch_kg)
  )

selected_hotspot_clusters <- hotspot_cluster_summary |>
  dplyr::slice_head(n = 2) |>
  dplyr::pull(hotspot_cluster)

hotspot_one_cells <- clustered_hotspot_cells |>
  dplyr::filter(
    hotspot_cluster == selected_hotspot_clusters[1]
  )

hotspot_two_cells <- clustered_hotspot_cells |>
  dplyr::filter(
    hotspot_cluster == selected_hotspot_clusters[2]
  )
# Preserve the ranking so Panel B is always the cluster with the highest
# total reported catch and Panel C is the second-highest.
selected_hotspot_summary <- hotspot_cluster_summary |>
  dplyr::filter(
    hotspot_cluster %in% selected_hotspot_clusters
  ) |>
  dplyr::arrange(
    match(
      hotspot_cluster,
      selected_hotspot_clusters
    )
  ) |>
  dplyr::mutate(
    inset_panel = c("B", "C")
  )

print(selected_hotspot_summary)

# Summarise the two selected high-catch clusters together
selected_hotspots_combined_summary <- selected_hotspot_summary |>
  dplyr::summarise(
    selected_clusters = dplyr::n(),
    top_decile_cells = sum(catch_cells),
    combined_reported_catch_kg = sum(total_catch_kg)
  )

print(selected_hotspots_combined_summary)


# Create square hotspot extents
create_hotspot_inset_extent <- function(
    hotspot_cells,
    buffer_distance_m = 50000
) {

  hotspot_extent <- sf::st_bbox(
    hotspot_cells
  )

  hotspot_centre_x <- mean(
    c(
      hotspot_extent[["xmin"]],
      hotspot_extent[["xmax"]]
    )
  )

  hotspot_centre_y <- mean(
    c(
      hotspot_extent[["ymin"]],
      hotspot_extent[["ymax"]]
    )
  )

  hotspot_width <- hotspot_extent[["xmax"]] -
    hotspot_extent[["xmin"]]

  hotspot_height <- hotspot_extent[["ymax"]] -
    hotspot_extent[["ymin"]]

  inset_half_width <- max(
    hotspot_width,
    hotspot_height
  ) / 2 + buffer_distance_m

  sf::st_bbox(
    c(
      xmin = hotspot_centre_x - inset_half_width,
      xmax = hotspot_centre_x + inset_half_width,
      ymin = hotspot_centre_y - inset_half_width,
      ymax = hotspot_centre_y + inset_half_width
    ),
    crs = sf::st_crs(hotspot_cells)
  )
}

hotspot_one_extent <- create_hotspot_inset_extent(
  hotspot_one_cells
)

hotspot_two_extent <- create_hotspot_inset_extent(
  hotspot_two_cells
)


# Shared catch styling

presentation_catch_breaks_kg <- c(
  0,
  10,
  100,
  1000,
  10000,
  Inf
)

presentation_catch_labels <- c(
  "0-10",
  "10-100",
  "100-1k",
  "1k-10k",
  "10k+"
)

presentation_catch_colours <- grDevices::hcl.colors(
  n = length(presentation_catch_labels),
  palette = "YlOrRd",
  rev = TRUE
)


# Regional inset boxes and labels
hotspot_one_extent_polygon <- sf::st_as_sf(
  sf::st_as_sfc(hotspot_one_extent)
)

hotspot_two_extent_polygon <- sf::st_as_sf(
  sf::st_as_sfc(hotspot_two_extent)
)

hotspot_inset_extent_polygons <- dplyr::bind_rows(
  hotspot_one_extent_polygon,
  hotspot_two_extent_polygon
)

create_inset_label_point <- function(
    inset_extent,
    panel_label
) {

  sf::st_as_sf(
    data.frame(
      inset_panel = panel_label,

      x = inset_extent[["xmin"]] +
        0.05 * (
          inset_extent[["xmax"]] -
            inset_extent[["xmin"]]
        ),

      y = inset_extent[["ymax"]] -
        0.07 * (
          inset_extent[["ymax"]] -
            inset_extent[["ymin"]]
        )
    ),
    coords = c("x", "y"),
    crs = sf::st_crs(inset_extent)
  )
}

hotspot_inset_labels <- dplyr::bind_rows(
  create_inset_label_point(
    hotspot_one_extent,
    "B"
  ),
  create_inset_label_point(
    hotspot_two_extent,
    "C"
  )
)


# Prepare catch centroids once for all three panels
bluefin_tuna_catch_centroids <-
  fisheries_species_data_projected$bluefin_tuna |>
  sf::st_centroid() |>
  dplyr::arrange(roundWeight_kg)


# FIGURE 1A
# Regional catch distribution and bathymetry

# Regional map extent

regional_map_extent <- map_bounding_box

original_map_width_m <- unname(
  map_bounding_box[["xmax"]] -
    map_bounding_box[["xmin"]]
)

# Add space on both sides of the regional map.
regional_map_extent[["xmin"]] <-
  map_bounding_box[["xmin"]] -
  0.75 * original_map_width_m

regional_map_extent[["xmax"]] <-
  map_bounding_box[["xmax"]] +
  0.55 * original_map_width_m

regional_map_aspect_ratio <- unname(
  (
    regional_map_extent[["xmax"]] -
      regional_map_extent[["xmin"]]
  ) /
    (
      regional_map_extent[["ymax"]] -
        regional_map_extent[["ymin"]]
    )
)


# Import expanded regional GEBCO

# Define the expanded regional GEBCO file used for Panel A
regional_gebco_path <- here::here(
  "data",
  "raw_data",
  "GEBCO",
  "GEBCO_2026_RegionalCartography_79W_47W_33N_53N.tif"
)


# Import expanded regional GEBCO
regional_gebco_path <- here::here(
  "data",
  "raw_data",
  "GEBCO",
  "GEBCO_2026_RegionalCartography_79W_47W_33N_53N.tif"
)

# Load regional bathymetry
regional_gebco <- terra::rast(
  regional_gebco_path
)
# Convert negative elevations to positive seabed depth.
regional_seabed_depth_m <- terra::ifel(
  regional_gebco < 0,
  -regional_gebco,
  NA
)

names(regional_seabed_depth_m) <- "seabed_depth_m"

# Prepare regional bathymetry

regional_extent_geographic <- regional_map_extent |>
  sf::st_as_sfc() |>
  sf::st_transform(4326) |>
  sf::st_bbox()

regional_bathymetry_cropped <- terra::crop(
  regional_seabed_depth_m,
  terra::ext(
    regional_extent_geographic[["xmin"]],
    regional_extent_geographic[["xmax"]],
    regional_extent_geographic[["ymin"]],
    regional_extent_geographic[["ymax"]]
  ),
  snap = "out"
)

regional_bathymetry_expanded <- terra::project(
  regional_bathymetry_cropped,
  paste0("EPSG:", study_area_projected_crs),
  method = "bilinear"
)


# Regional bathymetry styling

regional_bathymetry_colours <- grDevices::colorRampPalette(
  c(
    "#C9ECE8",
    "#8BD3D0",
    "#4CA9BF",
    "#377FA5",
    "#315B82",
    "#263F62"
  )
)(100)

regional_depth_breaks_m <- c(
  0,
  1000,
  2000,
  3000,
  4000,
  5500
)


# Create regional map

regional_bluefin_tuna_bathymetry_map <-

  tmap::tm_shape(
    regional_bathymetry_expanded,
    bbox = regional_map_extent
  ) +

  tmap::tm_raster(
    palette = regional_bathymetry_colours,
    style = "cont",
    breaks = seq(
      0,
      5500,
      length.out = 101
    ),
    legend.show = FALSE
  ) +

  tmap::tm_shape(
    study_area_land
  ) +

  tmap::tm_polygons(
    col = "grey92",
    border.col = "grey30",
    lwd = 0.3
  ) +

  tmap::tm_shape(
    bluefin_tuna_catch_centroids
  ) +

  tmap::tm_dots(
    col = "roundWeight_kg",
    style = "fixed",

    breaks = presentation_catch_breaks_kg,
    palette = presentation_catch_colours,
    labels = presentation_catch_labels,

    shape = 21,
    size = 0.10,
    alpha = 0.90,

    border.col = "grey20",
    border.lwd = 0.25,

    title = "Catch weight (kg)"
  ) +

  tmap::tm_shape(
    hotspot_inset_extent_polygons
  ) +

  tmap::tm_borders(
    col = "black",
    lwd = 1.5
  ) +

  tmap::tm_shape(
    hotspot_inset_labels
  ) +

  tmap::tm_text(
    text = "inset_panel",
    size = 1.0,
    col = "black",
    fontface = "plain",
    bg.color = "white",
    bg.alpha = 0.95
  ) +

  tmap::tm_scale_bar(
    position = c("right", "bottom"),
    width = 0.09,
    text.size = 0.45
  ) +

  tmap::tm_compass(
    position = c("right", "top"),
    size = 2.1,
    text.size = 0.70
  ) +

  tmap::tm_layout(
    main.title = "A. Regional catch distribution",
    main.title.position = "left",
    main.title.size = 1.0,
    main.title.fontface = "plain",

    bg.color = "#dce9f1",
    frame = TRUE,

    legend.outside = FALSE,
    legend.position = c("left", "center"),

    legend.bg.color = "white",
    legend.bg.alpha = 1,
    legend.frame = TRUE,

    legend.title.size = 0.80,
    legend.text.size = 0.65,

    legend.format = list(
      text.separator = "-"
    ),

    inner.margins = 0,
    asp = regional_map_aspect_ratio
  )


# Regional vertical bathymetry legend
regional_depth_legend_source <- ggplot2::ggplot(
  data.frame(
    x = 1,
    depth_m = seq(
      0,
      5500,
      length.out = 551
    )
  ),
  ggplot2::aes(
    x = x,
    y = depth_m,
    fill = depth_m
  )
) +

  ggplot2::geom_tile(
    width = 1,
    height = 10
  ) +

  ggplot2::scale_fill_gradientn(
    colours = regional_bathymetry_colours,
    limits = c(0, 5500),
    breaks = regional_depth_breaks_m,
    labels = scales::label_comma(),
    name = "Depth (m)",

    guide = ggplot2::guide_colourbar(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      label.position = "right",
      barheight = grid::unit(7.0, "cm"),
      barwidth = grid::unit(0.75, "cm"),
      ticks = TRUE,
      frame.colour = NA
    )
  ) +

  ggplot2::theme_void() +

  ggplot2::theme(
    legend.position = "right",

    legend.title = ggplot2::element_text(
      size = 11,
      face = "plain",
      colour = "grey10"
    ),

    legend.text = ggplot2::element_text(
      size = 10,
      colour = "grey10"
    ),

    legend.margin = ggplot2::margin(
      0, 0, 0, 0
    )
  )

regional_depth_legend_grob <- cowplot::get_legend(
  regional_depth_legend_source
)


# Approved Panel A composition

regional_map_grob <- tmap::tmap_grob(
  regional_bluefin_tuna_bathymetry_map
)

figure_one_regional_panel <- cowplot::ggdraw() +

  cowplot::draw_grob(
    regional_map_grob,
    x = 0,
    y = 0,
    width = 0.91,
    height = 1
  ) +

  cowplot::draw_grob(
    regional_depth_legend_grob,
    x = 0.88,
    y = 0.20,
    width = 0.11,
    height = 0.60
  )


# Preview Figure 1A
print(figure_one_regional_panel)


# FIGURE 1BC
# Two hotspot maps and detailed bathymetry

# Prepare hotspot bathymetry
prepare_hotspot_bathymetry <- function(
    inset_extent
) {

  geographic_extent <- inset_extent |>
    sf::st_as_sfc() |>
    sf::st_transform(4326) |>
    sf::st_bbox()

  cropped_bathymetry <- terra::crop(
    seabed_depth_m,
    terra::ext(
      geographic_extent[["xmin"]],
      geographic_extent[["xmax"]],
      geographic_extent[["ymin"]],
      geographic_extent[["ymax"]]
    ),
    snap = "out"
  )

  terra::project(
    cropped_bathymetry,
    paste0("EPSG:", study_area_projected_crs),
    method = "bilinear"
  )
}

hotspot_one_bathymetry <- prepare_hotspot_bathymetry(
  hotspot_one_extent
)

hotspot_two_bathymetry <- prepare_hotspot_bathymetry(
  hotspot_two_extent
)


# Prepare catches and contours

hotspot_one_all_catches <- sf::st_crop(
  bluefin_tuna_catch_centroids,
  hotspot_one_extent
)

hotspot_two_all_catches <- sf::st_crop(
  bluefin_tuna_catch_centroids,
  hotspot_two_extent
)

bathymetric_contour_depths_m <- c(
  200,
  500,
  1000,
  2000,
  3000,
  4000
)

bathymetric_contours <- terra::as.contour(
  seabed_depth_m,
  levels = bathymetric_contour_depths_m
)

bathymetric_contours_projected <- bathymetric_contours |>
  terra::project(
    paste0("EPSG:", study_area_projected_crs)
  ) |>
  sf::st_as_sf()

depth_contour_200m_projected <- sf::st_transform(
  depth_contour_200m,
  study_area_projected_crs
)


# Hotspot bathymetry styling

bathymetry_colours <- grDevices::colorRampPalette(
  c(
    "#7FDBDA",
    "#41B6C4",
    "#2C7FB8",
    "#253494",
    "#081D58"
  )
)(100)

bathymetry_display_breaks_m <- c(
  0,
  100,
  200,
  300,
  400
)


# Create hotspot map function

create_hotspot_bathymetry_map <- function(
    hotspot_bathymetry,
    all_catch_cells,
    inset_extent
) {

  tmap::tm_shape(
    hotspot_bathymetry,
    bbox = inset_extent
  ) +

    tmap::tm_raster(
      palette = bathymetry_colours,
      style = "cont",
      breaks = seq(
        0,
        400,
        length.out = 101
      ),
      legend.show = FALSE
    ) +

    tmap::tm_shape(
      bathymetric_contours_projected
    ) +

    tmap::tm_lines(
      col = "grey35",
      lwd = 0.40,
      alpha = 0.55
    ) +

    tmap::tm_shape(
      depth_contour_200m_projected
    ) +

    tmap::tm_lines(
      col = "grey20",
      lwd = 0.65,
      alpha = 0.75
    ) +

    tmap::tm_shape(
      study_area_land
    ) +

    tmap::tm_polygons(
      col = "grey92",
      border.col = "grey35",
      lwd = 0.35
    ) +

    tmap::tm_shape(
      all_catch_cells
    ) +

    tmap::tm_dots(
      col = "roundWeight_kg",
      style = "fixed",

      breaks = presentation_catch_breaks_kg,
      palette = presentation_catch_colours,

      shape = 21,
      size = 0.10,
      alpha = 0.90,

      border.col = "grey20",
      border.lwd = 0.25,

      legend.show = FALSE
    ) +

    # Larger distance bar for the approved B/C layout.
    tmap::tm_scale_bar(
      position = c("left", "top"),
      width = 0.18,
      text.size = 0.65
    ) +

    tmap::tm_layout(
      bg.color = "#dce9f1",
      frame = TRUE,

      main.title = "",
      title = "",

      legend.show = FALSE,

      inner.margins = 0,
      asp = 1
    )
}


# Create hotspot maps

hotspot_one_map <- create_hotspot_bathymetry_map(
  hotspot_bathymetry = hotspot_one_bathymetry,
  all_catch_cells = hotspot_one_all_catches,
  inset_extent = hotspot_one_extent
)

hotspot_two_map <- create_hotspot_bathymetry_map(
  hotspot_bathymetry = hotspot_two_bathymetry,
  all_catch_cells = hotspot_two_all_catches,
  inset_extent = hotspot_two_extent
)


# Convert hotspot maps to grobs

hotspot_one_grob_vertical <- tmap::tmap_grob(
  hotspot_one_map
)

hotspot_two_grob_vertical <- tmap::tmap_grob(
  hotspot_two_map
)


# Panel headings

panel_b_subtitle <- paste0(
  "Cluster ",
  selected_hotspot_summary$hotspot_cluster[1],
  " | ",
  selected_hotspot_summary$catch_cells[1],
  " top-decile cells | ",
  scales::comma(
    round(
      selected_hotspot_summary$total_catch_kg[1]
    )
  ),
  " kg"
)

panel_c_subtitle <- paste0(
  "Cluster ",
  selected_hotspot_summary$hotspot_cluster[2],
  " | ",
  selected_hotspot_summary$catch_cells[2],
  " top-decile cells | ",
  scales::comma(
    round(
      selected_hotspot_summary$total_catch_kg[2]
    )
  ),
  " kg"
)

panel_b_heading_vertical <- gridExtra::arrangeGrob(

  grid::textGrob(
    "B. Highest-catch hotspot",
    x = 0.5,
    hjust = 0.5,
    gp = grid::gpar(
      fontsize = 12,
      fontface = "plain",
      col = "grey15"
    )
  ),

  grid::textGrob(
    panel_b_subtitle,
    x = 0.5,
    hjust = 0.5,
    gp = grid::gpar(
      fontsize = 9,
      col = "grey30"
    )
  ),

  ncol = 1,
  heights = c(
    0.56,
    0.44
  )
)

panel_c_heading_vertical <- gridExtra::arrangeGrob(

  grid::textGrob(
    "C. Second-highest catch hotspot",
    x = 0.5,
    hjust = 0.5,
    gp = grid::gpar(
      fontsize = 12,
      fontface = "plain",
      col = "grey15"
    )
  ),

  grid::textGrob(
    panel_c_subtitle,
    x = 0.5,
    hjust = 0.5,
    gp = grid::gpar(
      fontsize = 9,
      col = "grey30"
    )
  ),

  ncol = 1,
  heights = c(
    0.56,
    0.44
  )
)


# Combine headings and maps

panel_b_vertical <- gridExtra::arrangeGrob(
  panel_b_heading_vertical,
  hotspot_one_grob_vertical,
  ncol = 1,
  heights = c(
    0.09,
    0.91
  )
)

panel_c_vertical <- gridExtra::arrangeGrob(
  panel_c_heading_vertical,
  hotspot_two_grob_vertical,
  ncol = 1,
  heights = c(
    0.09,
    0.91
  )
)

hotspot_maps_pair <- cowplot::plot_grid(

  cowplot::ggdraw() +
    cowplot::draw_grob(
      panel_b_vertical,
      x = 0,
      y = 0,
      width = 1,
      height = 1
    ),

  cowplot::ggdraw() +
    cowplot::draw_grob(
      panel_c_vertical,
      x = 0,
      y = 0,
      width = 1,
      height = 1
    ),

  nrow = 1,
  rel_widths = c(
    1,
    1
  )
)


# Vertical hotspot depth legend

hotspot_depth_legend_source <- ggplot2::ggplot(
  data.frame(
    x = 1,
    depth_m = seq(
      0,
      400,
      length.out = 401
    )
  ),
  ggplot2::aes(
    x = x,
    y = depth_m,
    fill = depth_m
  )
) +

  ggplot2::geom_tile(
    width = 1,
    height = 1
  ) +

  ggplot2::scale_fill_gradientn(
    colours = bathymetry_colours,
    limits = c(0, 400),
    breaks = bathymetry_display_breaks_m,
    labels = bathymetry_display_breaks_m,
    name = "Depth (m)",

    guide = ggplot2::guide_colourbar(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5,
      label.position = "right",
      barheight = grid::unit(7, "cm"),
      barwidth = grid::unit(0.75, "cm"),
      ticks = TRUE,
      frame.colour = NA
    )
  ) +

  ggplot2::theme_void() +

  ggplot2::theme(
    legend.position = "right",

    legend.title = ggplot2::element_text(
      size = 11,
      face = "plain",
      colour = "grey10"
    ),

    legend.text = ggplot2::element_text(
      size = 10,
      colour = "grey10"
    ),

    legend.margin = ggplot2::margin(
      0, 0, 0, 0
    ),

    plot.margin = ggplot2::margin(
      0, 0, 0, 0
    )
  )

hotspot_depth_legend_vertical <- cowplot::get_legend(
  hotspot_depth_legend_source
)


# Approved B/C composition

figure_one_hotspot_panels <- cowplot::plot_grid(

  hotspot_maps_pair,

  cowplot::ggdraw() +
    cowplot::draw_grob(
      hotspot_depth_legend_vertical,
      x = 0,
      y = 0.15,
      width = 1,
      height = 0.70
    ),

  nrow = 1,

  rel_widths = c(
    0.91,
    0.09
  )
)


# Preview Figure 1BC
print(figure_one_hotspot_panels)


# Cartographic diagnostic

# A small number of coastal centroids intersect the mapped land.
# These are retained because centroids represent reporting cells,
# not exact fishing locations.

centroids_on_land <- lengths(
  sf::st_intersects(
    bluefin_tuna_catch_centroids,
    study_area_land
  )
) > 0

centroid_land_summary <- tibble::tibble(
  total_centroids = nrow(
    bluefin_tuna_catch_centroids
  ),

  centroids_on_land = sum(
    centroids_on_land
  ),

  percentage_on_land = round(
    100 * mean(
      centroids_on_land
    ),
    2
  )
)

print(
  centroid_land_summary
)