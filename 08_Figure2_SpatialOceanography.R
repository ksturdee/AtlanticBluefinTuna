# 08_Figure2_SpatialOceanography.R
# Bluefin tuna catch, surface temperature and subtle bathymetry.
# Uses processed data from Scripts 02 and 04.
# Preview only.

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(scales)
library(grid)
library(here)

# Check required objects
required <- c(
  "mean_surface_temperature",
  "fisheries_species_data_projected",
  "study_area_land"
)

missing <- required[!vapply(required, exists, logical(1))]

if (length(missing)) {
  stop("Missing objects: ", paste(missing, collapse = ", "))
}

# Use the temperature raster CRS
map_crs <- sf::st_crs(terra::crs(mean_surface_temperature))

# Prepare tuna catch centroids
tuna_cells <- fisheries_species_data_projected$bluefin_tuna |>
  filter(is.finite(roundWeight_kg), roundWeight_kg > 0) |>
  st_transform(map_crs)

catch_breaks <- c(0, 10, 100, 1000, 10000, 50000)
catch_labels <- c("0–10", "10–100", "100–1k", "1k–10k", "10k–50k")

catch_colours <- grDevices::hcl.colors(
  5, "YlOrRd", rev = TRUE
)

tuna_points <- tuna_cells |>
  st_centroid() |>
  arrange(roundWeight_kg) |>
  mutate(
    catch_class = cut(
      roundWeight_kg,
      breaks = catch_breaks,
      labels = catch_labels,
      include.lowest = TRUE
    )
  )

# Set regional extent
bbox <- st_bbox(tuna_cells)

map_xlim <- c(bbox["xmin"] - 0.65, bbox["xmax"] + 0.65)
map_ylim <- c(bbox["ymin"] - 0.55, bbox["ymax"] + 0.55)

map_extent <- terra::ext(
  map_xlim[1], map_xlim[2],
  map_ylim[1], map_ylim[2]
)

# Crop existing GLORYS temperature raster
temperature_raster <- terra::crop(
  mean_surface_temperature,
  map_extent
)

temperature_data <- terra::as.data.frame(
  temperature_raster,
  xy = TRUE,
  na.rm = TRUE
)

names(temperature_data) <- c(
  "longitude", "latitude", "temperature_c"
)

# Load regional GEBCO bathymetry
gebco <- terra::rast(
  here::here(
    "data", "raw_data", "GEBCO",
    "GEBCO_2026_RegionalCartography_79W_47W_33N_53N.tif"
  )
)

# Convert the regional map extent to geographic coordinates
geographic_bbox <- sf::st_bbox(
  sf::st_transform(
    sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = unname(map_xlim[1]),
          ymin = unname(map_ylim[1]),
          xmax = unname(map_xlim[2]),
          ymax = unname(map_ylim[2])
        ),
        crs = map_crs
      )
    ),
    4326
  )
)

print(geographic_bbox)

# Crop GEBCO to the map area before aggregation and reprojection
gebco <- terra::crop(
  gebco,
  terra::ext(
    geographic_bbox[["xmin"]],
    geographic_bbox[["xmax"]],
    geographic_bbox[["ymin"]],
    geographic_bbox[["ymax"]]
  ),
  snap = "out"
)

# Convert negative elevations to positive seabed depths
gebco_depth <- terra::ifel(gebco < 0, -gebco, NA)

# Aggregate bathymetry for display only
gebco_depth <- terra::aggregate(
  gebco_depth,
  fact = 4,
  fun = mean,
  na.rm = TRUE
)

# Reproject to the temperature map CRS if necessary
if (!terra::same.crs(gebco_depth, temperature_raster)) {
  gebco_depth <- terra::project(
    gebco_depth,
    terra::crs(temperature_raster),
    method = "bilinear"
  )
}

bathymetry_data <- terra::as.data.frame(
  gebco_depth,
  xy = TRUE,
  na.rm = TRUE
)

names(bathymetry_data) <- c(
  "longitude", "latitude", "depth_m"
)

# Very subtle shading focused around the continental slope
bathymetry_data <- bathymetry_data |>
  mutate(
    overlay_alpha = 0.025 + 0.125 *
      exp(-0.5 * ((depth_m - 850) / 650)^2)
  )

# Prepare land
land <- st_transform(study_area_land, map_crs)

# Temperature contours
isotherms <- c(10, 12, 14, 16, 18)

contour_data <- ggplot_build(
  ggplot(
    temperature_data,
    aes(longitude, latitude, z = temperature_c)
  ) +
    geom_contour(breaks = isotherms)
)$data[[1]]

# Position one label per isotherm near the eastern map edge
contour_labels <- contour_data |>
  group_by(level) |>
  slice_min(
    abs(x - (map_xlim[2] - 0.45)),
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  mutate(
    label = paste0(level, "°C"),
    x = pmin(x - 0.08, map_xlim[2] - 0.25)
  )

# Build map
figure_two_spatial_temperature <- ggplot() +
  
  # Temperature background
  geom_raster(
    data = temperature_data,
    aes(longitude, latitude, fill = temperature_c)
  ) +
  
  scale_fill_gradientn(
    colours = c(
      "#253494", "#2C7FB8", "#41B6C4",
      "#A1DAB4", "#FFFFCC", "#FDAE61", "#D73027"
    ),
    name = "Mean surface\ntemperature (°C)",
    guide = guide_colourbar(
      barheight = unit(7, "cm"),
      barwidth = unit(0.65, "cm"),
      title.position = "top"
    )
  ) +
  
  # Subtle bathymetric shading
  geom_raster(
    data = bathymetry_data,
    aes(longitude, latitude, alpha = overlay_alpha),
    fill = "grey15",
    inherit.aes = FALSE
  ) +
  
  scale_alpha_identity(guide = "none") +
  
  # Temperature isotherms
  geom_contour(
    data = temperature_data,
    aes(longitude, latitude, z = temperature_c),
    breaks = isotherms,
    colour = "grey25",
    linewidth = 0.30,
    alpha = 0.55
  ) +
  
  # Highlight the 14°C contour
  geom_contour(
    data = temperature_data,
    aes(longitude, latitude, z = temperature_c),
    breaks = 14,
    colour = "grey15",
    linewidth = 0.65,
    alpha = 0.82
  ) +
  
  # Land
  geom_sf(
    data = land,
    fill = "grey92",
    colour = "grey35",
    linewidth = 0.25
  ) +
  
  # Tuna catches
  geom_sf(
    data = tuna_points,
    aes(colour = catch_class),
    size = 1.5,
    alpha = 0.92
  ) +
  
  scale_colour_manual(
    values = setNames(catch_colours, catch_labels),
    breaks = catch_labels,
    drop = FALSE,
    name = "Catch weight (kg)"
  ) +
  
  # Isotherm labels
  geom_label(
    data = contour_labels,
    aes(x, y, label = label),
    inherit.aes = FALSE,
    size = 3.2,
    fill = alpha("white", 0.78),
    colour = "grey15",
    label.size = 0,
    label.padding = unit(0.10, "lines")
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.18,
    text_cex = 0.78
  ) +
  
  # North arrow
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  coord_sf(
    crs = map_crs,
    default_crs = map_crs,
    xlim = map_xlim,
    ylim = map_ylim,
    expand = FALSE
  ) +
  
  labs(x = NULL, y = NULL) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.major = element_line(
      colour = alpha("white", 0.30),
      linewidth = 0.25
    ),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 9, colour = "grey25"),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.30, "cm"),
    plot.margin = margin(5, 10, 5, 5)
  )

# Preview only
print(figure_two_spatial_temperature)