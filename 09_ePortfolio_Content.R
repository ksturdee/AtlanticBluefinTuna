# 09_ePortfolio_Content.R
# Interactive versions of my Atlantic bluefin tuna maps

# Load the packages needed for the maps
library(sf)       # Work with spatial features
library(terra)    # Work with bathymetry and temperature rasters
library(dplyr)    # Filter and summarise catch data
library(leaflet)  # Create interactive maps
library(dbscan)   # Identify spatial clusters
library(here)     # Use project-relative paths
library(scales)   # Format catch values


# Load the processed fisheries and environmental dataset
catch_data <- sf::st_read(
  here::here(
    "data",
    "processed_data",
    "spatial_integration",
    "bluefin_tuna_fisheries_environment_2019_2023.gpkg"
  ),
  quiet = TRUE
)

# Keep reporting cells with positive catch values
catch_data <- catch_data |>
  dplyr::filter(
    is.finite(roundWeight_kg),
    roundWeight_kg > 0
  )

# Use the same projected CRS as my original spatial analysis
# EPSG:3347 measures distances in metres
catch_projected <- sf::st_transform(
  catch_data,
  3347
)

# Convert the catch data to geographic coordinates for Leaflet
# EPSG:4326 uses longitude and latitude
catch_data <- sf::st_transform(
  catch_data,
  4326
)
# Represent each reporting cell by its centroid
# This follows the approach used in Figures 1 and 2
catch_points <- sf::st_centroid(catch_projected) |>
  sf::st_transform(4326)

# Use the same catch classes as the original figures
catch_breaks <- c(0, 10, 100, 1000, 10000, Inf)

# Keep the original catch labels
catch_labels <- c(
  "0-10",
  "10-100",
  "100-1k",
  "1k-10k",
  "10k+"
)

# Use the same YlOrRd colours as Scripts 07 and 08
catch_colours <- grDevices::hcl.colors(
  5,
  "YlOrRd",
  rev = TRUE
)

# Assign each reporting cell to its catch class
catch_points <- catch_points |>
  dplyr::mutate(
    catch_class = cut(
      roundWeight_kg,
      breaks = catch_breaks,
      labels = catch_labels,
      include.lowest = TRUE
    )
  )

# Create the colour palette for the catch points
catch_palette <- leaflet::colorFactor(
  palette = catch_colours,
  domain = catch_labels,
  ordered = TRUE
)

# Prepare information shown when clicking a catch point
catch_points$popup_text <- paste0(
  "<b>Reporting hexagon:</b> ", catch_points$hexGridID,
  "<br><b>Reported catch:</b> ",
  scales::comma(catch_points$roundWeight_kg), " kg",
  "<br><b>Mean seabed depth:</b> ",
  round(catch_points$mean_seabed_depth_m), " m",
  "<br><b>Mean surface temperature:</b> ",
  round(catch_points$mean_surface_temperature_c, 2), " °C"
)


# Load the regional GEBCO bathymetry used in Figure 1A
regional_gebco <- terra::rast(
  here::here(
    "data",
    "raw_data",
    "GEBCO",
    "GEBCO_2026_RegionalCartography_79W_47W_33N_53N.tif"
  )
)

# Convert negative seabed elevations to positive depths
regional_depth <- terra::ifel(
  regional_gebco < 0,
  -regional_gebco,
  NA
)

# Reduce the raster resolution for interactive display
# This does not change the original bathymetry dataset
regional_factor <- max(
  1,
  ceiling(sqrt(terra::ncell(regional_depth) / 150000))
)

regional_depth_display <- terra::aggregate(
  regional_depth,
  fact = regional_factor,
  fun = mean,
  na.rm = TRUE
)

# Convert the display raster to geographic coordinates
regional_depth_display <- terra::project(
  regional_depth_display,
  "EPSG:4326",
  method = "bilinear"
)

# Use the bathymetry colours from Figure 1A
regional_depth_colours <- leaflet::colorNumeric(
  palette = grDevices::colorRampPalette(
    c(
      "#C9ECE8",
      "#8BD3D0",
      "#4CA9BF",
      "#377FA5",
      "#315B82",
      "#263F62"
    )
  )(100),
  domain = c(0, 5500),
  na.color = "transparent"
)

# Get the geographic boundaries of the regional bathymetry
bathymetry_extent <- terra::ext(regional_depth_display)

# Extract the western and eastern boundaries
map_west <- terra::xmin(bathymetry_extent)
map_east <- terra::xmax(bathymetry_extent)

# Extract the southern and northern boundaries
map_south <- terra::ymin(bathymetry_extent)
map_north <- terra::ymax(bathymetry_extent)


# Create interactive Figure 1A
figure_1A_interactive <- leaflet::leaflet(
  options = leaflet::leafletOptions(
    scrollWheelZoom = FALSE,  # Prevent accidental webpage zooming
    maxZoom = 10,             # Limit excessive zooming in
    maxBoundsViscosity = 1    # Restrict movement beyond the map extent
  )
) |>
  
  # Add a simple OpenStreetMap basemap
  leaflet::addTiles() |>
  
  # Add regional GEBCO bathymetry
  leaflet::addRasterImage(
    regional_depth_display,
    colors = regional_depth_colours,
    opacity = 0.85,
    project = FALSE,
    maxBytes = 20 * 1024 * 1024
  ) |>
  
  # Add catch locations using the original classifications
  leaflet::addCircleMarkers(
    data = catch_points,
    radius = 3,
    color = "#303030",
    weight = 0.4,
    fillColor = ~catch_palette(catch_class),
    fillOpacity = 0.9,
    popup = ~popup_text,
    group = "Reported catches"
  ) |>
  
  # Add the catch legend
  leaflet::addLegend(
    position = "bottomleft",
    colors = catch_colours,
    labels = catch_labels,
    title = "Catch weight (kg)",
    opacity = 1
  ) |>
  
  # Add the seabed-depth legend
  leaflet::addLegend(
    position = "bottomright",
    pal = regional_depth_colours,
    values = c(0, 5500),
    title = "Seabed depth (m)",
    opacity = 0.85
  ) |>
  
  # Allow visitors to turn catch locations on and off
  leaflet::addLayersControl(
    overlayGroups = "Reported catches",
    options = leaflet::layersControlOptions(
      collapsed = FALSE
    )
  ) |>
  
  # Add a distance scale
  leaflet::addScaleBar(
    position = "bottomleft"
  ) |>
  
  # Prevent panning beyond the bathymetry extent
  leaflet::setMaxBounds(
    lng1 = map_west,
    lat1 = map_south,
    lng2 = map_east,
    lat2 = map_north
  ) |>
  
  # Open the map at the full bathymetry extent
  leaflet::fitBounds(
    lng1 = map_west,
    lat1 = map_south,
    lng2 = map_east,
    lat2 = map_north
  )

# Preview Figure 1A without saving
figure_1A_interactive


# Identify high-catch cells using the 90th percentile
# This follows the hotspot method used in Script 07
catch_threshold <- stats::quantile(
  catch_projected$roundWeight_kg,
  probs = 0.90,
  na.rm = TRUE
)

# Keep reporting cells meeting the high-catch threshold
top_decile_cells <- catch_projected |>
  dplyr::filter(
    roundWeight_kg >= catch_threshold
  )

# Calculate centroids in the original projected CRS
top_decile_centroids <- sf::st_centroid(
  top_decile_cells
)

# Extract centroid coordinates for DBSCAN
cluster_coordinates <- sf::st_coordinates(
  top_decile_centroids
)

# Check that the projected coordinates use metres
# DBSCAN uses 30000 metres as its neighbourhood distance
crs_units <- sf::st_crs(top_decile_cells)$units_gdal

if (is.na(crs_units) ||
    !crs_units %in% c("metre", "meter", "metres", "meters")) {
  stop("The projected catch CRS must use metres.")
}

# Group nearby high-catch cells using the original DBSCAN settings
cluster_result <- dbscan::dbscan(
  cluster_coordinates,
  eps = 30000,  # Maximum neighbourhood distance of 30 km
  minPts = 4    # Minimum four cells needed for a cluster
)

# Add the DBSCAN cluster number to each reporting cell
top_decile_cells$hotspot_cluster <- cluster_result$cluster

# Remove isolated high-catch cells classified as noise
clustered_cells <- top_decile_cells |>
  dplyr::filter(
    hotspot_cluster > 0
  )

# Summarise reported catch within each cluster
cluster_summary <- clustered_cells |>
  sf::st_drop_geometry() |>
  dplyr::group_by(hotspot_cluster) |>
  dplyr::summarise(
    catch_cells = dplyr::n(),
    total_catch_kg = sum(roundWeight_kg),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(total_catch_kg)
  )

# Select the two clusters with the highest total catch
selected_clusters <- cluster_summary |>
  dplyr::slice_head(n = 2) |>
  dplyr::pull(hotspot_cluster)

# Stop if fewer than two clusters were identified
if (length(selected_clusters) < 2) {
  stop("Fewer than two DBSCAN clusters were identified.")
}

# Print the selected clusters for comparison with Script 07
print(
  cluster_summary |>
    dplyr::filter(
      hotspot_cluster %in% selected_clusters
    )
)

# Convert clustered cells to geographic coordinates for mapping
clustered_cells_map <- sf::st_transform(
  clustered_cells,
  4326
)

# Load the processed seabed-depth raster
hotspot_depth <- terra::rast(
  here::here(
    "data",
    "processed_data",
    "bathymetry",
    "seabed_depth_m.tif"
  )
)

# Reduce raster resolution for interactive display
hotspot_factor <- max(
  1,
  ceiling(sqrt(terra::ncell(hotspot_depth) / 150000))
)

hotspot_depth_display <- terra::aggregate(
  hotspot_depth,
  fact = hotspot_factor,
  fun = mean,
  na.rm = TRUE
)

# Convert the bathymetry raster to geographic coordinates
hotspot_depth_display <- terra::project(
  hotspot_depth_display,
  "EPSG:4326",
  method = "bilinear"
)

# Use the bathymetry colours from Figures 1B and 1C
hotspot_depth_colours <- leaflet::colorNumeric(
  palette = grDevices::colorRampPalette(
    c(
      "#7FDBDA",
      "#41B6C4",
      "#2C7FB8",
      "#253494",
      "#081D58"
    )
  )(100),
  domain = c(0, 400),
  na.color = "transparent"
)


# Create a function so Figures 1B and 1C use the same design
create_hotspot_map <- function(cluster_number) {
  
  # Select the reporting cells belonging to this cluster
  selected_cells <- clustered_cells_map |>
    dplyr::filter(
      hotspot_cluster == cluster_number
    )
  
  # Get the cluster extent in the original projected CRS
  selected_projected <- sf::st_transform(
    selected_cells,
    sf::st_crs(catch_projected)
  )
  
  # Find the centre and dimensions of the cluster
  cluster_bbox <- sf::st_bbox(selected_projected)
  
  cluster_centre_x <- mean(
    c(cluster_bbox["xmin"], cluster_bbox["xmax"])
  )
  
  cluster_centre_y <- mean(
    c(cluster_bbox["ymin"], cluster_bbox["ymax"])
  )
  
  cluster_width <- cluster_bbox["xmax"] - cluster_bbox["xmin"]
  cluster_height <- cluster_bbox["ymax"] - cluster_bbox["ymin"]
  
  # Add the same 50 km buffer used in Script 07
  half_width <- max(cluster_width, cluster_height) / 2 + 50000
  
  # Create a square extent around the selected cluster
  inset_bbox <- sf::st_bbox(
    c(
      xmin = unname(cluster_centre_x - half_width),
      ymin = unname(cluster_centre_y - half_width),
      xmax = unname(cluster_centre_x + half_width),
      ymax = unname(cluster_centre_y + half_width)
    ),
    crs = sf::st_crs(selected_projected)
  )
  
  # Convert the inset extent to geographic coordinates
  inset_geographic <- inset_bbox |>
    sf::st_as_sfc() |>
    sf::st_transform(4326) |>
    sf::st_bbox()
  
  # Show all reported catches within the inset
  # This matches the original Figure 1B/C approach
  inset_catches <- sf::st_crop(
    catch_points,
    inset_geographic
  )
  
  # Create popup information for the inset catch locations
  inset_catches$popup_text <- paste0(
    "<b>Reporting hexagon:</b> ", inset_catches$hexGridID,
    "<br><b>Reported catch:</b> ",
    scales::comma(inset_catches$roundWeight_kg), " kg"
  )
  
  # Create the interactive hotspot map
  hotspot_map <- leaflet::leaflet(
    options = leaflet::leafletOptions(
      scrollWheelZoom = FALSE,
      maxZoom = 12,
      maxBoundsViscosity = 1
    )
  ) |>
    
    # Add the OpenStreetMap basemap
    leaflet::addTiles() |>
    
    # Add the detailed seabed-depth raster
    leaflet::addRasterImage(
      hotspot_depth_display,
      colors = hotspot_depth_colours,
      opacity = 0.85,
      project = FALSE,
      maxBytes = 20 * 1024 * 1024
    ) |>
    
    # Add all reported catches within the inset extent
    leaflet::addCircleMarkers(
      data = inset_catches,
      radius = 4,
      color = "#303030",
      weight = 0.4,
      fillColor = ~catch_palette(catch_class),
      fillOpacity = 0.9,
      popup = ~popup_text,
      group = "Reported catches"
    ) |>
    
    # Add the original catch legend
    leaflet::addLegend(
      position = "bottomleft",
      colors = catch_colours,
      labels = catch_labels,
      title = "Catch weight (kg)",
      opacity = 1
    ) |>
    
    # Add the detailed bathymetry legend
    leaflet::addLegend(
      position = "bottomright",
      pal = hotspot_depth_colours,
      values = c(0, 400),
      title = "Seabed depth (m)",
      opacity = 0.85
    ) |>
    
    # Allow visitors to toggle catch locations
    leaflet::addLayersControl(
      overlayGroups = "Reported catches",
      options = leaflet::layersControlOptions(
        collapsed = FALSE
      )
    ) |>
    
    # Add a distance scale
    leaflet::addScaleBar(
      position = "bottomleft"
    ) |>
    
    # Keep the map within the regional bathymetry boundaries
    leaflet::setMaxBounds(
      lng1 = map_west,
      lat1 = map_south,
      lng2 = map_east,
      lat2 = map_north
    ) |>
    
    # Start closer to the centre of the bathymetry raster
    # A fixed zoom avoids fitting the full raster into the Viewer
    leaflet::setView(
      lng = mean(c(map_west, map_east)),
      lat = mean(c(map_south, map_north)),
      zoom = 5
    )
  
  # Return the finished interactive map
  hotspot_map
}


# Create Figure 1B using the highest-catch cluster
figure_1B_interactive <- create_hotspot_map(
  selected_clusters[1]
)

# Create Figure 1C using the second-highest catch cluster
figure_1C_interactive <- create_hotspot_map(
  selected_clusters[2]
)

# Preview Figure 1B
figure_1B_interactive

# Preview Figure 1C
figure_1C_interactive


# Load the five-year mean surface-temperature raster
temperature_raster <- terra::rast(
  here::here(
    "data",
    "processed_data",
    "oceanography",
    "glorys_mean_surface_temperature_2019_2023.tif"
  )
)

# Reduce raster resolution for interactive display
temperature_factor <- max(
  1,
  ceiling(sqrt(terra::ncell(temperature_raster) / 150000))
)

temperature_display <- terra::aggregate(
  temperature_raster,
  fact = temperature_factor,
  fun = mean,
  na.rm = TRUE
)

# Convert the display raster to geographic coordinates
temperature_display <- terra::project(
  temperature_display,
  "EPSG:4326",
  method = "bilinear"
)

# Find the actual temperature range
temperature_range <- as.numeric(
  terra::global(
    temperature_display,
    range,
    na.rm = TRUE
  )[1, ]
)

# Use the temperature colours from Figure 2
temperature_colours <- leaflet::colorNumeric(
  palette = c(
    "#253494",
    "#2C7FB8",
    "#41B6C4",
    "#A1DAB4",
    "#FFFFCC",
    "#FDAE61",
    "#D73027"
  ),
  domain = temperature_range,
  na.color = "transparent"
)

# Get the geographic extent of the temperature raster
temperature_extent <- terra::ext(temperature_display)

# Create the interactive version of Figure 2
figure_2_interactive <- leaflet::leaflet(
  options = leaflet::leafletOptions(
    scrollWheelZoom = FALSE,
    maxZoom = 10,
    maxBoundsViscosity = 1
  )
) |>
  
  # Add the OpenStreetMap basemap
  leaflet::addTiles() |>
  
  # Add the five-year mean sea-surface temperature
  leaflet::addRasterImage(
    temperature_display,
    colors = temperature_colours,
    opacity = 0.9,
    project = FALSE,
    maxBytes = 20 * 1024 * 1024
  ) |>
  
  # Add the reported catch locations
  leaflet::addCircleMarkers(
    data = catch_points,
    radius = 3,
    color = "#303030",
    weight = 0.4,
    fillColor = ~catch_palette(catch_class),
    fillOpacity = 0.9,
    popup = ~popup_text,
    group = "Reported catches"
  ) |>
  
  # Add the catch legend
  leaflet::addLegend(
    position = "bottomleft",
    colors = catch_colours,
    labels = catch_labels,
    title = "Catch weight (kg)",
    opacity = 1
  ) |>
  
  # Add the temperature legend
  leaflet::addLegend(
    position = "bottomright",
    pal = temperature_colours,
    values = temperature_range,
    title = "Mean surface temperature (°C)",
    opacity = 0.9
  ) |>
  
  # Allow visitors to turn catch locations on and off
  leaflet::addLayersControl(
    overlayGroups = "Reported catches",
    options = leaflet::layersControlOptions(
      collapsed = FALSE
    )
  ) |>
  
  # Add a distance scale
  leaflet::addScaleBar(
    position = "bottomleft"
  ) |>
  
  # Restrict panning to the temperature raster extent
  leaflet::setMaxBounds(
    lng1 = terra::xmin(temperature_extent),
    lat1 = terra::ymin(temperature_extent),
    lng2 = terra::xmax(temperature_extent),
    lat2 = terra::ymax(temperature_extent)
  ) |>
  
  # Start the map at the temperature raster extent
  leaflet::fitBounds(
    lng1 = terra::xmin(temperature_extent),
    lat1 = terra::ymin(temperature_extent),
    lng2 = terra::xmax(temperature_extent),
    lat2 = terra::ymax(temperature_extent)
  )

# Preview Figure 2 without saving
figure_2_interactive