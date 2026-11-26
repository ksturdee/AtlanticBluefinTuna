# 09_ePortfolio_Content.R
# Interactive versions of my Atlantic bluefin tuna maps

# Load the packages needed for the maps
library(sf)       # Work with spatial data
library(terra)    # Work with bathymetry and temperature rasters
library(dplyr)    # Filter and summarise catch data
library(leaflet)  # Create interactive maps
library(dbscan)   # Identify spatial clusters
library(here)     # Use relative project paths
library(scales)   # Format catch values
library(htmlwidgets) # Adjust interactive map behaviour


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

# Convert the catch polygons to geographic coordinates for Leaflet
catch_data <- sf::st_transform(
  catch_data,
  4326
)

# Represent reporting cells using their geometric centroids
# Calculate centroids before converting to longitude and latitude
catch_points <- sf::st_centroid(catch_projected) |>
  sf::st_transform(4326)

# Use the same catch classes as Figures 1 and 2
catch_breaks <- c(0, 10, 100, 1000, 10000, Inf)

# Keep the original catch labels
catch_labels <- c(
  "0-10",
  "10-100",
  "100-1k",
  "1k-10k",
  "10k+"
)

# Use the same YlOrRd palette as my original figures
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

# Create a colour palette for the catch points
catch_palette <- leaflet::colorFactor(
  palette = catch_colours,
  domain = catch_labels,
  ordered = TRUE
)

# Prepare the information shown when clicking a catch location
catch_points$popup_text <- paste0(
  "<b>Reporting hexagon:</b> ", catch_points$hexGridID,
  "<br><b>Reported catch:</b> ",
  scales::comma(catch_points$roundWeight_kg), " kg",
  "<br><b>Mean seabed depth:</b> ",
  round(catch_points$mean_seabed_depth_m), " m",
  "<br><b>Mean surface temperature:</b> ",
  round(catch_points$mean_surface_temperature_c, 2), " °C"
)


# Find the geographic extent of the catch locations
catch_extent <- sf::st_bbox(catch_points)

# Add some space around the catches for regional context
# These values control how much surrounding ocean is visible
regional_padding_lng <- 2.0
regional_padding_lat <- 1.5

# Create a regional extent around the reported catches
regional_view <- c(
  west = unname(catch_extent["xmin"]) - regional_padding_lng,
  south = unname(catch_extent["ymin"]) - regional_padding_lat,
  east = unname(catch_extent["xmax"]) + regional_padding_lng,
  north = unname(catch_extent["ymax"]) + regional_padding_lat
)


# Create a function to control the map boundaries
# This is useful when maps are embedded at different sizes
limit_map_view <- function(map, bounds) {
  
  # Pass the geographic boundaries to JavaScript
  map <- htmlwidgets::onRender(
    map,
    
    # Use the supplied coordinates rather than undefined map options
    sprintf(
      "
      function(el, x) {

        var map = this;

        var bounds = L.latLngBounds(
          [%f, %f],
          [%f, %f]
        );

        function updateView() {

          // Work out the zoom needed to fill the map window
          var minimumZoom = map.getBoundsZoom(bounds, true);

          // Do not allow zooming out beyond this level
          map.setMinZoom(minimumZoom);

          // Restrict movement to the selected region
          map.setMaxBounds(bounds);

          // Centre the map on the selected area
          map.setView(bounds.getCenter(), minimumZoom, {
            animate: false
          });

          // Correct the map position if the embed size changes
          map.panInsideBounds(bounds, {
            animate: false
          });

        }

        // Apply the view when the map is ready
        map.whenReady(updateView);

        // Update the view when Google Sites changes the map size
        map.on('resize', updateView);

      }
      ",
      bounds["south"],
      bounds["west"],
      bounds["north"],
      bounds["east"]
    )
  )
  
  # Return the map with its boundaries applied
  map
}


# Load the regional GEBCO bathymetry used in Figure 1A
regional_gebco <- terra::rast(
  here::here(
    "data",
    "raw_data",
    "GEBCO",
    "GEBCO_2026_RegionalCartography_79W_47W_33N_53N.tif"
  )
)

# Convert negative elevations to positive seabed depths
regional_depth <- terra::ifel(
  regional_gebco < 0,
  -regional_gebco,
  NA
)

# Create a bounding box around the regional catch area
# Remove coordinate names so sf recognises the four boundaries
regional_view_polygon <- sf::st_as_sfc(
  sf::st_bbox(
    c(
      xmin = unname(regional_view["west"]),
      ymin = unname(regional_view["south"]),
      xmax = unname(regional_view["east"]),
      ymax = unname(regional_view["north"])
    ),
    crs = sf::st_crs(4326)
  )
)

# Transform the view to match the GEBCO raster
regional_view_polygon <- sf::st_transform(
  regional_view_polygon,
  terra::crs(regional_depth)
)

# Get the extent needed for cropping the bathymetry
regional_crop_extent <- terra::ext(
  terra::vect(regional_view_polygon)
)

# Crop the bathymetry to the area surrounding the catches
# This removes the unnecessary parts of the regional raster
regional_depth_cropped <- terra::crop(
  regional_depth,
  regional_crop_extent,
  snap = "out"
)

# Reduce raster resolution for interactive display
# The original scientific dataset is not changed
regional_factor <- max(
  1,
  ceiling(
    sqrt(
      terra::ncell(regional_depth_cropped) / 150000
    )
  )
)

# Aggregate the display raster
regional_depth_display <- terra::aggregate(
  regional_depth_cropped,
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

# Remove completely empty rows and columns
regional_depth_display <- terra::trim(
  regional_depth_display
)

# Use the same bathymetry colours as Figure 1A
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

# Get the actual extent of the displayed bathymetry
bathymetry_extent <- terra::ext(
  regional_depth_display
)

# Store the geographic boundaries
map_west <- terra::xmin(bathymetry_extent)
map_east <- terra::xmax(bathymetry_extent)
map_south <- terra::ymin(bathymetry_extent)
map_north <- terra::ymax(bathymetry_extent)

# Store the boundaries for the interactive map
regional_bounds <- c(
  west = map_west,
  south = map_south,
  east = map_east,
  north = map_north
)


# Create the interactive version of Figure 1A
figure_1A_interactive <- leaflet::leaflet(
  options = leaflet::leafletOptions(
    scrollWheelZoom = FALSE,
    maxZoom = 10,
    maxBoundsViscosity = 1
  )
) |>
  
  # Add an OpenStreetMap basemap
  leaflet::addTiles() |>
  
  # Add the regional seabed-depth raster
  leaflet::addRasterImage(
    regional_depth_display,
    colors = regional_depth_colours,
    opacity = 0.95,
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
  
  # Add the catch-weight legend
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
    opacity = 0.95
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
  )

# Apply the bathymetry boundaries to Figure 1A
figure_1A_interactive <- limit_map_view(
  figure_1A_interactive,
  regional_bounds
)

# Preview Figure 1A without saving
figure_1A_interactive


# Identify the highest-catch 10% of reporting cells
# This follows the method used in Script 07
catch_threshold <- stats::quantile(
  catch_projected$roundWeight_kg,
  probs = 0.90,
  na.rm = TRUE
)

# Keep cells meeting the high-catch threshold
top_decile_cells <- catch_projected |>
  dplyr::filter(
    roundWeight_kg >= catch_threshold
  )

# Calculate centroids in EPSG:3347
top_decile_centroids <- sf::st_centroid(
  top_decile_cells
)

# Extract projected coordinates for DBSCAN
cluster_coordinates <- sf::st_coordinates(
  top_decile_centroids
)

# Group nearby high-catch cells using the original settings
# A 30 km neighbourhood and at least four cells are required
cluster_result <- dbscan::dbscan(
  cluster_coordinates,
  eps = 30000,
  minPts = 4
)

# Add the cluster number to each high-catch cell
top_decile_cells$hotspot_cluster <- cluster_result$cluster

# Remove isolated high-catch cells classified as noise
clustered_cells <- top_decile_cells |>
  dplyr::filter(
    hotspot_cluster > 0
  )

# Summarise the total reported catch within each cluster
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


# Load the detailed seabed-depth raster
hotspot_depth <- terra::rast(
  here::here(
    "data",
    "processed_data",
    "bathymetry",
    "seabed_depth_m.tif"
  )
)

# Use the original hotspot bathymetry colours
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


# Create a function for the two hotspot maps
create_hotspot_map <- function(cluster_number) {
  
  # Select the reporting cells belonging to this cluster
  selected_cells <- clustered_cells |>
    dplyr::filter(
      hotspot_cluster == cluster_number
    )
  
  # Find the projected extent of the selected cluster
  cluster_bbox <- sf::st_bbox(
    selected_cells
  )
  
  # Calculate the centre of the cluster
  cluster_centre_x <- mean(
    c(cluster_bbox["xmin"], cluster_bbox["xmax"])
  )
  
  cluster_centre_y <- mean(
    c(cluster_bbox["ymin"], cluster_bbox["ymax"])
  )
  
  # Calculate the width and height of the cluster
  cluster_width <- cluster_bbox["xmax"] - cluster_bbox["xmin"]
  cluster_height <- cluster_bbox["ymax"] - cluster_bbox["ymin"]
  
  # Add the same 50 km buffer used in Script 07
  half_width <- max(
    cluster_width,
    cluster_height
  ) / 2 + 50000
  
  # Create a square extent around the cluster
  inset_bbox <- sf::st_bbox(
    c(
      xmin = unname(cluster_centre_x - half_width),
      ymin = unname(cluster_centre_y - half_width),
      xmax = unname(cluster_centre_x + half_width),
      ymax = unname(cluster_centre_y + half_width)
    ),
    crs = sf::st_crs(selected_cells)
  )
  
  # Convert the inset extent to a spatial polygon
  inset_polygon <- sf::st_as_sfc(
    inset_bbox
  )
  
  # Convert the inset polygon to geographic coordinates
  inset_geographic <- sf::st_transform(
    inset_polygon,
    4326
  )
  
  # Get the geographic boundaries for Leaflet
  inset_extent <- sf::st_bbox(
    inset_geographic
  )
  
  # Store the boundaries for the interactive map
  inset_bounds <- c(
    west = unname(inset_extent["xmin"]),
    south = unname(inset_extent["ymin"]),
    east = unname(inset_extent["xmax"]),
    north = unname(inset_extent["ymax"])
  )
  
  # Select all reported catches within the inset
  # This follows the original Figure 1B/C maps
  inset_catches <- sf::st_crop(
    catch_projected,
    inset_bbox
  )
  
  # Calculate the catch centroids
  inset_catches <- sf::st_centroid(
    inset_catches
  )
  
  # Convert catch locations to geographic coordinates
  inset_catches <- sf::st_transform(
    inset_catches,
    4326
  )
  
  # Assign catch classes using the original breaks
  inset_catches <- inset_catches |>
    dplyr::mutate(
      catch_class = cut(
        roundWeight_kg,
        breaks = catch_breaks,
        labels = catch_labels,
        include.lowest = TRUE
      )
    )
  
  # Create popup information for each catch location
  inset_catches$popup_text <- paste0(
    "<b>Reporting hexagon:</b> ",
    inset_catches$hexGridID,
    "<br><b>Reported catch:</b> ",
    scales::comma(inset_catches$roundWeight_kg),
    " kg"
  )
  
  # Convert the inset extent to the bathymetry raster CRS
  raster_crop_polygon <- sf::st_transform(
    inset_polygon,
    terra::crs(hotspot_depth)
  )
  
  # Crop the bathymetry to the selected hotspot area
  inset_depth <- terra::crop(
    hotspot_depth,
    terra::ext(
      terra::vect(raster_crop_polygon)
    ),
    snap = "out"
  )
  
  # Reduce raster resolution for interactive display
  inset_factor <- max(
    1,
    ceiling(
      sqrt(
        terra::ncell(inset_depth) / 100000
      )
    )
  )
  
  # Aggregate the display raster
  inset_depth <- terra::aggregate(
    inset_depth,
    fact = inset_factor,
    fun = mean,
    na.rm = TRUE
  )
  
  # Convert the raster to geographic coordinates
  inset_depth <- terra::project(
    inset_depth,
    "EPSG:4326",
    method = "bilinear"
  )
  
  # Create the interactive hotspot map
  hotspot_map <- leaflet::leaflet(
    options = leaflet::leafletOptions(
      scrollWheelZoom = FALSE,
      maxZoom = 12,
      maxBoundsViscosity = 1
    )
  ) |>
    
    # Add an OpenStreetMap basemap
    leaflet::addTiles() |>
    
    # Add the detailed seabed-depth raster
    leaflet::addRasterImage(
      inset_depth,
      colors = hotspot_depth_colours,
      opacity = 0.95,
      project = FALSE,
      maxBytes = 20 * 1024 * 1024
    ) |>
    
    # Add all reported catches within the hotspot extent
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
    
    # Add the original catch-weight legend
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
      opacity = 0.95
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
    )
  
  # Apply the selected hotspot boundaries
  hotspot_map <- limit_map_view(
    hotspot_map,
    inset_bounds
  )
  
  # Return the interactive hotspot map
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

# Get the geographic extent of the temperature raster
temperature_raster_geographic <- terra::project(
  temperature_raster,
  "EPSG:4326",
  method = "bilinear"
)

# Get the raster boundaries
temperature_extent <- terra::ext(
  temperature_raster_geographic
)

# Keep the map within the available temperature data
temperature_bounds <- c(
  west = max(
    regional_view["west"],
    terra::xmin(temperature_extent)
  ),
  south = max(
    regional_view["south"],
    terra::ymin(temperature_extent)
  ),
  east = min(
    regional_view["east"],
    terra::xmax(temperature_extent)
  ),
  north = min(
    regional_view["north"],
    terra::ymax(temperature_extent)
  )
)

# Check that the selected area overlaps the temperature raster
if (
  temperature_bounds["west"] >= temperature_bounds["east"] ||
  temperature_bounds["south"] >= temperature_bounds["north"]
) {
  stop("The selected map extent does not overlap the temperature raster.")
}

# Crop the temperature raster to the selected study area
temperature_display <- terra::crop(
  temperature_raster_geographic,
  terra::ext(
    temperature_bounds["west"],
    temperature_bounds["east"],
    temperature_bounds["south"],
    temperature_bounds["north"]
  )
)

# Reduce raster resolution for interactive display
temperature_factor <- max(
  1,
  ceiling(
    sqrt(
      terra::ncell(temperature_display) / 150000
    )
  )
)

# Aggregate the temperature raster for display
temperature_display <- terra::aggregate(
  temperature_display,
  fact = temperature_factor,
  fun = mean,
  na.rm = TRUE
)

# Remove completely empty rows and columns
temperature_display <- terra::trim(
  temperature_display
)

# Find the minimum and maximum temperature
temperature_range <- as.numeric(
  terra::global(
    temperature_display,
    c("min", "max"),
    na.rm = TRUE
  )[1, ]
)

# Use the same temperature colours as Figure 2
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

# Get the final displayed temperature extent
temperature_extent <- terra::ext(
  temperature_display
)

# Store the map boundaries
temperature_bounds <- c(
  west = terra::xmin(temperature_extent),
  south = terra::ymin(temperature_extent),
  east = terra::xmax(temperature_extent),
  north = terra::ymax(temperature_extent)
)


# Create the interactive version of Figure 2
figure_2_interactive <- leaflet::leaflet(
  options = leaflet::leafletOptions(
    scrollWheelZoom = FALSE,
    maxZoom = 10,
    maxBoundsViscosity = 1
  )
) |>
  
  # Add an OpenStreetMap basemap
  leaflet::addTiles() |>
  
  # Add the five-year mean sea-surface temperature
  leaflet::addRasterImage(
    temperature_display,
    colors = temperature_colours,
    opacity = 0.95,
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
  
  # Add the catch-weight legend
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
    opacity = 0.95
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
  )

# Apply the temperature map boundaries
figure_2_interactive <- limit_map_view(
  figure_2_interactive,
  temperature_bounds
)

# Preview Figure 2 without saving
figure_2_interactive