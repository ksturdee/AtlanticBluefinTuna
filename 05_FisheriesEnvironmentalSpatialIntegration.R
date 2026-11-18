# 05_FisheriesEnvironmentalSpatialIntegration.R

# The purpose of this script is to bring together the three datasets from
# scripts 02, 03 and 04. The DFO fisheries hexagons will remain the primary
# spatial units.

# Mean seabed depth and mean surface temperature will be calculated
# within each bluefin tuna reporting cell. As well, I will determine the shortest 
# distance from each catch cell to the 200m bathymetric contour.

# This script will run independently of the others. 

# Load required packages:
library(sf)
library(terra)
library(dplyr)
library(here)

# Define file paths for the data sources
fisheries_geodatabase_path <- here::here(
  "data",
  "raw_data",
  "DFO_FisheriesAtlas",
  "Maritimes_Fisheries_Atlas_2019_2023.gdb"
)
bathymetry_data_directory <- here::here(
  "data",
  "processed_data",
  "bathymetry"
)
oceanographic_data_directory <- here::here(
  "data",
  "processed_data",
  "oceanography"
)

# Import processed seabed depth
seabed_depth_m <- terra::rast(
  file.path(
    bathymetry_data_directory,
    "seabed_depth_m.tif"
  )
)

# Give the extracted values a clear final column name
names(seabed_depth_m) <- "mean_seabed_depth_m"

depth_contour_200m <- sf::st_read(
  file.path(
    bathymetry_data_directory,
    "depth_contour_200m.gpkg"
  ),
  quiet = TRUE
)

# Import processed mean surface temperature
mean_surface_temperature <- terra::rast(
  file.path(
    oceanographic_data_directory,
    "glorys_mean_surface_temperature_2019_2023.tif"
  )
)

# Confirm the final extracted column name
names(mean_surface_temperature) <- "mean_surface_temperature_c"

# Transform the fisheries polygons to the geographic CRS used by the
# environmental rasters.
bluefin_tuna_catch_cells <- bluefin_tuna_catch_data |>
  sf::st_transform(4326) |>
  dplyr::select(
    hexGridID,
    roundWeight_kg
  )

# Extract mean seabed depth. Calculate mean seabed depth across each reporting-cell
bluefin_tuna_bathymetry_values <- terra::extract(
  seabed_depth_m,
  terra::vect(bluefin_tuna_catch_cells),
  fun = mean,
  exact = TRUE,
  na.rm = TRUE
)
# Remove the extraction row identifier
bluefin_tuna_bathymetry_values <-
  bluefin_tuna_bathymetry_values |>
  dplyr::select(-ID)

# Extract mean surface temp
# Calculate mean five year surface temp across each cell using the processed GLORYS
# raster
bluefin_tuna_temperature_values <- terra::extract(
  mean_surface_temperature,
  terra::vect(bluefin_tuna_catch_cells),
  fun = mean,
  exact = TRUE,
  na.rm = TRUE
)

# Remove the extraction row identifier
bluefin_tuna_temperature_values <-
  bluefin_tuna_temperature_values |>
  dplyr::select(-ID)

# Combine catch and environmental attributes
bluefin_tuna_integrated_data <- bluefin_tuna_catch_cells |>
  dplyr::bind_cols(
    bluefin_tuna_bathymetry_values,
    bluefin_tuna_temperature_values
  )

# Confirm final attribute names
print(names(bluefin_tuna_integrated_data))


# Distance to the 200 m contour; calculate the shortest distance (m) from the 
# hexagon boundary to the contour.

# Use a metre-based projected CRS for distance calculations
distance_calculation_crs <- 3347

# Transform bluefin tuna catch hexagons
bluefin_tuna_cells_projected <- sf::st_transform(
  bluefin_tuna_integrated_data,
  crs = distance_calculation_crs
)

# Transform the 200 m contour
depth_contour_200m_projected <- sf::st_transform(
  depth_contour_200m,
  crs = distance_calculation_crs
)

# Combine contour features into one geometry
combined_depth_contour_200m <- sf::st_union(
  sf::st_geometry(depth_contour_200m_projected)
)

# Calculate minimum distance from each catch hexagon to the contour
distance_to_200m_contour_m <- sf::st_distance(
  sf::st_geometry(bluefin_tuna_cells_projected),
  combined_depth_contour_200m
)

# Add distance in kilometres to the integrated dataset
bluefin_tuna_integrated_data$distance_to_200m_contour_km <-
  as.numeric(distance_to_200m_contour_m) / 1000


# Summarise distances
distance_to_200m_contour_summary <- bluefin_tuna_integrated_data |>
  sf::st_drop_geometry() |>
  dplyr::summarise(
    minimum_distance_km = min(distance_to_200m_contour_km),
    median_distance_km = median(distance_to_200m_contour_km),
    mean_distance_km = mean(distance_to_200m_contour_km),
    maximum_distance_km = max(distance_to_200m_contour_km)
  )

print(distance_to_200m_contour_summary)

# Reported Atlantic bluefin tuna catch cells were distributed across a broad 
# range of distances from the 200 m bathymetric contour. The median distance 
# was 17.0 km, compared with a mean of 38.2 km, indicating a right-skewed 
# distribution. Some catch cells intersected the contour, while the maximum 
# recorded distance was 326.1 km. These results describe the spatial proximity 
# of reported commercial catches to 200 m depth boundaries, rather than 
# demonstrating a preference for shelf-edge habitat.

# Define output directory
integrated_data_directory <- here::here(
  "data",
  "processed_data",
  "spatial_integration"
)


# Define output file paths
integrated_geopackage_path <- file.path(
  integrated_data_directory,
  "bluefin_tuna_fisheries_environment_2019_2023.gpkg"
)

integrated_csv_path <- file.path(
  integrated_data_directory,
  "bluefin_tuna_fisheries_environment_2019_2023.csv"
)

# Export GeoPackage with original hexagon geometry
sf::st_write(
  bluefin_tuna_integrated_data,
  dsn = integrated_geopackage_path,
  layer = "bluefin_tuna_environment",
  delete_dsn = TRUE,
  quiet = TRUE
)

# Export non-spatial attributes as CSV
integrated_attribute_data <- bluefin_tuna_integrated_data |>
  sf::st_drop_geometry()

# Save a portable non-spatial copy of the integrated attributes
utils::write.csv(
  integrated_attribute_data,
  file = integrated_csv_path,
  row.names = FALSE,
  na = ""
)



