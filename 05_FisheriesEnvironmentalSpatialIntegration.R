# 05_FisheriesEnvironmentalSpatialIntegration.R

# The purpose of this script is to bring together the three datasets from
# scripts 02, 03 and 04. The DFO fisheries hexagons will remain the primary
# spatial units. Note: Catch weight is not a direct measure of fish abundance, 
# and unreported cells must not be interpreted as zero catch in this analysis.

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

# Import the fisheries data from the geodatabase
selected_species_layers <- c(
  bluefin_tuna = "BluefinTuna_Landings_Weight_2019_2023",
  atlantic_herring = "Herring_Landings_Weight_2019_2023",
  atlantic_mackerel = "Mackerel_Landings_Weight_2019_2023",
  squid = "Squid_Landings_Weight_2019_2023",
  white_hake = "WhiteHake_Landings_Weight_2019_2023",
  silver_hake = "SilverHake_Landings_Weight_2019_2023",
  red_hake = "RedHake_Landings_Weight_2019_2023",
  cusk = "Cusk_Landings_Weight_2019_2023"
)

fisheries_species_data <- lapply(
  selected_species_layers,
  function(layer_name) {
    sf::st_read(
      fisheries_geodatabase_path,
      layer = layer_name,
      quiet = TRUE
    )
  }
)

# Import processed bathymetric layers
seabed_depth_m <- terra::rast(
  file.path(
    bathymetry_data_directory,
    "seabed_depth_m.tif"
  )
)
seafloor_slope_degrees <- terra::rast(
  file.path(
    bathymetry_data_directory,
    "seafloor_slope_degrees.tif"
  )
)
local_depth_variability_m <- terra::rast(
  file.path(
    bathymetry_data_directory,
    "local_depth_variability_m.tif"
  )
)
names(local_depth_variability_m) <- "local_depth_variability_m"

depth_contour_200m <- sf::st_read(
  file.path(
    bathymetry_data_directory,
    "depth_contour_200m.gpkg"
  ),
  quiet = TRUE
)

# Import processed oceanographic (Copernicus) layers
mean_oceanographic_conditions <- terra::rast(
  file.path(
    oceanographic_data_directory,
    "glorys_mean_oceanographic_conditions_2019_2023.tif"
  )
)

# Extract individual environmental variables
mean_surface_temperature <-
  mean_oceanographic_conditions[["mean_surface_temperature_c"]]

mean_surface_current_speed <-
  mean_oceanographic_conditions[["mean_current_speed_ms"]]

mean_eastward_surface_velocity <-
  mean_oceanographic_conditions[["mean_eastward_velocity_ms"]]

mean_northward_surface_velocity <-
  mean_oceanographic_conditions[["mean_northward_velocity_ms"]]

# Confirm all fisheries datasets have defined coordinate systems
stopifnot(
  all(vapply(
    fisheries_species_data,
    function(species_data) {
      !is.na(sf::st_crs(species_data))
    },
    logical(1)
  ))
)

# Confirm unique grid identifiers within each species
stopifnot(
  all(vapply(
    fisheries_species_data,
    function(species_data) {
      !anyDuplicated(species_data$hexGridID)
    },
    logical(1)
  ))
)

# Transform fisheries polygons to geographic coordinates
fisheries_species_data_geographic <- lapply(
  fisheries_species_data,
  sf::st_transform,
  crs = 4326
)

# -----------------------------------------------------------------------------
# Select original bluefin tuna catch polygons
bluefin_tuna_catch_cells <-
  fisheries_species_data_geographic$bluefin_tuna |>
  dplyr::select(
    hexGridID,
    roundWeight_kg
  )

#  Combine bathymetric variables
bathymetric_environmental_layers <- c(
  seabed_depth_m,
  seafloor_slope_degrees,
  local_depth_variability_m
)
# Confirm descriptive layer names
names(bathymetric_environmental_layers) <- c(
  "mean_seabed_depth_m",
  "mean_seafloor_slope_degrees",
  "mean_local_depth_variability_m"
)

# Extract bathymetric conditions
bluefin_tuna_bathymetry_values <- terra::extract(
  bathymetric_environmental_layers,
  terra::vect(bluefin_tuna_catch_cells),
  fun = mean,
  exact = TRUE,
  na.rm = TRUE
)

# Remove extraction row identifier
bluefin_tuna_bathymetry_values <- bluefin_tuna_bathymetry_values |>
  dplyr::select(-ID)

# Extract oceanographic conditions
bluefin_tuna_oceanographic_values <- terra::extract(
  mean_oceanographic_conditions,
  terra::vect(bluefin_tuna_catch_cells),
  fun = mean,
  exact = TRUE,
  na.rm = TRUE
)

# Remove extraction row identifier
bluefin_tuna_oceanographic_values <- bluefin_tuna_oceanographic_values |>
  dplyr::select(-ID)

#  Combine catch and environmental attributes
bluefin_tuna_environmental_data <- bluefin_tuna_catch_cells |>
  dplyr::bind_cols(
    bluefin_tuna_bathymetry_values,
    bluefin_tuna_oceanographic_values
  )
print(names(bluefin_tuna_environmental_data))

# Integrate potential prey catch layers usign hexGridID

potential_forage_species <- c(
  "atlantic_herring",
  "atlantic_mackerel",
  "squid",
  "white_hake",
  "silver_hake",
  "red_hake",
  "cusk"
)

potential_forage_catch_columns <- c(
  atlantic_herring = "herring_catch_kg",
  atlantic_mackerel = "mackerel_catch_kg",
  squid = "squid_catch_kg",
  white_hake = "white_hake_catch_kg",
  silver_hake = "silver_hake_catch_kg",
  red_hake = "red_hake_catch_kg",
  cusk = "cusk_catch_kg"
)

# Prepare catch attributes for each comparison species
potential_forage_catch_data <- lapply(
  potential_forage_species,
  function(species_name) {
    
    comparison_catch_data <- fisheries_species_data[[species_name]] |>
      sf::st_drop_geometry() |>
      dplyr::select(
        hexGridID,
        roundWeight_kg
      )
    
    names(comparison_catch_data)[
      names(comparison_catch_data) == "roundWeight_kg"
    ] <- potential_forage_catch_columns[[species_name]]
    
    comparison_catch_data
  }
)

names(potential_forage_catch_data) <- potential_forage_species

# Start with the environmental bluefin dataset

bluefin_tuna_integrated_data <- bluefin_tuna_environmental_data
# Add comparison-species catch values by shared fisheries grid ID
for (species_name in potential_forage_species) {
  
  bluefin_tuna_integrated_data <- dplyr::left_join(
    bluefin_tuna_integrated_data,
    potential_forage_catch_data[[species_name]],
    by = "hexGridID"
  )
}
# Summarise exact grid-cell overlap
potential_forage_overlap_summary <- tibble::tibble(
  species = potential_forage_species,
  
  bluefin_cells_with_matching_catch = vapply(
    potential_forage_species,
    function(species_name) {
      
      catch_column <-
        potential_forage_catch_columns[[species_name]]
      
      sum(
        !is.na(
          bluefin_tuna_integrated_data[[catch_column]]
        )
      )
    },
    integer(1)
  ),
  
  bluefin_cells_without_matching_catch = vapply(
    potential_forage_species,
    function(species_name) {
      
      catch_column <-
        potential_forage_catch_columns[[species_name]]
      
      sum(
        is.na(
          bluefin_tuna_integrated_data[[catch_column]]
        )
      )
    },
    integer(1)
  )
) |>
  dplyr::mutate(
    overlap_percentage =
      100 *
      bluefin_cells_with_matching_catch /
      nrow(bluefin_tuna_integrated_data)
  )

print(potential_forage_overlap_summary)

# Confirm final attribute names
print(names(bluefin_tuna_integrated_data))

print(
  potential_forage_overlap_summary,
  width = Inf
)
# Overlap is not 100% for any species, which is expected given the different 
# spatial distributions of the species and the fact that not all species are 
# caught in every hexagon.

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

# Confirm all distances are valid
stopifnot(
  all(
    is.finite(
      bluefin_tuna_integrated_data$distance_to_200m_contour_km
    )
  ),
  all(
    bluefin_tuna_integrated_data$distance_to_200m_contour_km >= 0
  )
)

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

utils::write.csv(
  integrated_attribute_data,
  file = integrated_csv_path,
  row.names = FALSE,
  na = ""
)



