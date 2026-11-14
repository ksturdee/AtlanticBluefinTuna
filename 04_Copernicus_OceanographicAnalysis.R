# 04_Copernicus_OceanographicAnalysis.R

# The purpose of this script is to process monthly ocean temperature and 
# surface-current data for the Scotian Shelf and adjacent Atlantic waters.
# Source: Copernicus Marine GLORYS12V1
# Reporting period: January 2019 - December 2023
#
# Variables:
# thetao = Sea water potential temperature (degrees Celsius)
# uo     = Eastward sea water velocity (m/s)
# vo     = Northward sea water velocity (m/s)
#
# This script runs independently of scripts 01, 02 and 03.
# Original NetCDF data are preserved without modification.

# Package setup
library(ggplot2)
library(metR)
library(ggspatial)
library(sf)
library(dplyr)

# Define original NetCDF location
copernicus_netcdf_path <- here::here(
  "data",
  "raw_data",
  "Copernicus",
  "cmems_mod_glo_phy_my_0.083deg_P1M-m_1794404831682.nc"
)

# Import variables
copernicus_oceanographic_data <- terra::sds(
  copernicus_netcdf_path
)

sea_surface_temperature <- copernicus_oceanographic_data["thetao"]
eastward_surface_velocity <- copernicus_oceanographic_data["uo"]
northward_surface_velocity <- copernicus_oceanographic_data["vo"]
print(copernicus_oceanographic_data)

# Note these are monthly modelled conditions not direct observations at tuna 
# locations. Also, the DFO fisheries data are aggregated over 2019-2023. We can 
# examine seasonal oceanographic patterns, but we cannot directly associate a 
# particular month's conditions with tuna catches without monthly catch records.

# We will calculate the surface current spead combining uo and vo using the 
# Pythagorean theorem: current_speed = sqrt(uo^2 + vo^2). This creates 60 monthly
# current-speed rasters. 

# Calculate current speed from eastward and northward velocity
surface_current_speed <- sqrt(
  eastward_surface_velocity^2 +
    northward_surface_velocity^2
)

# Preserve monthly timestamps
terra::time(surface_current_speed) <-
  terra::time(eastward_surface_velocity)

# Assign descriptive layer names
names(surface_current_speed) <- paste0(
  "current_speed_",
  format(terra::time(surface_current_speed), "%Y_%m")
)

# Summarise current speed across all months
surface_current_speed_summary <- terra::global(
  surface_current_speed,
  fun = c("min", "max", "mean"),
  na.rm = TRUE
)

print(head(surface_current_speed_summary))

# Visual inspection of the first month's current speed
# Define warm colour palette for ocean current speed
current_speed_colours <- grDevices::colorRampPalette(
  c(
    "#FFFFCC",  # Very low speed: pale yellow
    "#FED976",  # Low speed: golden yellow
    "#FD8D3C",  # Moderate speed: orange
    "#F03B20",  # High speed: red-orange
    "#800026"   # Very high speed: burgundy
  )
)(100)

# Select January 2019
january_2019_current_speed <- surface_current_speed[[1]]

# Plot current speed
terra::plot(
  january_2019_current_speed,
  main = "GLORYS: Surface Current Speed (January 2019)",
  col = current_speed_colours,
  colNA = "#D9D9D9"
)
# Strong offshore currents reach approximately 1.2 m/s, particularly between 39–41°N

# This snapshot shows the surface current speed for January 2019.
# The meandering high-velocity band in the southern part of the map is 
# consistent with the Gulf Stream system. The Scotian shelf is showing relatively 
# low current speeds, while the continental slope and deep ocean areas show 
# slightly higher speeds.

# Calculate mean sea surface temperature (2019-2023)
mean_surface_temperature <- terra::app(
  sea_surface_temperature,
  fun = mean,
  na.rm = TRUE
)
names(mean_surface_temperature) <- "mean_surface_temperature_c"

# Calculate mean surface current speed (2019-2023)
mean_surface_current_speed <- terra::app(
  surface_current_speed,
  fun = mean,
  na.rm = TRUE
)
names(mean_surface_current_speed) <- "mean_current_speed_ms"

# Calculate mean eastward surface velocity
mean_eastward_surface_velocity <- terra::app(
  eastward_surface_velocity,
  fun = mean,
  na.rm = TRUE
)
names(mean_eastward_surface_velocity) <- "mean_eastward_velocity_ms"

# Calculate mean northward surface velocity
mean_northward_surface_velocity <- terra::app(
  northward_surface_velocity,
  fun = mean,
  na.rm = TRUE
)
names(mean_northward_surface_velocity) <- "mean_northward_velocity_ms"

# Summary of results:
oceanographic_mean_layers <- c(
  mean_surface_temperature,
  mean_surface_current_speed,
  mean_eastward_surface_velocity,
  mean_northward_surface_velocity
)

oceanographic_summary <- terra::global(
  oceanographic_mean_layers,
  fun = c("min", "max", "mean"),
  na.rm = TRUE
)

print(oceanographic_summary)

# Five-Year Mean Oceanographic Conditions
# Temperature colour palette: Cool blue to warm red
temperature_colours <- grDevices::colorRampPalette(
  c(
    "#2166AC",  # Cold: deep blue
    "#67A9CF",  # Cool: light blue
    "#D1E5F0",  # Moderately cool
    "#F7F7F7",  # Intermediate: near white
    "#FDDBC7",  # Moderately warm
    "#EF8A62",  # Warm: orange-red
    "#B2182B"   # Hot: deep red
  )
)(100)

# Plot mean surface temperature
terra::plot(
  mean_surface_temperature,
  main = "GLORYS: Mean Surface Temperature (2019–2023)",
  col = temperature_colours,
  colNA = "#D9D9D9"
)
# Cooler waters dominate the northern Scotian Shelf and coastal regions.
# Warmer waters occur farther offshore toward the south. There is a pronounced 
# thermal transition is visible around 41-43°N

# Mean surface current speed
terra::plot(
  mean_surface_current_speed,
  main = "GLORYS: Mean Surface Current Speed (2019-2023)",
  col = current_speed_colours
)
# Stronger mean currents remain concentrated offshore. The five-year average 
# produces a smoother pattern than the January 2019 map.

# ------------------------------------------------------------------------------
# Mean Surface Temperature and Current Streamlines
# Combine the five-year mean oceanographic variables
mean_oceanographic_layers <- c(
  mean_surface_temperature,
  mean_eastward_surface_velocity,
  mean_northward_surface_velocity
)

# Convert raster layers into a tidy data frame
oceanographic_mapping_data <- terra::as.data.frame(
  mean_oceanographic_layers,
  xy = TRUE,
  na.rm = FALSE
)

# Assign descriptive column names
names(oceanographic_mapping_data) <- c(
  "longitude",
  "latitude",
  "temperature_c",
  "eastward_velocity_ms",
  "northward_velocity_ms"
)

# Adjust eastward velocity for longitude convergence
# This provides an approximate geographic plotting correction
oceanographic_mapping_data <- oceanographic_mapping_data |>
  dplyr::mutate(
    eastward_plot_component =
      eastward_velocity_ms / cos(latitude * pi / 180)
  )

# Create streamline map
mean_temperature_streamline_map <- ggplot() +
  
  geom_raster(
    data = oceanographic_mapping_data,
    aes(
      x = longitude,
      y = latitude,
      fill = temperature_c
    )
  ) +
  
  metR::geom_streamline(
    data = oceanographic_mapping_data,
    aes(
      x = longitude,
      y = latitude,
      dx = eastward_plot_component,
      dy = northward_velocity_ms
    ),
    colour = "grey15",
    linewidth = 0.3,
    alpha = 0.65,
    L = 4,
    res = 1,
    skip = 3,
    arrow.length = 0.25
  ) +
  
  scale_fill_gradientn(
    colours = temperature_colours,
    name = "Temperature (°C)",
    na.value = "#D9D9D9"
  ) +
  
  coord_quickmap(
    xlim = c(-68, -55),
    ylim = c(39, 48),
    expand = FALSE
  ) +
  
  labs(
    title = "Surface Temperature and Ocean Circulation",
    subtitle = "Scotian Shelf and adjacent Atlantic waters | 2019–2023",
    x = "Longitude",
    y = "Latitude",
    caption = paste(
      "Source: Copernicus Marine GLORYS.",
      "Streamlines represent five-year mean surface circulation."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "grey35"
    ),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.2
    ),
    plot.caption = element_text(
      size = 9,
      colour = "grey40"
    )
  )

# Display streamline map
mean_temperature_streamline_map

# NOTE: this is a visualisation using an approximate geographic correction for 
# eastward velocity. The Copernicus data are modelled in a regular grid with 
# 0.083° resolution, which does not account for the convergence of meridians 
# at higher latitudes. The eastward velocity component is adjusted by dividing 
# by the cosine of latitude to provide a more accurate representation of 
# east-west movement on a geographic map projection. Also, because the currents 
# are averaged over five years, many short-lived eddies will be smoothed out. 
# The streamlines cannot recover those missing features.

# -----------------------------------------------------------------------------

# Combine mean eastward and northward velocity components
mean_surface_velocity_components <- c(
  mean_eastward_surface_velocity,
  mean_northward_surface_velocity
)

# Define spacing between current vectors
current_vector_sampling_interval <- 6

# Generate regularly spaced vector locations
current_vector_locations <- terra::as.points(
  terra::aggregate(
    mean_surface_temperature,
    fact = current_vector_sampling_interval,
    fun = mean,
    na.rm = TRUE
  ),
  na.rm = TRUE
)

# Extract velocity components at sampled locations
current_vector_values <- terra::extract(
  mean_surface_velocity_components,
  current_vector_locations
)

# Extract geographic coordinates
current_vector_coordinates <- terra::crds(
  current_vector_locations
)

# Construct tidy vector dataset
current_vector_data <- data.frame(
  longitude = current_vector_coordinates[, 1],
  latitude = current_vector_coordinates[, 2],
  eastward_velocity_ms =
    current_vector_values$mean_eastward_velocity_ms,
  northward_velocity_ms =
    current_vector_values$mean_northward_velocity_ms
)

# Remove incomplete observations
current_vector_data <- current_vector_data[
  stats::complete.cases(current_vector_data),
]

# Calculate current magnitude
current_vector_data$current_speed_ms <- sqrt(
  current_vector_data$eastward_velocity_ms^2 +
    current_vector_data$northward_velocity_ms^2
)

# Scale arrows for geographic visualisation
current_vector_scale <- 1.5

# Adjust eastward component for longitude convergence
current_vector_data$longitude_end <-
  current_vector_data$longitude +
  (
    current_vector_data$eastward_velocity_ms /
      cos(current_vector_data$latitude * pi / 180)
  ) * current_vector_scale

current_vector_data$latitude_end <-
  current_vector_data$latitude +
  current_vector_data$northward_velocity_ms *
  current_vector_scale

# Plot mean surface temperature and directional currents

terra::plot(
  mean_surface_temperature,
  main = "GLORYS: Mean Surface Temperature and Currents (2019–2023)",
  col = temperature_colours,
  colNA = "#D9D9D9"
)

graphics::arrows(
  x0 = current_vector_data$longitude,
  y0 = current_vector_data$latitude,
  x1 = current_vector_data$longitude_end,
  y1 = current_vector_data$latitude_end,
  length = 0.05,
  angle = 20,
  col = "grey20",
  lwd = 0.8
)

# The arrows are scaled for display in longitude–latitude coordinates, not for 
# measuring current displacement.





# Save analysis-ready GLORYS layers for integration with the DFO fisheries and 
# GEBCO bathymetry datasets.

# Define output directory
oceanographic_output_directory <- here::here(
  "data",
  "processed_data",
  "oceanography"
)


# Export five-year mean oceanographic variables
# Combine mean environmental variables
oceanographic_mean_layers <- c(
  mean_surface_temperature,
  mean_surface_current_speed,
  mean_eastward_surface_velocity,
  mean_northward_surface_velocity
)

# Save mean oceanographic variables
terra::writeRaster(
  oceanographic_mean_layers,
  filename = file.path(
    oceanographic_output_directory,
    "glorys_mean_oceanographic_conditions_2019_2023.tif"
  ),
  overwrite = TRUE
)


# Export monthly surface current speed

terra::writeRaster(
  surface_current_speed,
  filename = file.path(
    oceanographic_output_directory,
    "glorys_monthly_surface_current_speed_2019_2023.tif"
  ),
  overwrite = TRUE
)

# 4. Verify required exports

expected_oceanographic_files <- file.path(
  oceanographic_output_directory,
  c(
    "glorys_mean_oceanographic_conditions_2019_2023.tif",
    "glorys_monthly_surface_current_speed_2019_2023.tif"
  )
)
