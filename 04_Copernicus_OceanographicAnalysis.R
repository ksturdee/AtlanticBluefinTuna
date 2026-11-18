# 04_Copernicus_OceanographicAnalysis.R

# The purpose of this script is to process monthly ocean temperature and 
# surface-current data for the Scotian Shelf and adjacent Atlantic waters.
# Source: Copernicus Marine GLORYS12V1
# Reporting period: January 2019 - December 2023. 

# Variable:
# thetao = Sea water potential temperature (degrees Celsius)

# This script runs independently of scripts 01, 02 and 03.
# Original NetCDF data are preserved without modification.

# Package setup
library(terra)
library(here)

# Define original NetCDF location
copernicus_netcdf_path <- here::here(
  "data",
  "raw_data",
  "Copernicus",
  "cmems_mod_glo_phy_my_0.083deg_P1M-m_1794404831682.nc"
)

# Import and extract temperature data
copernicus_oceanographic_data <- terra::sds(
  copernicus_netcdf_path
)

sea_surface_temperature <- copernicus_oceanographic_data["thetao"]

# Note these are monthly modeled conditions not direct observations at tuna 
# locations. Also, the DFO fisheries data are aggregated over 2019-2023. We can 
# examine seasonal oceanographic patterns, but we cannot directly associate a 
# particular month's conditions with tuna catches without monthly catch records.

# Calculate mean sea surface temperature (2019-2023)
mean_surface_temperature <- terra::app(
  sea_surface_temperature,
  fun = mean,
  na.rm = TRUE
)
names(mean_surface_temperature) <- "mean_surface_temperature_c"

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

# Save analysis-ready GLORYS layers for integration with the DFO fisheries and 
# GEBCO bathymetry datasets.

# Define output directory
oceanographic_output_directory <- here::here(
  "data",
  "processed_data",
  "oceanography"
)

# Export five-year mean surface temperature
mean_temperature_output_path <- file.path(
  oceanographic_output_directory,
  "glorys_mean_surface_temperature_2019_2023.tif"
)
terra::writeRaster(
  mean_surface_temperature,
  filename = mean_temperature_output_path,
  overwrite = TRUE
)
