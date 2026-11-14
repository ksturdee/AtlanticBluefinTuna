# 03_Bathymetry.R

# Purpose:
# Prepare bathymetric data for investigating relationships between seafloor 
# characteristics and reported Atlantic bluefin tuna catch distributions.

# This script runs independently of scripts 01 and 02.
# Original GEBCO data are preserved without modification.

# I will analyze depth (m) by determining depths associated with tuna catch cells,
# seafloor slope (°) by identifying if catches occur near steep underwater terrain,
# distance to 200 m contour (km) will reveal associations with continental shelf
# boundaries, and bathymetric variability to characterise local seafloor compelxity.

# This script will import the raw GEBCO GeoTIFF using terra, seperate ocean 
# bathymetry from land elevations, calculate seafloor slope, identify 200 m depth
# contour, extract bathymetric variables for bluefin tuna catch hexagons, and 
# produce descriptive statistics and visualisatiosn. Note: seabed depth is an
# environmental characteristic of their catch locations NOT the depth that they 
# are swimming at.

# Note about the dataset: GEBCO stores elevations relative to sea level. Therefore,
# Negative values represent locations below sea level, and positive values 
# represent locations above sea level. Also, GEBCO is in WGS84.


# Load required packages
library(terra)
library(here)

# Define data location for GEBCO bathymetry GeoTIFF
gebco_bathymetry_path <- here::here(
  "data",
  "raw_data",
  "GEBCO",
  "gebco_2026_n48.0_s39.0_w-68.0_e-55.0_geotiff.tif"
)

# Import original GEBCO bathymetry raster using terra
gebco_elevation <- terra::rast(
  gebco_bathymetry_path
)
print(gebco_elevation) # Check raster properties

# Retain only elevations below sea level. Land and cells at or above sea level become NA
gebco_ocean_elevation <- terra::ifel(
  gebco_elevation < 0,
  gebco_elevation,
  NA
)
seabed_depth_m <- -gebco_ocean_elevation # convert to positive values (m)

# Assign a descriptive raster layer name
names(seabed_depth_m) <- "seabed_depth_m"

# Confirm the resulting depth range
seabed_depth_summary <- terra::global(
  seabed_depth_m,
  fun = c("min", "max", "mean"),
  na.rm = TRUE
)
print(seabed_depth_summary) # min 1, max 5429, mean 2257.5 m


# Visualise processed seabed depth

# Bathymetry colour palette
bathymetry_colours <- colorRampPalette(
  c(
    "#7FDBDA",  # Shallow water: turquoise
    "#41B6C4",  # Continental shelf
    "#2C7FB8",  # Continental slope
    "#253494",  # Deep ocean
    "#081D58"   # Abyssal depths
  )
)(100)

plot(
  seabed_depth_m,
  main = "GEBCO 2026: Seabed Depth (m)",
  col = bathymetry_colours,
  colNA = "#D9D9D9"
)

# Calculate slope using the original GEBCO elevation raster.
# terra accounts for geographic coordinates when calculating terrain slope.
# Using the continuous elevation surface avoids artificial gradients
# introduced by masking land before calculating slope.

seafloor_slope_degrees <- terra::terrain(
  gebco_elevation,
  v = "slope",
  unit = "degrees",
  neighbors = 8
)

# Retain slope values only where seabed depth is positive
seafloor_slope_degrees <- terra::ifel(
  !is.na(seabed_depth_m),
  seafloor_slope_degrees,
  NA
)

# Assign a descriptive layer name
names(seafloor_slope_degrees) <- "seafloor_slope_degrees"

# Summarise calculated slope values
seafloor_slope_summary <- terra::global(
  seafloor_slope_degrees,
  fun = c("min", "max", "mean"),
  na.rm = TRUE
)

print(seafloor_slope_summary) # min 0, max 49.27, mean 1.06

# Confirm slope and depth rasters share the same geometry
stopifnot(
  terra::compareGeom(
    seabed_depth_m,
    seafloor_slope_degrees,
    stopOnError = FALSE
  )
)

# Colour palette for seafloor slope
slope_colours <- colorRampPalette(
  c(
    "#F7FBFF",  # Very gentle slope
    "#C6DBEF",
    "#6BAED6",
    "#2171B5",
    "#08306B"   # Steep slope
  )
)(100)

# Plot seafloor slope
plot(
  seafloor_slope_degrees,
  main = "GEBCO 2026: Seafloor Slope (degrees)",
  col = slope_colours,
  colNA = "#D9D9D9",
  range = c(0, 15)
)

# Extract the 200 m depth contour from the seabed depth raster. This contour 
# represents the boundary of the continental shelf. It gives us a spatial reference
# for seeing if bluefin tuna catches are associated with the shelf break.

# Extract the 200 m seabed depth contour
depth_contour_200m <- terra::as.contour(
  seabed_depth_m,
  levels = 200
)

# Assign a descriptive layer name
names(depth_contour_200m) <- "depth_m"
print(depth_contour_200m)

# Plot bathymetry with the 200 m contour
plot(
  seabed_depth_m,
  main = "GEBCO 2026: 200 m Bathymetric Contour",
  col = bathymetry_colours,
  colNA = "#D9D9D9"
)

lines(
  depth_contour_200m,
  col = "#C0392B", # Red for visibility
  lwd = 1.5
)
# There are numerous small polygons in the 200 m contour, which is expected 
# given the complex nature of the continental shelf. The contour lines represent 
# areas where the seabed depth is exactly 200 m, and they can be used to assess 
# the proximity of bluefin tuna catches to the continental shelf edge.

# Local bathymetry variability; identify areas of stable vs. complex seafloor 
# terrain. This is done by calculating the standard deviation of seabed depth 
# within a moving window (focal operation). A 5x5 cell window is used, 
# which corresponds to approximately 5 km x 5 km given the GEBCO resolution.
# Low values = relatively uniform seabed depth. High values = strong local changes.

# Define a 5 x 5 moving window
bathymetry_focal_window <- matrix(
  1,
  nrow = 5,
  ncol = 5
)

# Calculate the standard deviation of seabed depth
local_depth_variability_m <- terra::focal(
  seabed_depth_m,
  w = bathymetry_focal_window,
  fun = "sd",
  na.rm = TRUE
)

# Preserve ocean-only coverage
local_depth_variability_m <- terra::ifel(
  !is.na(seabed_depth_m),
  local_depth_variability_m,
  NA
)

# Assign descriptive layer name
names(local_depth_variability_m) <- "local_depth_variability_m"

# Summarise bathymetric variability
local_depth_variability_summary <- terra::global(
  local_depth_variability_m,
  fun = c("min", "max", "mean"),
  na.rm = TRUE
)
print(local_depth_variability_summary) # Min 0 m, max 410.7 m, mean 10.6 m

# Colour palette for bathymetric variability
bathymetry_variability_colours <- colorRampPalette(
  c(
    "#F7FCF0",  # Very low variability
    "#C7E9C0",
    "#74C476",
    "#238B45",
    "#00441B"   # High variability
  )
)(100)

# Plot local bathymetric variability
plot(
  local_depth_variability_m,
  main = "GEBCO 2026: Local Bathymetric Variability (m)",
  col = bathymetry_variability_colours,
  colNA = "#D9D9D9"
)

# As expect, the Scotian shelf appears to have relatively low variability, while
# the continental slope and areas of the deep ocean show higher variability, and
# deep ocean plains stabilize (low variability).

# Export processed bathymetric rasters for future analysis.

# Define output directory
bathymetry_output_directory <- here::here(
  "data",
  "processed_data",
  "bathymetry"
)

# Seabed depth raster export
terra::writeRaster(
  seabed_depth_m,
  filename = file.path(
    bathymetry_output_directory,
    "seabed_depth_m.tif"
  ),
  overwrite = TRUE
)

# Seafloor slope raster export
terra::writeRaster(
  seafloor_slope_degrees,
  filename = file.path(
    bathymetry_output_directory,
    "seafloor_slope_degrees.tif"
  ),
  overwrite = TRUE
)

# Local bathymetric variability raster export
terra::writeRaster(
  local_depth_variability_m,
  filename = file.path(
    bathymetry_output_directory,
    "local_depth_variability_m.tif"
  ),
  overwrite = TRUE
)

# Export 200 m depth contour as a shapefile
terra::writeVector(
  depth_contour_200m,
  filename = file.path(
    bathymetry_output_directory,
    "depth_contour_200m.gpkg"
  ),
  filetype = "GPKG",
  overwrite = TRUE
)

# At this point I have not reprojected or resampled the rasters.



