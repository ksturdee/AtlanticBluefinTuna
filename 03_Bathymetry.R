# 03_Bathymetry.R

# Purpose: Process GEBCO 2026 bathymetry for the Atlantic bluefin tuna project.
# I will analyse seabed depth (m) to determine depths associated with tuna
# catch cells, and use the 200 m contour to examine how reported catches
# are distributed relative to the continental shelf boundary.

# Original GEBCO data are preserved without modification.

# Note about the dataset: GEBCO stores elevations relative to sea level.
# Negative values represent locations below sea level, and positive values
# represent locations above sea level. GEBCO is in WGS84.

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

# Retain only elevations below sea level. 
# Land and cells at or above sea level become NA
gebco_ocean_elevation <- terra::ifel(
  gebco_elevation < 0,
  gebco_elevation,
  NA
)

seabed_depth_m <- -gebco_ocean_elevation # convert to positive values (m)

# Assign a descriptive raster layer name
names(seabed_depth_m) <- "seabed_depth_m"

# Confirm the resulting depth range; check min, max and mean depth
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

# Preview raster
plot(
  seabed_depth_m,
  main = "GEBCO 2026: Seabed Depth (m)",
  col = bathymetry_colours,
  colNA = "#D9D9D9" # grey for missing cells
)

# Extract the 200 m seabed depth contour
depth_contour_200m <- terra::as.contour(
  seabed_depth_m,
  levels = 200
)

# Assign a descriptive layer name
names(depth_contour_200m) <- "depth_m"

print(depth_contour_200m)

# The 200 m contour may contain numerous separate line features because
# the continental shelf has a complex shape. These lines represent locations
# where seabed depth is approximately 200 m.

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

# The distance between tuna catch hexagons and this contour will be calculated
# in Script 05. This describes spatial proximity, not habitat preference.

# Export processed bathymetry for future analysis

# Define output directory
bathymetry_output_directory <- here::here(
  "data",
  "processed_data",
  "bathymetry"
)

# Create the directory if it does not already exist
dir.create(
  bathymetry_output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# Export seabed depth raster
terra::writeRaster(
  seabed_depth_m,
  filename = file.path(
    bathymetry_output_directory,
    "seabed_depth_m.tif"
  ),
  overwrite = TRUE
)


# Export 200 m depth contour as a GeoPackage
terra::writeVector(
  depth_contour_200m,
  filename = file.path(
    bathymetry_output_directory,
    "depth_contour_200m.gpkg"
  ),
  filetype = "GPKG",
  overwrite = TRUE
)

# The raster retains its original GEBCO grid.
# No reprojection or resampling is performed in this script.


