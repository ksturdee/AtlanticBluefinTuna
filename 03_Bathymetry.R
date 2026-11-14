# 03_Bathymetry.R

# Investigating the seafloor characteristics associated with bluefin tuna catch
# locations using the GEBCO 2026 bathymetry dataset. Specifically, I'm interested
# in how seafloor depth and topographic structure relate to reported catch hotspots.

# Will analyze depth (m) by determining depths associated with tuna catch cells,
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