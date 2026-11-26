# 09_ePortfolio_Content.R
# Creating interactive content for my Atlantic bluefin tuna ePortfolio


# List the packages needed for making maps and graphs:
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(htmlwidgets)

# Set relative paths to the existing project folders
data_dir <- "data"        # Original and processed datasets
figures_dir <- "figures"  # Figures from the scientific analysis
outputs_dir <- "outputs"  # Other analysis outputs


# Choose consistent colours for the ePortfolio
colours <- list(
  navy = "#102A43",        # Dark blue for headings and backgrounds
  ocean = "#176B91",       # Ocean blue for maps
  turquoise = "#24A6A6",   # Turquoise for highlights
  light_blue = "#D9EEF2",  # Light blue for supporting information
  coral = "#F47758",       # Coral for catch hotspots
  white = "#FFFFFF",       # White for backgrounds
  grey = "#566573"         # Grey for labels and supporting text
)


# Create the starting map for the ePortfolio
portfolio_map <- leaflet(
  
  # Allow visitors to zoom using the map controls
  options = leafletOptions(
    zoomControl = TRUE,
    
    # Stop the map zooming accidentally when scrolling the webpage
    scrollWheelZoom = FALSE
  )
  
) |>
  
  # Add a simple basemap so the data will be easy to see
  addProviderTiles(
    providers$CartoDB.Positron
  ) |>
  
  # Centre the map on Atlantic Canada
  setView(
    lng = -62,  # Longitude of the starting map centre
    lat = 44,   # Latitude of the starting map centre
    zoom = 5    # Starting zoom level
  ) |>
  
  # Add a scale bar so visitors can estimate distances
  addScaleBar(
    position = "bottomleft"
  )


# Display the interactive map in the RStudio Viewer
# This is only a preview and does not save a file
portfolio_map

# List the datasets available in my project
available_files <- list.files(
  path = data_dir,       # Look inside the data folder
  recursive = TRUE,      # Include files inside subfolders
  full.names = FALSE     # Show relative filenames
)


# Print the filenames in the Console
# This will help identify which datasets to use in the maps
print(available_files)

