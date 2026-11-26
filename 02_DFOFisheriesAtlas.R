# 02_DFOFisheriesAtlas.R

# The purpose of this script is to prepare Atlantic bluefin tuna catch data
# from the DFO Fisheries Atlas, aggregated for 2019-2023.
# I will summarise reported catch weights and prepare the spatial data
# needed for the final catch distribution and environmental maps.

# This script is self-contained. It does not source Script 01 and
# does not rely on objects left in the Global Environment.

# Catch weights represent reported commercial landings, not abundance.
# Fishing effort is unavailable, and unreported cells are not treated as zero.


## Bluefin_tuna layer summary statistics and visualizations.
library(dplyr)
library(rnaturalearth)
library(sf)
library(here)

# Locate DFO fisheries geodatabase
fisheries_geodatabase_path <- here::here(
  "data",
  "raw_data",
  "DFO_FisheriesAtlas",
  "Maritimes_Fisheries_Atlas_2019_2023.gdb"
)

# Import layer
bluefin_tuna_catch_data <- sf::st_read(
  fisheries_geodatabase_path,
  layer = "BluefinTuna_Landings_Weight_2019_2023",
  quiet = TRUE
)

# Prep spatial data for mapping 
# Statistics Canada Lambert (EPSG:3347) is metre-based and is more
# appropriate than one UTM zone for this study area's broad east-west extent.
study_area_projected_crs <- 3347

# Project bluefin tuna catch cells to the study area CRS
bluefin_tuna_catch_projected <- sf::st_transform(
  bluefin_tuna_catch_data,
  crs = study_area_projected_crs
)

# Retain the original object structure required by Scripts 07 and 08
# This allows the existing figure scripts to run without changing
# their references to fisheries_species_data_projected$bluefin_tuna.
fisheries_species_data_projected <- list(
  bluefin_tuna = bluefin_tuna_catch_projected
)

# Load provincial and state boundaries for map backgrounds
# Transform land to match the projected fisheries data
study_area_land <- rnaturalearth::ne_states(
  country = c(
    "Canada",
    "United States of America"
  ),
  returnclass = "sf"
) |>
  sf::st_transform(study_area_projected_crs)

# Define the map extent using the bluefin tuna reporting cells
bluefin_tuna_bounding_box <- sf::st_bbox(
  bluefin_tuna_catch_projected
)

# Calculate the full width and height in metres
extent_width_metres <-
  bluefin_tuna_bounding_box[["xmax"]] -
  bluefin_tuna_bounding_box[["xmin"]]

extent_height_metres <-
  bluefin_tuna_bounding_box[["ymax"]] -
  bluefin_tuna_bounding_box[["ymin"]]

# Define the map extent with a small buffer around the catches
map_bounding_box <- sf::st_bbox(
  c(
    xmin = bluefin_tuna_bounding_box[["xmin"]] -
      0.02 * extent_width_metres,
    
    xmax = bluefin_tuna_bounding_box[["xmax"]] +
      0.02 * extent_width_metres,
    
    ymin = bluefin_tuna_bounding_box[["ymin"]] -
      0.08 * extent_height_metres,
    
    ymax = bluefin_tuna_bounding_box[["ymax"]] +
      0.02 * extent_height_metres
  ),
  crs = sf::st_crs(study_area_projected_crs)
)

# Calculate map aspect ratio
study_area_aspect_ratio <- unname(
  (map_bounding_box[["xmax"]] - map_bounding_box[["xmin"]]) /
    (map_bounding_box[["ymax"]] - map_bounding_box[["ymin"]])
)

# Catch statistics

# Retain records with valid, positive reported catch weights
# Missing and unreported cells are not assigned zero catch.
bluefin_tuna_positive_catches <- bluefin_tuna_catch_data |>
  dplyr::filter(
    is.finite(roundWeight_kg),
    roundWeight_kg > 0
  )

# Extract catch weights as a numeric vector for descriptive analysis
bluefin_tuna_catch_weights_kg <-
  bluefin_tuna_positive_catches$roundWeight_kg

# Calculate how many reporting cells represent the highest 10%. Ceiling() makes
# sure a whole number of cells is selected
highest_catch_cell_count <- ceiling(
  0.10 * length(bluefin_tuna_catch_weights_kg)
)

# Calculate descriptive statistics, summarise
bluefin_tuna_catch_summary <- tibble::tibble(
  reporting_period = "2019-2023",
  
  reported_catch_cells = length(bluefin_tuna_catch_weights_kg),
  
  total_round_weight_kg = sum(
    bluefin_tuna_catch_weights_kg
  ), # Combined reported catch weight
  
  mean_round_weight_kg = mean(
    bluefin_tuna_catch_weights_kg
  ), # Avg catch per reporting cell
  
  median_round_weight_kg = median(
    bluefin_tuna_catch_weights_kg
  ), # Typical cell catch (median) less affected by extremes
  
  standard_deviation_kg = sd(
    bluefin_tuna_catch_weights_kg
  ), # Variation in catch weight between cells
  
  minimum_round_weight_kg = min(
    bluefin_tuna_catch_weights_kg
  ),
  
  maximum_round_weight_kg = max(
    bluefin_tuna_catch_weights_kg
  ),
  
  round_weight_90th_percentile_kg = unname(
    quantile(bluefin_tuna_catch_weights_kg, 0.90)
  ), # Threshold to seperate highest catch 10% of cells 
  
  catch_share_highest_10_percent_cells = sum(
    sort(
      bluefin_tuna_catch_weights_kg,
      decreasing = TRUE
    )[seq_len(highest_catch_cell_count)]
  ) / sum(bluefin_tuna_catch_weights_kg)
) # Share of total catch recorded in the highest ranked 10% of cells

# Display summary
print(bluefin_tuna_catch_summary, width = Inf)

# Interpretation
# Between 2019 and 2023, the DFO Fisheries Atlas recorded approximately 1.40 
# million kg of Atlantic bluefin tuna landings across 2,163 spatial grid cells. 
# Reported catch weight was strongly right-skewed, with a mean of 645 kg per cell 
# and a median of 228 kg. Individual cell totals ranged from 40.8 to 43,284 kg, 
# indicating substantial spatial variation. Catch was highly concentrated, with 
# the highest-catch 10% of reported cells accounting for 61.0% of total recorded 
# weight. These results demonstrate pronounced spatial heterogeneity in commercial 
# bluefin tuna landings, although the observed patterns may also reflect 
# differences in fishing effort, regulations and reporting.


