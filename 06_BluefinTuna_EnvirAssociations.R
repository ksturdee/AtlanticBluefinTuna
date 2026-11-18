# 06_BluefinTuna_EnvironmentalAssociations.R

# Research question has been narrowed: How are reported Atlantic bluefin tuna 
# catches spatially distributed across Atlantic Canada, and how do these 
# patterns correspond with seabed depth and mean sea surface temperature?

# The purpose of this script is to calculate summary statistics supporting
# the final spatial figures and presentation for this course. 

# Load required packages
library(sf)
library(dplyr)
library(here)

# Import the integrated fisheries and environmental dataset
integrated_data_path <- here::here(
  "data",
  "processed_data",
  "spatial_integration",
  "bluefin_tuna_fisheries_environment_2019_2023.gpkg"
)


bluefin_tuna_spatial_data <- sf::st_read(
  integrated_data_path,
  layer = "bluefin_tuna_environment",
  quiet = TRUE
)

# Remove geometry because correlations use the attribute values
# Keep only positive, finite reported catch weights
bluefin_tuna_analysis_data <- bluefin_tuna_spatial_data |>
  sf::st_drop_geometry() |>
  dplyr::rename(
    bluefin_tuna_catch_kg = roundWeight_kg
  ) |>
  dplyr::filter(
    is.finite(bluefin_tuna_catch_kg),
    bluefin_tuna_catch_kg > 0
  ) 

# Define envir variables retained
catch_association_variables <- c(
  "mean_seabed_depth_m",
  "mean_surface_temperature_c",
  "distance_to_200m_contour_km"
)

# Count valid and missing environmental values
# is.finite() excludes NA, NaN and infinite values
environmental_completeness_summary <- tibble::tibble(
  variable = catch_association_variables,
  
  complete_records = vapply(
    catch_association_variables,
    function(variable_name) {
      sum(is.finite(bluefin_tuna_analysis_data[[variable_name]]))
    },
    integer(1)
  ),
  
  missing_records = vapply(
    catch_association_variables,
    function(variable_name) {
      sum(!is.finite(bluefin_tuna_analysis_data[[variable_name]]))
    },
    integer(1)
  )
)

print(environmental_completeness_summary)


# Keep cells with complete values for the three environmental variables
# These cells will be used for the environmental summaries and correlations
bluefin_tuna_environmental_complete_cases <-
  bluefin_tuna_analysis_data |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(catch_association_variables),
      is.finite
    )
  )

cat(
  "Complete catch-environment records:",
  nrow(bluefin_tuna_environmental_complete_cases),
  "\n"
)

# Summarise reported catch weight

bluefin_tuna_catch_weights_kg <-
  bluefin_tuna_analysis_data$bluefin_tuna_catch_kg

# Calculate how many reporting cells represent the highest 10%.
highest_catch_cell_count <- ceiling(
  0.10 * length(bluefin_tuna_catch_weights_kg)
)

# Calculate the total catch represented by the highest-ranked 10% of cells.
highest_10_percent_catch_kg <- sum(
  sort(
    bluefin_tuna_catch_weights_kg,
    decreasing = TRUE
  )[seq_len(highest_catch_cell_count)]
)

# Produce the catch summary needed for the presentation.
bluefin_tuna_catch_summary <- tibble::tibble(
  reporting_period = "2019-2023",
  
  reported_catch_cells = length(
    bluefin_tuna_catch_weights_kg
  ),
  
  total_reported_catch_kg = sum(
    bluefin_tuna_catch_weights_kg
  ),
  
  mean_catch_per_cell_kg = mean(
    bluefin_tuna_catch_weights_kg
  ),
  
  median_catch_per_cell_kg = median(
    bluefin_tuna_catch_weights_kg
  ),
  
  minimum_catch_per_cell_kg = min(
    bluefin_tuna_catch_weights_kg
  ),
  
  maximum_catch_per_cell_kg = max(
    bluefin_tuna_catch_weights_kg
  ),
  
  catch_share_highest_10_percent_cells =
    highest_10_percent_catch_kg /
    sum(bluefin_tuna_catch_weights_kg)
)

print(
  bluefin_tuna_catch_summary,
)


# Summarise environmental conditions associated with catch cells

# These summaries describe environmental conditions across reporting cells.
# They do not demonstrate tuna habitat preferences.
bluefin_tuna_environmental_summary <-
  bluefin_tuna_environmental_complete_cases |>
  dplyr::summarise(
    reporting_cells = dplyr::n(),
    
    minimum_mean_seabed_depth_m = min(
      mean_seabed_depth_m
    ),
    
    median_mean_seabed_depth_m = median(
      mean_seabed_depth_m
    ),
    
    mean_seabed_depth_m = mean(
      mean_seabed_depth_m
    ),
    
    maximum_mean_seabed_depth_m = max(
      mean_seabed_depth_m
    ),
    
    minimum_mean_surface_temperature_c = min(
      mean_surface_temperature_c
    ),
    
    median_mean_surface_temperature_c = median(
      mean_surface_temperature_c
    ),
    
    mean_surface_temperature_c = mean(
      mean_surface_temperature_c
    ),
    
    maximum_mean_surface_temperature_c = max(
      mean_surface_temperature_c
    ),
    
    minimum_distance_to_200m_contour_km = min(
      distance_to_200m_contour_km
    ),
    
    median_distance_to_200m_contour_km = median(
      distance_to_200m_contour_km
    ),
    
    mean_distance_to_200m_contour_km = mean(
      distance_to_200m_contour_km
    ),
    
    maximum_distance_to_200m_contour_km = max(
      distance_to_200m_contour_km
    )
  )

print(
  bluefin_tuna_environmental_summary
)

# Calculate Spearman correlations using original catch weights
# Spearman measures whether values generally increase or decrease together

environmental_variable_labels <- c(
  mean_seabed_depth_m = "Mean seabed depth (m)",
  mean_surface_temperature_c =
    "Mean surface temperature (degrees C)",
  distance_to_200m_contour_km =
    "Distance to 200 m contour (km)"
)
  
environmental_correlation_summary <- tibble::tibble(
  environmental_variable = catch_association_variables,

  spearman_rho = vapply(
    catch_association_variables,
    function(variable_name) {
      stats::cor(
        bluefin_tuna_environmental_complete_cases$bluefin_tuna_catch_kg,
        bluefin_tuna_environmental_complete_cases[[variable_name]],
        method = "spearman"
      )
    },
    numeric(1)
  )
) |>
  dplyr::mutate(
    environmental_variable =
      unname(environmental_variable_labels[environmental_variable])
  )

print(environmental_correlation_summary)

# Interpretation

# Previous analysis found moderate negative rank associations between reported
# catch weight and mean seabed depth, and between reported catch weight and
# five-year mean surface temperature.

# A negative correlation with seabed depth indicates that larger reported
# catches generally occurred in reporting cells with shallower mean depths.

# A negative correlation with mean surface temperature indicates that larger
# reported catches generally occurred in reporting cells with cooler five-year
# mean surface temperatures.

# Distance to the 200 m contour describes how reporting cells are distributed
# relative to the continental shelf boundary.

# These relationships do not account for differences in fishing effort,
# regulations, reporting practices or spatial dependence among neighbouring
# reporting cells. They must not be interpreted as causal relationships or
# direct measures of tuna abundance or habitat preference.


# Export presentation statistics

statistics_output_directory <- here::here(
  "outputs"
)

# Create the output directory if it does not already exist.
dir.create(
  statistics_output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

catch_summary_output_path <- file.path(
  statistics_output_directory,
  "bluefin_tuna_catch_summary.csv"
)

environmental_summary_output_path <- file.path(
  statistics_output_directory,
  "bluefin_tuna_environmental_summary.csv"
)

correlation_summary_output_path <- file.path(
  statistics_output_directory,
  "bluefin_tuna_environmental_correlations.csv"
)

utils::write.csv(
  bluefin_tuna_catch_summary,
  catch_summary_output_path,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  bluefin_tuna_environmental_summary,
  environmental_summary_output_path,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  environmental_correlation_summary,
  correlation_summary_output_path,
  row.names = FALSE,
  na = ""
)

# Confirm all presentation-statistics files were created.
stopifnot(
  file.exists(catch_summary_output_path),
  file.exists(environmental_summary_output_path),
  file.exists(correlation_summary_output_path)
)

