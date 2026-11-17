# 06_BluefinTuna_EnvironmentalAssociations.R

# Research question has been narrowed: How do bathymetry, oceanographic 
# conditions and potential forage-fishery distributions relate to reported 
# Atlantic bluefin tuna catch hotspots in Atlantic Canada?

# The purpose of this script is to look at relationships between reported 
# Atlantic bluefin tuna catch weight, bathymetry, oceanographic variables and 
# assocaitions with potential prey-fisheries. This script also runs independently 
# of scripts 01-05.

# Load required packages
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# Import the integrated fisheries and environmental dataset
integrated_data_path <- here::here(
  "data",
  "processed_data",
  "spatial_integration",
  "bluefin_tuna_fisheries_environment_2019_2023.gpkg"
)

stopifnot(file.exists(integrated_data_path))

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
  ) |>
  dplyr::mutate(
    log_bluefin_tuna_catch = log10(bluefin_tuna_catch_kg)
  ) # Log scale makes the right-skewed catches easier to plot


# Four variables used to examine catch associations
catch_association_variables <- c(
  "mean_seabed_depth_m",
  "mean_surface_temperature_c",
  "mean_current_speed_ms",
  "distance_to_200m_contour_km"
)

# Include slope and local variability when checking predictor overlap
predictor_diagnostic_variables <- c(
  "mean_seabed_depth_m",
  "mean_seafloor_slope_degrees",
  "mean_local_depth_variability_m",
  "distance_to_200m_contour_km",
  "mean_surface_temperature_c",
  "mean_current_speed_ms"
)

# Check that all required fields exist before analysing them
stopifnot(
  all(
    predictor_diagnostic_variables %in%
      names(bluefin_tuna_analysis_data)
  )
)


# Count valid and missing environmental values
# is.finite() excludes NA, NaN and infinite values
environmental_completeness_summary <- tibble::tibble(
  variable = predictor_diagnostic_variables,
  
  complete_records = vapply(
    predictor_diagnostic_variables,
    function(variable_name) {
      sum(is.finite(bluefin_tuna_analysis_data[[variable_name]]))
    },
    integer(1)
  ),
  
  missing_records = vapply(
    predictor_diagnostic_variables,
    function(variable_name) {
      sum(!is.finite(bluefin_tuna_analysis_data[[variable_name]]))
    },
    integer(1)
  )
)

print(environmental_completeness_summary)


# Keep cells with complete values for the four main relationships
# Slope and variability do not determine membership in this sample
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


# Give the four environmental variables readable figure labels
environmental_variable_labels <- c(
  mean_seabed_depth_m = "Mean seabed depth (m)",
  mean_surface_temperature_c = "Mean surface temperature (°C)",
  mean_current_speed_ms = "Mean surface current speed (m/s)",
  distance_to_200m_contour_km = "Distance to 200 m contour (km)"
)

# Reshape the data so all four relationships can use one plot
environmental_relationship_data <-
  bluefin_tuna_environmental_complete_cases |>
  dplyr::select(
    hexGridID,
    bluefin_tuna_catch_kg,
    log_bluefin_tuna_catch,
    dplyr::all_of(catch_association_variables)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(catch_association_variables),
    names_to = "environmental_variable",
    values_to = "environmental_value"
  ) |>
  dplyr::mutate(
    environmental_variable = factor(
      environmental_variable,
      levels = names(environmental_variable_labels),
      labels = unname(environmental_variable_labels)
    )
  )


# Plot catch weight against each environmental variable
# Points represent reporting cells; LOESS shows the broad trend
environmental_relationship_figure <- ggplot(
  environmental_relationship_data,
  aes(
    x = environmental_value,
    y = log_bluefin_tuna_catch
  )
) +
  geom_point(
    colour = "#2166AC",
    alpha = 0.12,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    colour = "#D94801",
    fill = "#FDD0A2",
    linewidth = 1,
    se = TRUE
  ) +
  facet_wrap(
    ~ environmental_variable,
    scales = "free_x",
    ncol = 2
  ) +
  labs(
    title = "Environmental Associations of Atlantic Bluefin Tuna Catch",
    subtitle = "Reported commercial catch cells, Atlantic Canada (2019–2023)",
    x = NULL,
    y = expression(log[10]*"(reported catch weight, kg)"),
    caption = paste(
      "Sources: DFO Fisheries Atlas, GEBCO 2026 and Copernicus GLORYS.",
      "Relationships do not account for fishing effort."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, colour = "grey35"),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = grid::unit(1.2, "lines"),
    plot.margin = margin(15, 20, 20, 25),
    plot.caption = element_text(
      size = 8,
      colour = "grey40",
      hjust = 1
    )
  )

# Preview only
print(environmental_relationship_figure)


# Calculate Spearman correlations using original catch weights
# Spearman measures whether values generally increase or decrease together
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


# Check relationships among all six environmental variables
# Strong correlations suggest predictors may describe similar gradients
predictor_complete_cases <- bluefin_tuna_analysis_data |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(predictor_diagnostic_variables),
      is.finite
    )
  )

environmental_predictor_correlations <- stats::cor(
  predictor_complete_cases |>
    dplyr::select(dplyr::all_of(predictor_diagnostic_variables)),
  method = "spearman"
)

print(round(environmental_predictor_correlations, 2))


# Convert the correlation matrix to a table for plotting
predictor_correlation_data <- as.data.frame(
  as.table(environmental_predictor_correlations)
)

names(predictor_correlation_data) <- c(
  "variable_x",
  "variable_y",
  "spearman_rho"
)

# Short labels keep the heatmap readable
predictor_labels <- c(
  mean_seabed_depth_m = "Seabed depth",
  mean_seafloor_slope_degrees = "Seafloor slope",
  mean_local_depth_variability_m = "Depth variability",
  distance_to_200m_contour_km = "200 m contour distance",
  mean_surface_temperature_c = "Surface temperature",
  mean_current_speed_ms = "Current speed"
)

# Keep only the lower half of the matrix
# The upper half repeats the same correlations
predictor_correlation_data <- predictor_correlation_data |>
  dplyr::mutate(
    x_order = match(variable_x, names(predictor_labels)),
    y_order = match(variable_y, names(predictor_labels))
  ) |>
  dplyr::filter(y_order > x_order) |>
  dplyr::mutate(
    variable_x = factor(
      variable_x,
      levels = names(predictor_labels),
      labels = unname(predictor_labels)
    ),
    variable_y = factor(
      variable_y,
      levels = names(predictor_labels),
      labels = unname(predictor_labels)
    )
  )


# Plot the environmental predictor correlations
# Values near +1 or -1 indicate strong rank associations
environmental_correlation_heatmap <- ggplot(
  predictor_correlation_data,
  aes(
    x = variable_x,
    y = variable_y,
    fill = spearman_rho
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = sprintf("%.2f", spearman_rho)),
    size = 3.5
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Spearman's ρ"
  ) +
  coord_equal() +
  labs(
    title = "Correlations Among Environmental Variables",
    subtitle = "Bluefin tuna reporting cells, 2019–2023",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Preview only
print(environmental_correlation_heatmap)

# Slope and bathymetric variability are almost redundant (p= 0.99). 
# Depth, temperature and currents are interconnected; deeper catch locations tend
# to have warmer five-year mean surface temperatures, stronger mean surface currents.
# Distance to the 200 m contour is relatively distinct.
