# Atlantic bluefin tuna and comparison-species catch exploration
#
# This script is self-contained: it does not source 01_DataExploration.Rmd and
# does not rely on objects left in the Global Environment by script 01.
# Run it from the R project that contains data/raw_data/DFO_FisheriesAtlas/.
#
# Interpretation:
# - roundWeight_kg is reported commercial catch aggregated by fisheries grid
#   cell for 2019-2023; it is not a direct measure of abundance or habitat use.
# - the public layers do not provide fishing effort, trip counts or hook counts.
# - unreported cells are not converted to zero because they may be unfished,
#   unreported or privacy-screened.
# - herring, mackerel and squid catches are fishery-distribution indicators,
#   not prey-biomass estimates; spatial overlap does not establish predation.


## Bluefin_tuna layer summary statistics and visualizations.
library(ggplot2)
library(dplyr)
library(tmap)
library(rnaturalearth)
library(sf)
library(spdep)

tmap_mode("plot")

fisheries_geodatabase_path <- here::here(
  "data",
  "raw_data",
  "DFO_FisheriesAtlas",
  "Maritimes_Fisheries_Atlas_2019_2023.gdb"
)

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
    st_read(
      fisheries_geodatabase_path,
      layer = layer_name,
      quiet = TRUE
    )
  }
)

# Interested in visualizing the distribution of bluefin tuna catch weight across 
# the study area. The dataset uses roundWeight_kg to represent "live weight, or 
# round weight (kg)".

# Statistics Canada Lambert is metre-based and is more appropriate than one
# UTM zone for this study area's broad east-west extent.
study_area_projected_crs <- 3347

fisheries_species_data_projected <- lapply(
  fisheries_species_data,
  sf::st_transform,
  crs = study_area_projected_crs
)

study_area_land <- rnaturalearth::ne_states(
  country = c(
    "Canada",
    "United States of America"
  ),
  returnclass = "sf"
) |>
  sf::st_transform(study_area_projected_crs)

# Derive shared map extent from all eight species layers. This avoids
# clipping observations and avoids depending on study_area_extent from script 01.

# clipping observations and avoids depending on study_area_extent from script 01.
species_layer_bounding_boxes <- do.call(
  rbind,
  lapply(
    fisheries_species_data_projected,
    function(species_data) {
      
      species_bounding_box <- sf::st_bbox(species_data)
      
      c(
        xmin = unname(species_bounding_box[["xmin"]]),
        ymin = unname(species_bounding_box[["ymin"]]),
        xmax = unname(species_bounding_box[["xmax"]]),
        ymax = unname(species_bounding_box[["ymax"]])
      )
    }
  )
)

combined_species_extent <- c(
  xmin = min(species_layer_bounding_boxes[, "xmin"]),
  ymin = min(species_layer_bounding_boxes[, "ymin"]),
  xmax = max(species_layer_bounding_boxes[, "xmax"]),
  ymax = max(species_layer_bounding_boxes[, "ymax"])
)

extent_width_metres <-
  combined_species_extent[["xmax"]] -
  combined_species_extent[["xmin"]]

extent_height_metres <-
  combined_species_extent[["ymax"]] -
  combined_species_extent[["ymin"]]

# Define a shared map extent with a small buffer around all species
map_bounding_box <- sf::st_bbox(
  c(
    xmin = combined_species_extent[["xmin"]] - 0.02 * extent_width_metres,
    xmax = combined_species_extent[["xmax"]] + 0.02 * extent_width_metres,
    ymin = combined_species_extent[["ymin"]] - 0.08 * extent_height_metres,
    ymax = combined_species_extent[["ymax"]] + 0.02 * extent_height_metres
  ),
  crs = sf::st_crs(study_area_projected_crs)
)

# Calculate map aspect ratio
study_area_aspect_ratio <- unname(
  (map_bounding_box[["xmax"]] - map_bounding_box[["xmin"]]) /
    (map_bounding_box[["ymax"]] - map_bounding_box[["ymin"]])
)

# Identical breaks are used for all species, so colours have the same meaning
# on every map.
catch_weight_breaks_kg <- c(
  0,
  10,
  100,
  1e3,
  1e4,
  1e5,
  Inf
)

catch_weight_colours <- grDevices::hcl.colors(
  n = length(catch_weight_breaks_kg) - 1L,
  palette = "YlOrRd",
  rev = TRUE
)

species_map_titles <- c(
  bluefin_tuna = "Bluefin tuna",
  atlantic_herring = "Atlantic herring",
  atlantic_mackerel = "Atlantic mackerel",
  squid = "Squid",
  white_hake = "White hake",
  silver_hake = "Silver hake",
  red_hake = "Red hake",
  cusk = "Cusk"
)

# Exploratory multi-species catch maps
create_exploratory_catch_map <- function(
    species_catch_data,
    panel_title
) {
  
  # Represent each fisheries hexagon by its centroid
  catch_cell_centroids <- species_catch_data |>
    sf::st_centroid() |>
    dplyr::arrange(roundWeight_kg)
  
  # Create map
  catch_weight_map <-
    tmap::tm_shape(
      study_area_land,
      bbox = map_bounding_box
    ) +
    tmap::tm_polygons(
      col = "grey92",
      border.col = "grey30",
      lwd = 0.3
    ) +
    tmap::tm_shape(catch_cell_centroids) +
    tmap::tm_dots(
      col = "roundWeight_kg",
      style = "fixed",
      breaks = catch_weight_breaks_kg,
      palette = catch_weight_colours,
      shape = 21,
      size = 0.10,
      alpha = 0.90,
      border.col = "grey20",
      border.lwd = 0.25,
      title = "Round weight (kg)"
    ) +
    tmap::tm_scale_bar(
      position = c("right", "bottom"),
      width = 0.25,
      text.size = 0.6
    ) +
    tmap::tm_compass(
      position = c("right", "top"),
      size = 2,
      text.size = 0.6
    ) +
    tmap::tm_layout(
      main.title = panel_title,
      main.title.position = "left",
      main.title.size = 1.1,
      main.title.fontface = "bold",
      bg.color = "#dce9f1",
      frame = TRUE,
      legend.outside = TRUE,
      legend.outside.position = "right",
      legend.frame = FALSE,
      legend.title.size = 0.9,
      legend.text.size = 0.75,
      legend.format = list(
        big.mark = ",",
        text.separator = "-"
      ),
      inner.margins = 0,
      asp = study_area_aspect_ratio
    )
  
  return(catch_weight_map)
}


# Generate species maps

catch_weight_maps <- lapply(
  names(fisheries_species_data_projected),
  function(species_name) {
    
    create_exploratory_catch_map(
      species_catch_data =
        fisheries_species_data_projected[[species_name]],
      
      panel_title =
        species_map_titles[[species_name]]
    )
  }
)

# Assign descriptive names to the map list
names(catch_weight_maps) <- names(fisheries_species_data_projected)

# Display each map individually
for (species_name in names(catch_weight_maps)) {
  
  print(catch_weight_maps[[species_name]])
}

# # Bluefin tuna
# catch_weight_maps$bluefin_tuna
# 
# # Atlantic herring
# catch_weight_maps$atlantic_herring
# 
# # Atlantic mackerel
# catch_weight_maps$atlantic_mackerel
# 
# # Squid
# catch_weight_maps$squid
# 
# # White hake
# catch_weight_maps$white_hake
# 
# # Silver hake
# catch_weight_maps$silver_hake
# 
# # Red hake
# catch_weight_maps$red_hake
# 
# # Cusk
# catch_weight_maps$cusk


# Catch statistics
bluefin_tuna_catch_data <- fisheries_species_data$bluefin_tuna # Extract

# Retain records with valid, positive reported catch weights
bluefin_tuna_positive_catches <- bluefin_tuna_catch_data |>
  dplyr::filter(
    !is.na(roundWeight_kg),
    is.finite(roundWeight_kg),
    roundWeight_kg > 0
  )

# Extract catch weights for descriptive analysis
bluefin_tuna_catch_weights_kg <-
  bluefin_tuna_positive_catches$roundWeight_kg

# Calculate the number of highest-catch cells representing 10% of records
highest_catch_cell_count <- ceiling(
  0.10 * length(bluefin_tuna_catch_weights_kg)
)

# Calculate descriptive statistics
bluefin_tuna_catch_summary <- tibble::tibble(
  reporting_period = "2019-2023",
  
  reported_catch_cells = length(bluefin_tuna_catch_weights_kg),
  
  total_round_weight_kg = sum(
    bluefin_tuna_catch_weights_kg
  ),
  
  mean_round_weight_kg = mean(
    bluefin_tuna_catch_weights_kg
  ),
  
  median_round_weight_kg = median(
    bluefin_tuna_catch_weights_kg
  ),
  
  standard_deviation_kg = sd(
    bluefin_tuna_catch_weights_kg
  ),
  
  minimum_round_weight_kg = min(
    bluefin_tuna_catch_weights_kg
  ),
  
  maximum_round_weight_kg = max(
    bluefin_tuna_catch_weights_kg
  ),
  
  round_weight_90th_percentile_kg = unname(
    quantile(bluefin_tuna_catch_weights_kg, 0.90)
  ),
  
  catch_share_highest_10_percent_cells = sum(
    sort(
      bluefin_tuna_catch_weights_kg,
      decreasing = TRUE
    )[seq_len(highest_catch_cell_count)]
  ) / sum(bluefin_tuna_catch_weights_kg)
)

# Display summary
print(bluefin_tuna_catch_summary, width = Inf)

# Catch-weight distribution
bluefin_tuna_catch_histogram <- ggplot(
  bluefin_tuna_positive_catches,
  aes(x = roundWeight_kg)
) +
  geom_histogram(
    bins = 40,
    fill = "darkblue",
    colour = "grey20",
    linewidth = 0.2
  ) +
  scale_x_log10(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Distribution of Atlantic bluefin tuna catch weight",
    subtitle = "Reported commercial catch cells, 2019-2023",
    x = "Round weight per catch cell (kg, log scale)",
    y = "Number of catch cells",
    caption = paste(
      "Source: DFO Maritimes Fisheries Atlas.",
      "Positive reported catch cells only."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

bluefin_tuna_catch_histogram


bluefin_tuna_catch_concentration <- tibble::tibble(
  round_weight_kg = sort(
    bluefin_tuna_catch_weights_kg,
    decreasing = TRUE
  )
) |>
  dplyr::mutate(
    cumulative_cell_percentage =
      100 * dplyr::row_number() / dplyr::n(),
    
    cumulative_catch_percentage =
      100 * cumsum(round_weight_kg) / sum(round_weight_kg)
  )

bluefin_tuna_concentration_plot <- ggplot(
  bluefin_tuna_catch_concentration,
  aes(
    x = cumulative_cell_percentage,
    y = cumulative_catch_percentage
  )
) +
  geom_line(
    colour = "darkblue", 
# cumulative percentage of reported bluefin tuna catch weight, with grid cells 
# ranked from highest to lowest catch.
    linewidth = 1
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    colour = "grey50"
# represents an equal distribution of catch weight across cells
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    title = "Concentration of reported bluefin tuna catch",
    subtitle = "Catch cells ranked from highest to lowest round weight",
    x = "Cumulative percentage of catch cells",
    y = "Cumulative percentage of reported catch weight",
    caption = "DFO Fisheries Atlas, 2019-2023"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

bluefin_tuna_concentration_plot


# Between 2019 and 2023, the DFO Fisheries Atlas recorded approximately 1.40 
# million kg of Atlantic bluefin tuna landings across 2,163 spatial grid cells. 
# Reported catch weight was strongly right-skewed, with a mean of 645 kg per cell 
# and a median of 228 kg. Individual cell totals ranged from 40.8 to 43,284 kg, 
# indicating substantial spatial variation. Catch was highly concentrated, with 
# the highest-catch 10% of reported cells accounting for 61.0% of total recorded 
# weight. These results demonstrate pronounced spatial heterogeneity in commercial 
# bluefin tuna landings, although the observed patterns may also reflect 
# differences in fishing effort, regulations and reporting.


