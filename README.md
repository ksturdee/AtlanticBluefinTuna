# Atlantic Bluefin Tuna: Spatial Patterns in Reported Catch

Exploring the spatial distribution of reported Atlantic bluefin tuna catches
in Atlantic Canada and their associations with seabed depth and sea surface
temperature using R.

## Research Question

How are reported Atlantic bluefin tuna catches spatially distributed across
Atlantic Canada, and how do these patterns correspond with bathymetry and
mean sea surface temperature?

Study Area: the Scotian Shelf and adjacent Atlantic waters,
using commercial fisheries records aggregated across 2019-2023.

## Datasets
- **DFO Fisheries Atlas (2019–2023):** Reported commercial bluefin tuna
  landings aggregated into spatial reporting hexagons.
- **GEBCO 2026:** Bathymetric data used to examine seabed depth and
  proximity to the 200 m contour.
- **Copernicus Marine GLORYS:** Monthly modeled ocean temperature
  averaged across 2019–2023.

## Methods

Spatial processing and analysis were conducted in R using `sf`, `terra`,
`dplyr`, `tmap`, `ggplot2` and `dbscan`.

The workflow involved:

1. Validating the source datasets.
2. Preparing reported bluefin tuna catch data.
3. Processing bathymetry and five-year mean surface temperature.
4. Extracting environmental conditions across fisheries reporting cells.
5. Calculating descriptive statistics and Spearman correlations.
6. Identifying spatial clusters of high-catch reporting cells using DBSCAN.
7. Producing two final spatial figures.

High-catch cells were defined using the 90th percentile of reported catch
weight. DBSCAN clustering used a 30 km neighbourhood and a minimum of four
cells per cluster. The two clusters with the highest total reported catch
were selected for detailed mapping.

## Results

The analysis identified pronounced spatial variation in reported commercial
bluefin tuna catches.

- Approximately 1.40 million kg of reported catch was distributed across
  2,163 reporting cells.
- The highest-catch 10% of cells accounted for approximately 61% of total
  reported catch weight.
- Reported catch weight showed negative rank associations with mean seabed
  depth and five-year mean surface temperature.
- The two largest high-catch spatial clusters were examined in detail
  against local bathymetry.

## Final Figures
Regional distribution of reported Atlantic bluefin tuna catches
over GEBCO bathymetry.

<img width="1366" height="1008" alt="Figure1A_RegionalBluefinTuna_CatchAndBathymetry" src="https://github.com/user-attachments/assets/b0d0c74e-a0cc-4ff2-80f8-b2e4dee4c428" />
Figure 1A: Reported Catch Distribution and Bathymetry

<img width="1726" height="768" alt="Figure1BC_BluefinTuna_HotspotsAndBathymetry" src="https://github.com/user-attachments/assets/1ff0e2d4-9ffb-420e-8b3f-e3e8f53cc493" />
Figure 1BC: The two highest-catch spatial clusters were selected using DBSCAN
clustering of top-decile reporting cells.

Reported bluefin tuna catches over five-year mean sea surface
temperature (2019–2023), with labelled isotherms and subtle
bathymetric shading.

<img width="3600" height="2400" alt="Figure2_CatchTemperature" src="https://github.com/user-attachments/assets/959eb9c7-9cc5-4ba3-9200-160206c33578" />
Figure 2: Catch Distribution and Surface Temperature

## Reproducibility

The numbered R scripts document the workflow from dataset validation
through spatial processing, statistical analysis and final figure production.
Original datasets are preserved separately from processed outputs.
