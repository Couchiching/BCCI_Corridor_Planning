# ============================================================
# BCCI REGIONAL CORRIDOR MAPPING
# Preliminary Analytical Workflow
# CC | KLT | NLT
#
# Working pseudocode — methods, indicators, and parameters
# to be developed collaboratively throughout the project.
# ============================================================


# ------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------

# sf          - Vector data processing and spatial operations
# terra       - Raster processing and landscape analysis
# dplyr       - Data manipulation and summarization
# purrr       - Iterative processing across datasets
# exactextractr - Raster summaries within polygons
# ggplot2     - Visualization and preliminary mapping
#
# Potential connectivity packages:
# landscapemetrics - Landscape structure and fragmentation
# gdistance        - Cost-distance and least-cost analysis
# leastcostpath    - Least-cost path and corridor analysis
# Circuitscape     - Circuit-theory connectivity (external tool)
#
# Additional packages to be determined as methods develop.


# ------------------------------------------------------------
# 1. REGIONAL BASELINE
# ------------------------------------------------------------

# Define regional study area
# sf::st_union()       - Combine operational areas
# sf::st_buffer()      - Include surrounding landscape
# sf::st_transform()   - Standardize coordinate systems

# Load and standardize spatial datasets
# sf::st_read()        - Read vector datasets
# terra::rast()        - Read raster datasets
# sf::st_make_valid()  - Repair invalid geometries
# sf::st_intersection() / st_filter() - Select regional features
# terra::crop() / mask() - Prepare regional rasters

# Assess coverage, resolution, currency, and data quality
# sf::st_is_valid()
# terra::res(), crs(), ext()
# dplyr::summarise()

# Output: Standardized regional baseline (GeoPackage)


# ------------------------------------------------------------
# 2. CONSERVATION VALUES
# ------------------------------------------------------------

# Review existing conservation priorities and strategies
# Define shared conservation objectives
# Select relevant ecological indicators

# Identify significant natural heritage features
# sf::st_intersection() - Identify overlapping features
# sf::st_union()        - Consolidate ecological features
# sf::st_area()         - Calculate habitat area

# Identify potential habitat cores
# terra::patches()     - Identify contiguous habitat patches
# terra::ifel()        - Classify suitable habitat
# terra::distance()    - Assess proximity between features

# Assess ecological values across the regional landscape
# terra::rasterize()   - Convert vector indicators to rasters
# terra::app()         - Apply agreed-upon analytical functions
# exactextractr::exact_extract() - Summarize raster indicators

# Output: Ecological value layers


# ------------------------------------------------------------
# 3. LANDSCAPE CONNECTIVITY
# ------------------------------------------------------------

# Define ecological features to connect
# Select connectivity approach and spatial scale

# Assess landscape structure and fragmentation
# landscapemetrics::calculate_lsm()
# terra::patches()

# Develop landscape resistance/permeability surface
# terra::classify()    - Reclassify land cover
# terra::rasterize()   - Incorporate landscape barriers
# terra::app()         - Combine resistance variables

# Model existing and potential ecological connections
# gdistance::transition() - Construct conductance surface
# gdistance::geoCorrection() - Correct for spatial geometry
# gdistance::costDistance() - Calculate cost-weighted distance
# gdistance::shortestPath() - Identify least-cost paths

# Potential alternative/complementary methods:
# leastcostpath        - Least-cost corridor analysis
# Circuitscape         - Circuit-theory connectivity

# Identify critical linkages, barriers, and connectivity gaps
# Compare model outputs with existing protected lands
# Identify potential restoration opportunities

# Output: Regional connectivity layers