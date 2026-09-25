
# ============================================================
# NHIC 1 KM GRID — DERIVED SAR RICHNESS
# ============================================================
#
# Source: Ontario NHIC Provincially Tracked Species 1 km Grid
# https://geohub.lio.gov.on.ca/datasets/lio::provincially-tracked-species-1km-grid/about
#
# Purpose:
# Calculate documented species richness and Element Occurrence
# counts per 1 km grid cell for regional conservation planning.
#
# Methods:
# - Count distinct scientific names within each grid cell.
# - Define Ontario SAR as END, THR, or SC under SARO.
# - Exclude restricted species and non-species elements.
# - Preserve the complete provincial grid.
# - Reproject to EPSG:26917.
#
# Limitations:
# Richness reflects documented occurrences, not confirmed
# presence or absence. Restricted species are excluded.
# Redistribution is subject to applicable NHIC licence terms.
#
# Output: NHIC_Species_1km_Derived.gpkg
# Layer:  NHIC_SAR_Richness_1km_Derived
# ============================================================

library(sf)
library(dplyr)

# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

base <- "C:/Users/ConservAnalyst/Documents/ConservationAnalyst/GISDatabase"

gdb <- file.path(
  base,
  "Baselayers/NHIC_Species_1km/PROVTRKG/Non_Sensitive.gdb"
)

output <- file.path(
  base,
  "Baselayers/NHIC_Species_1km/NHIC_Species_1km_Derived.gpkg"
)

layer <- "NHIC_SAR_Richness_1km_Derived"
target_crs <- 26917

stopifnot(dir.exists(gdb))

# ------------------------------------------------------------
# READ SOURCE DATA
# ------------------------------------------------------------

grid <- st_read(
  gdb,
  layer = "PROV_TRK_SPECIES_1KM_GRID",
  quiet = TRUE
)

detail <- st_read(
  gdb,
  layer = "PROV_TRK_SPECIES_GRID_DETAIL",
  quiet = TRUE
) |>
  st_drop_geometry()

# Verify grid identifiers and table relationship.

stopifnot(
  !anyNA(grid$OGF_ID),
  !anyDuplicated(grid$OGF_ID),
  all(detail$PROV_TRK_SPECIES_1KM_GRID_ID %in% grid$OGF_ID)
)

# ------------------------------------------------------------
# FILTER SPECIES RECORDS
# ------------------------------------------------------------

# Exclude restricted species, plant communities, wildlife
# concentration areas, and records without scientific names.

species <- detail |>
  filter(
    ELEMENT_TYPE == "SPECIES",
    !is.na(SCIENTIFIC_NAME),
    nzchar(trimws(SCIENTIFIC_NAME))
  ) |>
  mutate(
    SCIENTIFIC_NAME = trimws(SCIENTIFIC_NAME)
  )

# ------------------------------------------------------------
# CALCULATE GRID METRICS
# ------------------------------------------------------------

summary <- species |>
  group_by(PROV_TRK_SPECIES_1KM_GRID_ID) |>
  summarise(
    Tracked_Richness = n_distinct(SCIENTIFIC_NAME),
    
    SAR_Richness = n_distinct(
      SCIENTIFIC_NAME[
        SARO_STATUS %in% c("END", "THR", "SC")
      ]
    ),
    
    SAR_END = n_distinct(
      SCIENTIFIC_NAME[SARO_STATUS %in% "END"]
    ),
    
    SAR_THR = n_distinct(
      SCIENTIFIC_NAME[SARO_STATUS %in% "THR"]
    ),
    
    SAR_SC = n_distinct(
      SCIENTIFIC_NAME[SARO_STATUS %in% "SC"]
    ),
    
    EO_Count = n_distinct(
      NHIC_EO_ID[
        SARO_STATUS %in% c("END", "THR", "SC")
      ],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# ------------------------------------------------------------
# JOIN TO PROVINCIAL GRID
# ------------------------------------------------------------

# Preserve all grid cells. Zero indicates no qualifying
# records, not confirmed species absence.

derived <- grid |>
  left_join(
    summary,
    by = c(
      "OGF_ID" = "PROV_TRK_SPECIES_1KM_GRID_ID"
    )
  ) |>
  mutate(
    across(
      c(
        Tracked_Richness,
        SAR_Richness,
        SAR_END,
        SAR_THR,
        SAR_SC,
        EO_Count
      ),
      ~ coalesce(.x, 0L)
    )
  ) |>
  st_transform(target_crs)

# ------------------------------------------------------------
# VALIDATE
# ------------------------------------------------------------

stopifnot(
  nrow(derived) == nrow(grid),
  !anyDuplicated(derived$OGF_ID),
  st_crs(derived)$epsg == target_crs,
  all(derived$SAR_Richness <= derived$Tracked_Richness),
  all(
    derived$SAR_Richness ==
      derived$SAR_END +
      derived$SAR_THR +
      derived$SAR_SC
  )
)

# ------------------------------------------------------------
# EXPORT DERIVED GEOPACKAGE
# ------------------------------------------------------------

# Replace only the derived output. Original NHIC data and
# BCCI_Regional_Baseline.gpkg remain unchanged.

st_write(
  derived,
  output,
  layer = layer,
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------

cat("\nNHIC DERIVED SAR RICHNESS\n")
cat("-------------------------\n")
cat("Source grid cells:", nrow(grid), "\n")
cat("Source detail records:", nrow(detail), "\n")
cat("Included species records:", nrow(species), "\n")
cat("Derived grid cells:", nrow(derived), "\n")
cat("Cells with SAR:", sum(derived$SAR_Richness > 0), "\n")
cat("Maximum SAR richness:", max(derived$SAR_Richness), "\n")
cat("Maximum tracked richness:", max(derived$Tracked_Richness), "\n")
cat("CRS:", st_crs(derived)$epsg, "\n")
cat("Output:", output, "\n")
cat("Layer:", layer, "\n")
