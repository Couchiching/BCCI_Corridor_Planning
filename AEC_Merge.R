library(sf)
library(dplyr)

# ============================================================
# MERGE AEC REACHES — BCCI REGIONAL BASELINE
# ============================================================

base <- "C:/Users/ConservAnalyst/Documents/ConservationAnalyst/GISDatabase/Baselayers/AEC"

sources <- data.frame(
  region = c("w14", "w04", "w05"),
  package = c(
    "AEC_V3_GeoHub_Rev0_GPKG_Package04_LakeHuronSouth",
    "AEC_V3_GeoHub_Rev0_GPKG_Package02_LakeOntario",
    "AEC_V3_GeoHub_Rev0_GPKG_Package02_LakeOntario"
  ),
  folder = c(
    "w14_Georgian_Bay_South_Simcoe",
    "w04_Lake_Ontario_Central",
    "w05_Lake_Ontario_Kawarthas"
  )
)

# ------------------------------------------------------------
# READ AND STANDARDIZE
# ------------------------------------------------------------

# ------------------------------------------------------------
# MERGE AEC REACHES — w14, w04, w05
# ------------------------------------------------------------

reaches <- lapply(seq_len(nrow(sources)), function(i) {
  # ------------------------------------------------------------
  # MERGE AEC REACHES — w14, w04, w05
  # ------------------------------------------------------------
  
  reaches <- lapply(seq_len(nrow(sources)), function(i) {
    
    s <- sources[i, ]
    
    gpkg <- file.path(
      base,
      s$package,
      s$folder,
      paste0(s$region, "_AEC_V3_GeoHub_Rev0.gpkg")
    )
    
    layer <- paste0(
      s$region,
      "_Reach_SimpleGeometry_GeoHub"
    )
    
    # Verify source and layer
    if (!file.exists(gpkg)) {
      stop("GeoPackage not found: ", gpkg)
    }
    
    if (!layer %in% st_layers(gpkg)$name) {
      stop("Layer not found: ", layer)
    }
    
    # Read, reproject, and standardize geometry
    x <- st_read(gpkg, layer = layer, quiet = TRUE)
    
    if (is.na(st_crs(x))) {
      stop("Missing CRS: ", s$region)
    }
    
    x |>
      st_transform(26917) |>
      st_cast("MULTILINESTRING", warn = FALSE) |>
      mutate(Source_Region = s$region)
  })
  
  # Combine all three regions
  aec_reaches <- bind_rows(reaches)
  
  # Verify
  cat("Total reaches:", nrow(aec_reaches), "\n")
  print(table(aec_reaches$Source_Region))
  print(st_crs(aec_reaches)$epsg)

# ------------------------------------------------------------
# MERGE
# ------------------------------------------------------------

merged <- bind_rows(reaches)

# ------------------------------------------------------------
# EXPORT
# ------------------------------------------------------------

output <- file.path(
  base,
  "AEC_BCCI_Regional_Reaches.gpkg"
)

st_write(
  merged,
  output,
  layer = "AEC_BCCI_Regional_Reaches",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# VERIFY
# ------------------------------------------------------------

cat("\nMerged AEC reaches:\n")

print(
  merged |>
    st_drop_geometry() |>
    count(Source_Region)
)

cat("\nTotal reaches:", nrow(merged))
cat("\nCRS:", st_crs(merged)$input)
cat("\nOutput:", output, "\n")
