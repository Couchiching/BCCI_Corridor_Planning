
# ============================================================
# BCCI REGIONAL BASELINE — AOI AND SOLRIS V3
#
# Purpose:
#   1. Combine CC, KLT, and NLT operational areas
#   2. Generate a 50 km regional buffer
#   3. Download and extract SOLRIS v3
#   4. Clip SOLRIS to the regional AOI
#
# Outputs:
#   BCCI_Regional_Baseline.gpkg (Regional_AOI updated)
#   SOLRIS_v3_BCCI.tif
#
# Requirements: sf, terra
# Source: Ontario SOLRIS Version 3.0 (2000–2015)
# ============================================================

library(sf)
library(terra)

# ------------------------------------------------------------
# 1. PATHS AND SETTINGS
# ------------------------------------------------------------

base <- "C:/Users/ConservAnalyst/Documents/ConservationAnalyst/GISDatabase"

gpkg <- file.path(
  base,
  "CorridorMapping/Standardized/BCCI_Regional_Baseline.gpkg"
)

solris_dir <- file.path(base, "Baselayers/SOLRIS_v3")

zip_file <- file.path(solris_dir, "SOLRIS_Version_3_0.zip")

raster_name <- "SOLRIS_Version_3_0_LAMBERT.tif"

source_raster <- file.path(solris_dir, raster_name)

output <- file.path(
  dirname(gpkg),
  "SOLRIS_v3_BCCI.tif"
)

download_url <- paste0(
  "https://ws.gisetl.lrc.gov.on.ca/",
  "fmedatadownload/Packages/SOLRIS_Version_3_0.zip"
)

operational_layers <- c(
  "CC_Operational_Area",
  "KLT_Operational_Area",
  "NLT_Operational_Area"
)

target_crs <- 26917
buffer_m <- 50000

dir.create(solris_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(gpkg)) {
  stop("Baseline GeoPackage not found: ", gpkg)
}

# ------------------------------------------------------------
# 2. BUILD REGIONAL AOI
# ------------------------------------------------------------

areas <- lapply(operational_layers, function(layer) {
  
  x <- st_read(gpkg, layer = layer, quiet = TRUE)
  
  x |>
    st_transform(target_crs) |>
    st_make_valid() |>
    st_geometry()
  
})

combined <- do.call(c, areas)

regional <- st_union(combined) |>
  st_buffer(buffer_m) |>
  st_make_valid()

regional <- st_sf(
  Name = "BCCI Regional AOI",
  Buffer_km = buffer_m / 1000,
  geometry = regional
)

# Replace only the Regional_AOI layer
st_write(
  regional,
  gpkg,
  layer = "Regional_AOI",
  delete_layer = TRUE,
  quiet = TRUE
)

cat("Regional AOI updated.\n")
cat(
  "Area (km2):",
  round(as.numeric(st_area(regional)) / 1e6, 1),
  "\n"
)

# ------------------------------------------------------------
# 3. DOWNLOAD SOLRIS V3
# ------------------------------------------------------------

if (!file.exists(source_raster)) {
  
  # Download archive if necessary
  if (!file.exists(zip_file)) {
    
    options(timeout = 3600)
    
    download.file(
      download_url,
      destfile = zip_file,
      mode = "wb",
      method = "libcurl"
    )
    
  }
  
  # Verify raster is present in archive
  contents <- unzip(zip_file, list = TRUE)
  
  if (!raster_name %in% contents$Name) {
    stop("SOLRIS raster not found in ZIP archive.")
  }
  
  # Extract only the required raster
  unzip(
    zip_file,
    files = raster_name,
    exdir = solris_dir
  )
  
}

if (!file.exists(source_raster)) {
  stop("SOLRIS source raster not found.")
}

# ------------------------------------------------------------
# 4. READ SOURCE RASTER AND AOI
# ------------------------------------------------------------

solris <- rast(source_raster)

aoi <- st_read(
  gpkg,
  layer = "Regional_AOI",
  quiet = TRUE
)

# Transform AOI to original SOLRIS projection
aoi <- st_transform(aoi, crs(solris))

# ------------------------------------------------------------
# 5. CROP AND MASK
# ------------------------------------------------------------

solris_bcci <- crop(
  solris,
  vect(aoi),
  mask = TRUE,
  snap = "out"
)

# ------------------------------------------------------------
# 6. EXPORT REGIONAL RASTER
# ------------------------------------------------------------

writeRaster(
  solris_bcci,
  output,
  overwrite = TRUE,
  gdal = c(
    "COMPRESS=LZW",
    "TILED=YES"
  )
)

# ------------------------------------------------------------
# 7. VERIFY OUTPUT
# ------------------------------------------------------------

result <- rast(output)

cat("\nSOLRIS extraction complete.\n")
cat("Output:", output, "\n")
cat("Resolution (m):", res(result), "\n")
cat("CRS:", crs(result, describe = TRUE)$name, "\n")
cat("Size (MB):", round(file.size(output) / 1024^2, 1), "\n")

cat("CRS preserved:", same.crs(result, solris), "\n")
cat("Resolution preserved:", identical(res(result), res(solris)), "\n")
