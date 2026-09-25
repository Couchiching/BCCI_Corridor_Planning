# ============================================================
# LIO — COMPLETE HOSTED LAYER 28
# Source: Ontario Land Information Ontario
# CRS: NAD83 / UTM Zone 17N (EPSG:26917)
# ============================================================

library(sf)
library(jsonlite)

base <- "C:/Users/ConservAnalyst/Documents/ConservationAnalyst"

gpkg <- file.path(
  base,
  "GISDatabase/CorridorMapping/Standardized",
  "BCCI_Regional_Baseline.gpkg"
)

service <- paste0(
  "https://ws.lioservices.lrc.gov.on.ca/arcgis2/rest/services/",
  "LIO_OPEN_DATA/LIO_Open06/MapServer/28/query"
)

# Retrieve all feature IDs
ids <- fromJSON(paste0(
  service, "?where=1%3D1&returnIdsOnly=true&f=json"
))$objectIds

stopifnot(length(ids) > 0)

# Download in batches to avoid URL length limits
features <- lapply(
  split(ids, ceiling(seq_along(ids) / 50)),
  function(x) {
    st_read(paste0(
      service,
      "?objectIds=", paste(x, collapse = ","),
      "&outFields=*&returnGeometry=true&f=geojson"
    ), quiet = TRUE)
  }
)

# Combine and reproject
data <- do.call(rbind, features) |>
  st_transform(26917)

# Verify complete retrieval
stopifnot(
  nrow(data) == length(ids),
  !anyDuplicated(data$OBJECTID)
)

# Save complete dataset
st_write(
  data,
  gpkg,
  layer = "ORM_Land_Use_Designation",
  delete_layer = TRUE,
  quiet = TRUE
)

cat("Features:", nrow(data), "\n")
cat("CRS:", st_crs(data)$epsg, "\n")
