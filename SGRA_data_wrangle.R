# ============================================================
# ONTARIO SIGNIFICANT GROUNDWATER RECHARGE AREAS (SGRA)
# BCCI Regional Baseline — 30 km Buffer
# ============================================================
#
# SOURCE:
# Ontario Source Protection Information Atlas (SPIA 3.6)
# Government of Ontario — Source Protection / SPEM_Map
#
# DATA ACCESS:
# The SGRA dataset was located through the SPIA Geocortex
# REST directory rather than Ontario GeoHub.
#
# 1. Open the SPIA Geocortex REST directory:
#    https://www.lioapplications.lrc.gov.on.ca/Geocortex/Essentials/essentials414/REST/sites/SourceWaterProtection
#
# 2. Navigate to:
#    Map > Map Services > Water Quantity (Service 5) >
#    Layers > Significant Groundwater Recharge Area (Layer 29)
#
# 3. Open Map Service 5 with ?f=pjson to retrieve the
#    underlying ArcGIS REST service URL and temporary token:
#
#    https://www.lioapplications.lrc.gov.on.ca/Geocortex/Essentials/essentials414/REST/sites/SourceWaterProtection/map/mapservices/5?f=pjson
#
# 4. Query layer 29 using the ArcGIS REST API.
#
# ARCGIS REST SERVICE:
# https://ws.services.geospatial.gov.on.ca/arcgis1/rest/services/Source_Protection/SPEM_Map/MapServer/29
#
# METADATA:
# https://dev.lioapplications.lrc.gov.on.ca/Geocortex/Essentials/essentials414/REST/sites/SourceWaterProtection/viewers/SWPViewer/VirtualDirectory/Documents/Metadata/SWP_SIGNIF_GW_RECHARGE_AREA_FT.pdf
#
# METHOD:
# 1. Combine CC, KLT, and NLT operational areas.
# 2. Buffer combined operational areas by 30 km.
# 3. Query SGRA features intersecting the regional envelope.
# 4. Download individual features with checkpoints.
# 5. Retry failed geometry using 10 m generalization.
# 6. Standardize CRS and combine downloaded features.
# 7. Retain complete polygons intersecting the actual buffer.
# 8. Validate and export to GeoPackage.
#
# DATA QUALITY:
# OBJECTID 45 (SPA_ID 28; Vulnerability Score 4) caused
# ArcGIS HTTP 500 errors when requesting full geometry.
# Successfully recovered using maxAllowableOffset = 10.
# This feature has generalized geometry.
#
# Previous successful retrieval (2026-09-23):
# Provincial features: 12,295
# Regional envelope candidates: 53
# Final regional features: 33
#
# Output CRS: EPSG:26917 (NAD83 / UTM Zone 17N)
#
# NOTE:
# Authentication tokens are temporary. Retrieve a fresh
# token from Geocortex when needed.
# Do not commit tokens to GitHub.
#
# ============================================================


library(sf)
library(httr)
library(jsonlite)


# ------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------

base <- paste0(
  "C:/Users/ConservAnalyst/Documents/",
  "ConservationAnalyst/GISDatabase"
)

baseline <- file.path(
  base,
  "CorridorMapping/Standardized/BCCI_Regional_Baseline.gpkg"
)

output <- file.path(
  base,
  "Baselayers/SourceWaterProtection/SWP_SGRA_BCCI.gpkg"
)

checkpoint <- file.path(
  dirname(output),
  "SGRA_download_checkpoint"
)

service <- paste0(
  "https://ws.services.geospatial.gov.on.ca/arcgis1/rest/services/",
  "Source_Protection/SPEM_Map/MapServer/29"
)

target_crs <- 26917
buffer_m <- 30000

dir.create(
  dirname(output),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  checkpoint,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 2. AUTHENTICATION
# ------------------------------------------------------------

token_file <- file.path(
  base,
  "Baselayers/SourceWaterProtection/SGRA_token.txt"
)

token <- trimws(readLines(token_file, warn = FALSE)[1])

stopifnot(nzchar(token))

# ------------------------------------------------------------
# 3. COMBINE OPERATIONAL AREAS
# ------------------------------------------------------------

layers <- c(
  "CC_Operational_Area",
  "KLT_Operational_Area",
  "NLT_Operational_Area"
)

areas <- lapply(layers, function(layer) {
  
  x <- st_read(
    baseline,
    layer = layer,
    quiet = TRUE
  )
  
  st_transform(x, target_crs)
  
})

aoi <- st_union(
  do.call(
    c,
    lapply(areas, st_geometry)
  )
)

aoi <- st_make_valid(aoi)

# Buffer combined operational areas by 30 km
aoi_buffer <- st_buffer(aoi, buffer_m)

cat("Combined operational areas buffered by 30 km.\n")


# ------------------------------------------------------------
# 4. PREPARE ARCGIS SPATIAL QUERY
# ------------------------------------------------------------

# Use the bounding envelope for server-side filtering.
# The actual 30 km buffer is applied after downloading.

bbox <- st_bbox(
  st_transform(aoi_buffer, 4326)
)

envelope <- toJSON(
  list(
    xmin = unname(bbox["xmin"]),
    ymin = unname(bbox["ymin"]),
    xmax = unname(bbox["xmax"]),
    ymax = unname(bbox["ymax"]),
    spatialReference = list(wkid = 4326)
  ),
  auto_unbox = TRUE
)


# ------------------------------------------------------------
# 5. ARCGIS QUERY HELPER
# ------------------------------------------------------------

request <- function(query) {
  
  response <- POST(
    paste0(service, "/query"),
    body = c(
      query,
      list(
        token = token,
        f = "json"
      )
    ),
    encode = "form",
    timeout(180)
  )
  
  stop_for_status(response)
  
  result <- fromJSON(
    content(response, "text", encoding = "UTF-8")
  )
  
  if (!is.null(result$error)) {
    stop(
      "ArcGIS error ",
      result$error$code,
      ": ",
      result$error$message
    )
  }
  
  result
}


# ------------------------------------------------------------
# 6. RETRIEVE REGIONAL OBJECTIDs
# ------------------------------------------------------------

result <- request(list(
  where = "1=1",
  geometry = envelope,
  geometryType = "esriGeometryEnvelope",
  inSR = 4326,
  spatialRel = "esriSpatialRelIntersects",
  returnIdsOnly = "true"
))

ids <- sort(unique(result$objectIds))

if (!length(ids)) {
  stop("No SGRA features intersect the regional extent.")
}

cat(
  "Regional envelope candidates:",
  length(ids),
  "\n"
)


# ------------------------------------------------------------
# 7. DOWNLOAD INDIVIDUAL FEATURES
# ------------------------------------------------------------

download_feature <- function(id, generalize = FALSE) {
  
  query <- list(
    objectIds = id,
    outFields = "*",
    returnGeometry = "true",
    outSR = if (generalize) 26917 else 4326,
    f = "geojson",
    token = token
  )
  
  # Apply generalization only when full geometry fails
  if (generalize) {
    
    query$maxAllowableOffset <- 10
    query$geometryPrecision <- 1
    
  }
  
  response <- RETRY(
    "GET",
    paste0(service, "/query"),
    query = query,
    times = 3,
    pause_base = 2,
    timeout(180),
    quiet = TRUE
  )
  
  stop_for_status(response)
  
  txt <- content(
    response,
    "text",
    encoding = "UTF-8"
  )
  
  result <- fromJSON(
    txt,
    simplifyVector = FALSE
  )
  
  if (!is.null(result$error)) {
    stop(result$error$message)
  }
  
  if (!identical(result$type, "FeatureCollection")) {
    stop("Invalid GeoJSON response.")
  }
  
  x <- st_read(txt, quiet = TRUE)
  
  if (
    nrow(x) != 1 ||
    !identical(as.numeric(x$OBJECTID), as.numeric(id))
  ) {
    stop("Unexpected feature or OBJECTID.")
  }
  
  x
}


# ------------------------------------------------------------
# 8. DOWNLOAD WITH CHECKPOINTS AND GEOMETRY RECOVERY
# ------------------------------------------------------------

for (i in seq_along(ids)) {
  
  id <- ids[i]
  
  file <- file.path(
    checkpoint,
    paste0("SGRA_", id, ".rds")
  )
  
  # Skip previously downloaded features
  if (file.exists(file)) next
  
  cat(
    "Downloading",
    i,
    "of",
    length(ids),
    "| OBJECTID:",
    id,
    "\n"
  )
  
  # Attempt full-resolution geometry first
  x <- tryCatch(
    download_feature(id),
    error = function(e) {
      
      message(
        "Full geometry failed for OBJECTID ",
        id,
        ": ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
  
  # Retry with 10 m generalization if necessary
  if (is.null(x)) {
    
    message(
      "Retrying OBJECTID ",
      id,
      " with 10 m generalization."
    )
    
    x <- tryCatch(
      download_feature(id, generalize = TRUE),
      error = function(e) {
        
        message(
          "FAILED OBJECTID ",
          id,
          ": ",
          conditionMessage(e)
        )
        
        NULL
      }
    )
    
  }
  
  # Save successful download immediately
  if (!is.null(x)) {
    
    saveRDS(x, file)
    
  }
  
}


# ------------------------------------------------------------
# 9. VERIFY DOWNLOAD COMPLETENESS
# ------------------------------------------------------------

downloaded <- list.files(
  checkpoint,
  pattern = "^SGRA_[0-9]+\\.rds$",
  full.names = TRUE
)

downloaded_ids <- as.numeric(
  sub(
    "^SGRA_([0-9]+)\\.rds$",
    "\\1",
    basename(downloaded)
  )
)

missing_ids <- setdiff(ids, downloaded_ids)

cat("\nDownload summary:\n")
cat("Expected:", length(ids), "\n")
cat("Downloaded:", length(intersect(ids, downloaded_ids)), "\n")
cat("Missing:", length(missing_ids), "\n")

if (length(missing_ids)) {
  
  print(missing_ids)
  
  stop(
    "Download incomplete. Retry missing features before export."
  )
  
}

# Use only checkpoint files belonging to the current query
downloaded <- downloaded[
  downloaded_ids %in% ids
]


# ------------------------------------------------------------
# 10. STANDARDIZE CRS AND COMBINE
# ------------------------------------------------------------

features <- lapply(downloaded, function(file) {
  
  x <- readRDS(file)
  
  st_transform(x, target_crs)
  
})

sgra <- do.call(rbind, features)

stopifnot(
  nrow(sgra) == length(ids),
  !anyDuplicated(sgra$OBJECTID),
  setequal(sgra$OBJECTID, ids),
  st_crs(sgra)$epsg == target_crs
)

cat(
  "Combined features:",
  nrow(sgra),
  "\n"
)


# ------------------------------------------------------------
# 11. FILTER TO ACTUAL 30 KM BUFFER
# ------------------------------------------------------------

# Preserve complete SGRA polygon boundaries.
# Do not clip geometries to the buffer.

sgra <- st_filter(
  sgra,
  aoi_buffer,
  .predicate = st_intersects
)

cat(
  "Features intersecting actual buffer:",
  nrow(sgra),
  "\n"
)


# ------------------------------------------------------------
# 12. REPAIR AND VALIDATE GEOMETRIES
# ------------------------------------------------------------

original_ids <- sgra$OBJECTID

# Repair each feature without expanding the attribute rows
geom <- lapply(st_geometry(sgra), function(g) {
  
  x <- st_sfc(g, crs = st_crs(sgra))
  
  x <- st_make_valid(x)
  
  # Retain all polygon components
  x <- st_collection_extract(x, "POLYGON", warn = FALSE)
  
  # Combine components into one geometry per OBJECTID
  x <- st_combine(x)
  
  st_cast(x, "MULTIPOLYGON", warn = FALSE)[[1]]
  
})

st_geometry(sgra) <- st_sfc(
  geom,
  crs = st_crs(sgra)
)

stopifnot(
  identical(sgra$OBJECTID, original_ids),
  !anyDuplicated(sgra$OBJECTID),
  all(st_is_valid(sgra)),
  !any(st_is_empty(sgra)),
  all(st_geometry_type(sgra) == "MULTIPOLYGON"),
  st_crs(sgra)$epsg == target_crs
)

cat("Repaired features:", nrow(sgra), "\n")
cat("All geometries valid:", all(st_is_valid(sgra)), "\n")


# ------------------------------------------------------------
# 13. EXPORT AND VERIFY
# ------------------------------------------------------------

st_write(
  sgra,
  output,
  layer = "SWP_SGRA",
  delete_dsn = TRUE,
  quiet = TRUE
)

saved <- st_read(
  output,
  layer = "SWP_SGRA",
  quiet = TRUE
)

stopifnot(
  nrow(saved) == nrow(sgra),
  setequal(saved$OBJECTID, original_ids),
  !anyDuplicated(saved$OBJECTID),
  all(st_is_valid(saved)),
  !any(st_is_empty(saved)),
  all(st_geometry_type(saved) == "MULTIPOLYGON"),
  st_crs(saved)$epsg == target_crs
)

cat("\nSUCCESS\n")
cat("Saved features:", nrow(saved), "\n")
cat("All geometries valid:", all(st_is_valid(saved)), "\n")
cat("CRS:", st_crs(saved)$epsg, "\n")
cat("Output:", normalizePath(output), "\n")