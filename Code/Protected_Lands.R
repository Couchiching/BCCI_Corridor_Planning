library(sf)
library(dplyr)
library(purrr)

# ============================================================
# BCCI — REGIONAL CONSERVATION BASELINE
# ============================================================
# Partners: CC, NLT, KLT
# CRS: NAD83 / UTM Zone 17N (EPSG:26917)
#
# Outputs:
#   BCCI_Regional_Baseline_v2.gpkg
#   BCCI_Regional_Baseline_QA.csv
# ============================================================

# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

base <- "C:/Users/ConservAnalyst/Documents/ConservationAnalyst"

out <- file.path(base, "GISDatabase/CorridorMapping/Standardized")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

gpkg <- file.path(out, "BCCI_Regional_Baseline_v2.gpkg")

target_crs <- 26917

cc_gdb <- file.path(
  base, "GISDatabase/CouchichingData/CC_Acquisition.gdb"
)

index_path <- file.path(
  base,
  "GISDatabase/CouchichingData/MapSeries_Index/Master_Property_Index.gpkg"
)

# ------------------------------------------------------------
# 2. SOURCE DATASETS
# ------------------------------------------------------------

sources <- list(
  
  CC_Operational_Area = list(
    path = cc_gdb,
    layer = "CCRegion_OLTA_website"
  ),
  
  NLT_Operational_Area = list(
    path = file.path(
      base, "GISDatabase/Partner Data/NLT/NLT_Service_Area.gpkg"
    ),
    layer = "NLT_Service_Area"
  ),
  
  NLT_Properties = list(
    path = file.path(
      base, "GISDatabase/Partner Data/NLT/NLT_Land_Interest.gpkg"
    ),
    layer = "NLT_Land_Interest"
  ),
  
  KLT_Operational_Area = list(
    path = file.path(
      base, "GISDatabase/Partner Data/KLT/KLT_ServiceArea.shp"
    ),
    layer = "KLT_ServiceArea"
  ),
  
  KLT_Properties = list(
    path = file.path(
      base, "GISDatabase/Partner Data/KLT/KLT_ProtectedProperties.shp"
    ),
    layer = "KLT_ProtectedProperties"
  )
  
)

# ------------------------------------------------------------
# 3. READ AND STANDARDIZE
# ------------------------------------------------------------

read_standardized <- function(path, layer) {
  
  if (!file.exists(path)) {
    stop("Source not found: ", path)
  }
  
  x <- st_read(path, layer = layer, quiet = TRUE)
  
  if (is.na(st_crs(x))) {
    stop("Missing CRS: ", layer)
  }
  
  st_transform(x, target_crs)
}

data <- imap(
  sources,
  ~ read_standardized(.x$path, .x$layer)
)

# ------------------------------------------------------------
# KLT GEOMETRY REPAIR
# ------------------------------------------------------------
# Ston(e)y Lake Family Forest contains a ring self-intersection
# at X: 730055.2415, Y: 4939084.5446 (EPSG:26917).
#
# Identified by sf/GEOS and visually inspected in ArcGIS Pro
# on September 24, 2026.
#
# Repair applies only to the standardized working copy.
# Original KLT shapefile remains unchanged.
# ------------------------------------------------------------

klt <- data$KLT_Properties

invalid <- which(!st_is_valid(klt))

if (length(invalid) > 0) {
  klt[invalid, ] <- st_make_valid(klt[invalid, ])
}

stopifnot(all(st_is_valid(klt)))

data$KLT_Properties <- klt

# ------------------------------------------------------------
# 4. CC PROPERTIES — MASTER INDEX
# ------------------------------------------------------------

index <- read_standardized(index_path, "Properties")

data$CC_Properties <- index |>
  filter(
    trimws(Type) %in% c("Nature Reserve", "Easement")
  )

if (nrow(data$CC_Properties) == 0) {
  stop("No CC Nature Reserve or Easement records found.")
}

# ------------------------------------------------------------
# 5. COMBINED REGIONAL AOI
# ------------------------------------------------------------

areas <- bind_rows(
  map(
    data[c(
      "CC_Operational_Area",
      "NLT_Operational_Area",
      "KLT_Operational_Area"
    )],
    ~ st_sf(geometry = st_geometry(.x))
  )
)

areas <- st_make_valid(areas)

data$Regional_AOI <- st_sf(
  Name = "BCCI Regional Operational Area",
  geometry = st_union(st_geometry(areas))
)

# ------------------------------------------------------------
# 6. EXPORT GEOPACKAGE AND QA
# ------------------------------------------------------------

# Remove previous output to avoid retaining obsolete layers.
if (file.exists(gpkg)) {
  file.remove(gpkg)
}

summary <- imap_dfr(data, function(x, name) {
  
  st_write(
    x,
    gpkg,
    layer = name,
    append = FALSE,
    quiet = TRUE
  )
  
  y <- st_read(gpkg, layer = name, quiet = TRUE)
  
  valid <- st_is_valid(y, NA_on_exception = TRUE)
  
  tibble(
    Dataset = name,
    Features = nrow(y),
    EPSG = st_crs(y)$epsg,
    Empty = sum(st_is_empty(y)),
    Invalid = sum(valid == FALSE, na.rm = TRUE),
    Unknown_Validity = sum(is.na(valid)),
    CRS_OK = isTRUE(st_crs(y) == st_crs(target_crs))
  )
  
})

# ------------------------------------------------------------
# 7. QUALITY ASSURANCE
# ------------------------------------------------------------

print(summary, n = Inf, width = Inf)

write.csv(
  summary,
  file.path(out, "BCCI_Regional_Baseline_QA.csv"),
  row.names = FALSE
)

stopifnot(
  all(summary$CRS_OK),
  all(summary$Features > 0),
  all(summary$Empty == 0),
  all(summary$Invalid == 0),
  all(summary$Unknown_Validity == 0)
)

message("\nBCCI regional baseline complete: ", gpkg)
