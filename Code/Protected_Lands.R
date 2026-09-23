
library(sf)
library(dplyr)
library(purrr)

# ============================================================
# BCCI — REGIONAL CONSERVATION BASELINE
# ============================================================
# Partners: Couchiching Conservancy (CC)
#           Northumberland Land Trust (NLT)
#           Kawartha Land Trust (KLT)
#
# CRS: NAD83 / UTM Zone 17N (EPSG:26917)
#
# Outputs:
#   BCCI_Regional_Baseline.gpkg
#   BCCI_Regional_Baseline_QA.csv
#
# KLT properties are provisional (CPCAD 2025).
# Replace with partner-provided data when available.
# ============================================================

# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

base <- "C:/Users/ConservAnalyst/Documents/ConservationAnalyst"

out <- file.path(base, "GISDatabase/CorridorMapping/Standardized")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

gpkg <- file.path(
  out,
  "BCCI_Regional_Baseline_v2.gpkg"
)
target_crs <- 26917

cc_gdb <- file.path(
  base, "GISDatabase/CouchichingData/CC_Acquisition.gdb"
)

index_path <- file.path(
  base,
  "GISDatabase/CouchichingData/MapSeries_Index/Master_Property_Index.gpkg"
)

cpcad_gdb <- file.path(
  base,
  "GISDatabase/Baselayers/Canadian_Protected_Conserved_Areas_CPCAD",
  "ProtectedConservedArea_2025/ProtectedConservedArea_2025.gdb"
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
      base,
      "GISDatabase - Copy/CouchichingData/Other_Orgs/KLT_QE2/KLT_Boundary.shp"
    ),
    layer = "KLT_Boundary"
  )
)

# ------------------------------------------------------------
# 3. READ AND STANDARDIZE
# ------------------------------------------------------------

read_standardized <- function(path, layer) {
  
  x <- st_read(path, layer = layer, quiet = TRUE)
  
  if (is.na(st_crs(x))) {
    stop("Missing CRS: ", layer)
  }
  
  st_transform(x, target_crs)
}

# Read operational areas and NLT properties.
data <- imap(
  sources,
  ~ read_standardized(.x$path, .x$layer)
)

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
# 5. CPCAD 2025
# ------------------------------------------------------------

cpcad <- read_standardized(
  cpcad_gdb,
  "ProtectedConservedArea_2025"
)

# Provisional KLT holdings: ownership or management.
data$KLT_Properties <- cpcad |>
  filter(
    if_any(
      any_of(c("OWNER_E", "MGMT_E")),
      ~ grepl(
        "Kawartha Land Trust",
        coalesce(as.character(.x), ""),
        ignore.case = TRUE
      )
    )
  )

if (nrow(data$KLT_Properties) == 0) {
  stop("No KLT properties identified in CPCAD.")
}

cat("\nKLT properties identified:\n")

data$KLT_Properties |>
  st_drop_geometry() |>
  as_tibble() |>
  select(any_of(c(
    "NAME_E", "OWNER_E", "MGMT_E", "STATUS"
  ))) |>
  print(n = Inf, width = Inf)


# ------------------------------------------------------------
# 6. COMBINED REGIONAL AOI
# ------------------------------------------------------------

# Combine the three operational areas.
# Retain individual boundaries in their original layers.

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

# Repair geometries before union.
areas <- st_make_valid(areas)

regional_aoi <- st_sf(
  Name = "BCCI Regional Operational Area",
  geometry = st_union(st_geometry(areas))
)

data$Regional_AOI <- regional_aoi


# ------------------------------------------------------------
# 7. REGIONAL CPCAD BASELINE
# ------------------------------------------------------------

# Select protected/conserved areas intersecting the AOI.
# Retain complete polygons rather than clipping boundaries.

data$Regional_CPCAD <- cpcad[
  lengths(st_intersects(cpcad, regional_aoi)) > 0,
]

# ------------------------------------------------------------
# 8. EXPORT GEOPACKAGE
# ------------------------------------------------------------

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
# 9. QUALITY ASSURANCE
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
