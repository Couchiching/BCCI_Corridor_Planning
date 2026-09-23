
# ============================================================
# CONSERVATION CONTEXT — REGIONAL DATA COVERAGE AUDIT
#
# Tests existing conservation context datasets against:
#   CC  = Full CCRegion
#   KLT = KLT outside CCRegion
#   NLT = NLT outside CCRegion
#
# KLT and NLT are tested independently.
#
# Output:
# Short_Name | CC | KLT | NLT | All_3 | Notes
#
# Yes   = Spatial data present within test area
# No    = No spatial data detected
# Error = Dataset could not be evaluated
#
# This is a data-presence audit, not a completeness audit.
# ============================================================


# ============================================================
# 1. LIBRARIES AND PATHS
# ============================================================

library(sf)
library(terra)
library(dplyr)
library(readr)
library(purrr)
library(tibble)

root <- "C:/Users/ConservAnalyst/Documents/ConservationAnalyst"

inventory_path <- file.path(
  root,
  "GitHub/CouchichingConservationAnalystR/DataNeeded/Conservation_Context_Report_Layers.csv"
)

output_path <- file.path(
  root,
  "GISDatabase/Conservation_Context_Coverage_Audit.csv"
)

target_crs <- 26917


# ============================================================
# 2. OPERATIONAL AREAS
# ============================================================

areas <- list(
  
  CC = list(
    path = file.path(
      root,
      "GISDatabase/CouchichingData/CC_Acquisition.gdb"
    ),
    layer = "CCRegion"
  ),
  
  KLT = list(
    path = file.path(
      root,
      "GISDatabase - Copy/CouchichingData/Other_Orgs/KLT_QE2/KLT_Boundary.shp"
    ),
    layer = NULL
  ),
  
  NLT = list(
    path = file.path(
      root,
      "GISDatabase/Partner Data/NLT/NLT_Service_Area.gpkg"
    ),
    layer = NULL
  )
  
)


# ============================================================
# 3. READ AND PREPARE OPERATIONAL AREAS
# ============================================================

read_area <- function(x) {
  
  if (!file.exists(x$path)) {
    stop("Operational area not found: ", x$path)
  }
  
  if (is.null(x$layer)) {
    
    st_read(
      x$path,
      quiet = TRUE
    )
    
  } else {
    
    st_read(
      x$path,
      layer = x$layer,
      quiet = TRUE
    )
    
  }
  
}

# Read boundaries
boundaries <- map(
  areas,
  read_area
)

# Transform to common CRS and repair boundaries
boundaries <- map(
  boundaries,
  ~ st_transform(.x, target_crs) |>
    st_make_valid()
)

# Dissolve operational areas
CC <- st_union(boundaries$CC)
KLT <- st_union(boundaries$KLT)
NLT <- st_union(boundaries$NLT)

# Create test areas
test_areas <- list(
  
  CC = CC,
  
  KLT = st_difference(
    KLT,
    CC
  ),
  
  NLT = st_difference(
    NLT,
    CC
  )
  
)

# Verify test areas
area_summary <- tibble(
  Area = names(test_areas),
  
  Area_km2 = map_dbl(
    test_areas,
    ~ as.numeric(st_area(.x)) / 1e6
  )
)

print(area_summary)

if (any(map_lgl(test_areas, ~ all(st_is_empty(.x))))) {
  stop("One or more operational test areas are empty.")
}


# ============================================================
# 4. READ CONSERVATION CONTEXT INVENTORY
# ============================================================

inventory <- read_csv(
  inventory_path,
  show_col_types = FALSE,
  locale = locale(encoding = "Windows-1252")
) |>
  filter(
    !is.na(Short_Name),
    Short_Name != ""
  ) |>
  distinct(
    Short_Name,
    .keep_all = TRUE
  )

cat(
  "\nDatasets in inventory:",
  nrow(inventory),
  "\n"
)


# ============================================================
# 5. COVERAGE FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# VECTOR COVERAGE
# ------------------------------------------------------------

check_vector_coverage <- function(x, area) {
  
  # Remove missing and empty geometries
  x <- x[
    !is.na(st_geometry(x)) & !st_is_empty(x),
  ]
  
  if (nrow(x) == 0) {
    return(FALSE)
  }
  
  # Identify actual geometry type for each feature
  geom_type <- as.character(
    st_geometry_type(x)
  )
  
  # ----------------------------------------------------------
  # POLYGONS
  # ----------------------------------------------------------
  
  polygons <- x[
    geom_type %in% c("POLYGON", "MULTIPOLYGON"),
  ]
  
  if (nrow(polygons) > 0) {
    
    if (any(
      lengths(
        st_relate(
          polygons,
          area,
          pattern = "2********"
        )
      ) > 0
    )) {
      return(TRUE)
    }
    
  }
  
  # ----------------------------------------------------------
  # LINES
  # ----------------------------------------------------------
  
  lines <- x[
    geom_type %in% c("LINESTRING", "MULTILINESTRING"),
  ]
  
  if (nrow(lines) > 0) {
    
    if (any(
      lengths(
        st_relate(
          lines,
          area,
          pattern = "1********"
        )
      ) > 0
    )) {
      return(TRUE)
    }
    
  }
  
  # ----------------------------------------------------------
  # POINTS
  # ----------------------------------------------------------
  
  points <- x[
    geom_type %in% c("POINT", "MULTIPOINT"),
  ]
  
  if (nrow(points) > 0) {
    
    if (any(
      lengths(
        st_relate(
          points,
          area,
          pattern = "0********"
        )
      ) > 0
    )) {
      return(TRUE)
    }
    
  }
  
  # No qualifying intersection detected
  return(FALSE)
  
}


# ------------------------------------------------------------
# RASTER COVERAGE
# ------------------------------------------------------------

check_raster_coverage <- function(r, area) {
  
  area_vect <- terra::vect(
    st_sf(geometry = area)
  )
  
  area_vect <- terra::project(
    area_vect,
    terra::crs(r)
  )
  
  # Check bounding extent before cropping
  re <- terra::ext(r)
  ae <- terra::ext(area_vect)
  
  overlaps <- (
    terra::xmin(re) < terra::xmax(ae) &&
      terra::xmax(re) > terra::xmin(ae) &&
      terra::ymin(re) < terra::ymax(ae) &&
      terra::ymax(re) > terra::ymin(ae)
  )
  
  if (!overlaps) {
    return(FALSE)
  }
  
  # Crop to operational area
  r_crop <- terra::crop(
    r,
    area_vect
  )
  
  # Mask to actual area
  r_mask <- terra::mask(
    r_crop,
    area_vect
  )
  
  # Check for valid raster cells
  valid_cells <- terra::global(
    !is.na(r_mask[[1]]),
    "sum",
    na.rm = TRUE
  )
  
  isTRUE(valid_cells[1, 1] > 0)
  
}


# ============================================================
# 6. CHECK INDIVIDUAL DATASET
# ============================================================

check_layer <- function(Short_Name, file_path, Layer) {
  
  cat("\nChecking:", Short_Name, "\n")
  
  # Standard result structure
  make_result <- function(
    CC = "Error",
    KLT = "Error",
    NLT = "Error",
    Notes = ""
  ) {
    
    tibble(
      Short_Name = Short_Name,
      CC = CC,
      KLT = KLT,
      NLT = NLT,
      All_3 = ifelse(
        all(c(CC, KLT, NLT) == "Yes"),
        "Yes",
        ifelse(
          any(c(CC, KLT, NLT) == "Error"),
          "Error",
          "No"
        )
      ),
      Notes = Notes
    )
    
  }
  
  # Missing path
  if (
    is.na(file_path) ||
    !nzchar(trimws(file_path))
  ) {
    
    return(
      make_result(
        Notes = "No file path in inventory"
      )
    )
    
  }
  
  # Normalize Windows path
  file_path <- gsub(
    "\\\\",
    "/",
    file_path
  )
  
  # Missing file
  if (!file.exists(file_path)) {
    
    return(
      make_result(
        Notes = "File not found"
      )
    )
    
  }
  
  # Identify raster
  is_raster <- grepl(
    "\\.(tif|tiff|img|vrt)$",
    file_path,
    ignore.case = TRUE
  )
  
  # ----------------------------------------------------------
  # RUN SPATIAL TEST
  # ----------------------------------------------------------
  
  result <- tryCatch({
    
    if (is_raster) {
      
      # ======================================================
      # RASTER
      # ======================================================
      
      r <- terra::rast(file_path)
      
      coverage <- map_lgl(
        test_areas,
        ~ check_raster_coverage(r, .x)
      )
      
    } else {
      
      # ======================================================
      # VECTOR
      # ======================================================
      
      if (
        is.na(Layer) ||
        !nzchar(trimws(Layer))
      ) {
        
        x <- st_read(
          file_path,
          quiet = TRUE
        )
        
      } else {
        
        x <- st_read(
          file_path,
          layer = Layer,
          quiet = TRUE
        )
        
      }
      
      if (nrow(x) == 0) {
        stop("Dataset contains no features")
      }
      
      if (is.na(st_crs(x))) {
        stop("Dataset has no defined CRS")
      }
      
      # Remove missing and empty geometries
      x <- x[
        !is.na(st_geometry(x)) & !st_is_empty(x),
      ]
      
      if (nrow(x) == 0) {
        stop("No usable geometries")
      }
      
      # Transform to common CRS
      x <- st_transform(
        x,
        target_crs
      )
      
      # Repair only invalid geometries
      invalid <- !st_is_valid(
        x,
        NA_on_exception = TRUE
      )
      
      invalid[is.na(invalid)] <- FALSE
      
      if (any(invalid)) {
        
        x[invalid, ] <- st_make_valid(
          x[invalid, ]
        )
        
      }
      
      coverage <- map_lgl(
        test_areas,
        ~ check_vector_coverage(x, .x)
      )
      
    }
    
    # Format results
    make_result(
      CC = ifelse(coverage["CC"], "Yes", "No"),
      KLT = ifelse(coverage["KLT"], "Yes", "No"),
      NLT = ifelse(coverage["NLT"], "Yes", "No")
    )
    
  }, error = function(e) {
    
    make_result(
      Notes = paste(
        "Read/test error:",
        conditionMessage(e)
      )
    )
    
  })
  
  return(result)
  
}


# ============================================================
# 7. RUN COVERAGE AUDIT
# ============================================================

coverage_results <- pmap_dfr(
  inventory |>
    select(
      Short_Name,
      file_path,
      Layer
    ),
  check_layer
)


# ============================================================
# 8. EXPORT RESULTS
# ============================================================

write_csv(
  coverage_results,
  output_path,
  na = ""
)

cat(
  "\nCoverage audit complete.\n",
  "Output:",
  output_path,
  "\n"
)


# ============================================================
# 9. SUMMARY AND VALIDATION
# ============================================================

cat("\nOverall coverage:\n")

coverage_results |>
  count(All_3) |>
  print()


cat("\nCoverage by operational area:\n")

coverage_results |>
  summarise(
    Total = n(),
    CC = sum(CC == "Yes"),
    KLT = sum(KLT == "Yes"),
    NLT = sum(NLT == "Yes"),
    All_3 = sum(All_3 == "Yes")
  ) |>
  print()


cat("\nCC baseline failures:\n")

coverage_results |>
  filter(CC != "Yes") |>
  print(n = Inf, width = Inf)


cat("\nDatasets without coverage in all three areas:\n")

coverage_results |>
  filter(All_3 == "No") |>
  print(n = Inf, width = Inf)


cat("\nDatasets with errors:\n")

coverage_results |>
  filter(All_3 == "Error") |>
  print(n = Inf, width = Inf)

