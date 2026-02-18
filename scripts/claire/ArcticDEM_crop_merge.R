# Download and clip DEMs to study area

# --- 1. SETTINGS & PATHS ---
root_path <- "C:/Users/caeth/Documents/Data/ArcticDEM_FMCH"
if (!dir.exists(root_path)) dir.create(root_path, recursive = TRUE)
setwd(root_path)

# Path to the range file from earlier
range_path <- "C:/Users/caeth/Documents/Data/FMCH range shapefiles (from Mike Suitor)/FMCH_Range_ABoVE_Aligned.shp"

# Create output folder for raw tiles
tile_dir <- file.path(root_path, "ArcticDEM_tiles")
if (!dir.exists(tile_dir)) dir.create(tile_dir)

options(timeout = 999999)

# --- 2. PREPARE SPATIAL INDEX (LOCAL) ---

# Path to saved ArcticDEM index file
index_path <- "C:/Users/caeth/Documents/Data/Arctic DEMs_FMCH Range/ArcticDEM_Mosaic_Index_v4_1_10m.shp" 

# Load the local index
arctic_index <- st_read(index_path)

# Transform FMCH range to match the index (Index is usually WGS84 / EPSG:4326)
studyarea_wgs84 <- st_transform(FMCH_range, st_crs(arctic_index))

# Find the tiles that overlap with the Fortymile range
files <- st_intersection(arctic_index, studyarea_wgs84) %>%
  select(any_of(c("dem_id", "tile", "fileurl", "name")))

# Ensure dem_id exists for the download loop
if(!"dem_id" %in% names(files)) files$dem_id <- files$name

print(paste("Found", nrow(files), "tiles to download locally."))

# --- 3. DOWNLOAD & UNTAR ---

for(i in 1:nrow(files)){
  
  target_id <- files$dem_id[i]
  url <- as.character(files$fileurl[i])
  output_tif <- file.path(tile_dir, paste0(target_id, "_dem.tif"))
  
  if(file.exists(output_tif)){
    message(paste("Skipping", target_id, "- already exists."))
    next
  }
  
  message(paste("Downloading tile", i, "of", nrow(files), ":", target_id))
  
  tf <- tempfile(fileext = ".tar.gz")
  
  try({
    download.file(url, tf, mode = "wb")
    # Extract only the DEM file
    untar(tf, files = paste0(target_id, "_dem.tif"), exdir = tile_dir)
  })
  
  if(file.exists(tf)) unlink(tf) # Clean up temp tarball
}
