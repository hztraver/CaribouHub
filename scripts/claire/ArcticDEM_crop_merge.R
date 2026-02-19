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

# --- 4. MOSAIC, PROJECT, & CLIP ---
library(terra)

# 1. List all the extracted DEM tiles
tif_list <- list.files(tile_dir, pattern = "_dem.tif$", full.names = TRUE)

# 2. Create a Virtual Raster (VRT) 
# This "links" the tiles without using massive amounts of RAM
v_native <- vrt(tif_list, "FMCH_native_vrt.vrt", overwrite = TRUE)

# 3. Project and Clip to the FMCH_range
# We do this in one step using the 'mask' and 'crop' functions
message("Projecting and clipping DEM to the FMCH range...")

# Define target CRS (from your Albers shapefile)
target_crs <- st_crs(FMCH_range)$wkt

# A. Project the whole mosaic to Canada Albers
dem_albers <- project(v_native, target_crs, method = "bilinear")

# B. Crop to the bounding box of the range
dem_cropped <- crop(dem_albers, vect(FMCH_range))

# C. Mask to the exact shape of the range (sets area outside range to NA)
dem_final <- mask(dem_cropped, vect(FMCH_range))
plot(dem_final)

# --- 5. CALCULATE TERRAIN & SAVE ---

message("Calculating slope and aspect for the clipped area...")
terrain_stack <- c(dem_final, terrain(dem_final, v = c("slope", "aspect")))
names(terrain_stack) <- c("elevation", "slope", "aspect")

# Save the final product
writeRaster(terrain_stack, "FMCH_ArcticDEM_Clipped_Albers.tif", overwrite = TRUE)

print("Processing complete! Your DEM is now aligned with your caribou range and ABoVE data.")