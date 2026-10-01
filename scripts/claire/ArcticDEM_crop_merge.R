library(terra)

# Load fmch range
fmch_range <- "C:/Users/caeth/Documents/Data/FMCH range shapefiles (from Mike Suitor)/FMCH_Range_ABoVE_Aligned.shp"

# Create output folder for raw tiles
tile_dir <- file.path(root_path, "ArcticDEM_tiles")
if (!dir.exists(tile_dir)) dir.create(tile_dir)

options(timeout = 999999)

# Path to saved ArcticDEM index file
index_path <- "C:/Users/caeth/Documents/Data/Arctic DEMs_FMCH Range/ArcticDEM_Mosaic_Index_v4_1_10m.shp" 

# Load the local index
arctic_index <- st_read(index_path)

# Transform FMCH range to match the index (Index is usually WGS84 / EPSG:4326)
studyarea_wgs84 <- st_transform(fmch_range, st_crs(arctic_index))

# Find the tiles that overlap with the Fortymile range
files <- st_intersection(arctic_index, studyarea_wgs84) %>%
  select(any_of(c("dem_id", "tile", "fileurl", "name")))

# Ensure dem_id exists for the download loop
if(!"dem_id" %in% names(files)) files$dem_id <- files$name

print(paste("Found", nrow(files), "tiles to download locally."))

# Download tiles

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



# List all the extracted DEM tiles
tif_list <- list.files(tile_dir, pattern = "_dem.tif$", full.names = TRUE)

# Create a Virtual Raster (VRT) 
# This "links" the tiles without using massive amounts of RAM
v_native <- vrt(tif_list, "FMCH_native_vrt.vrt", overwrite = TRUE)

# Define target CRS (from your Albers shapefile)
target_crs <- st_crs(FMCH_range)$wkt

# Project the whole mosaic to Canada Albers
dem_albers <- project(v_native, target_crs, method = "bilinear")

# Crop to the bounding box of the range
dem_cropped <- crop(dem_albers, vect(FMCH_range))

# Mask to the exact shape of the range (sets area outside range to NA)
dem_final <- mask(dem_cropped, vect(FMCH_range))
plot(dem_final)

# Save the final product
writeRaster(terrain_stack, "FMCH_ArcticDEM_Clipped_Albers.tif", overwrite = TRUE)