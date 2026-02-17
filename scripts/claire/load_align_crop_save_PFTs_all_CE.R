# ==============================================================================
# PROJECT: ABoVE PFT Processing for Fortymile Caribou Range
# PURPOSE: Load, Align, Crop, and Save PFT Rasters (2005-2020)
# ==============================================================================

library(sf)
library(terra)

# --- 1. SETTINGS & PATHS ---
root_path  <- "C:/Users/caeth/Documents/Data/ABOVE PFT 2005-2020/AK_Yukon_PFT_TopCover_2032_1.1-20260212_222829" 
setwd(root_path)

# Years to process
years_to_process <- c(2005, 2010, 2015, 2020)

# Create output folder for processed files
output_dir <- "Clipped_PFT_Data"
if (!dir.exists(output_dir)) dir.create(output_dir)

# --- 2. PREPARE SPATIAL DATA ---

# Load shapefile
FMCH_range <- st_read("Fortymile_Caribou_Expanded_Range_2013-2014_Final.shp")

# Grab Target CRS from one 2005 file (to ensure perfect alignment)
sample_file <- list.files(path = "ABoVE_PFT_2005", pattern = "\\.tif$", full.names = TRUE)[1]
target_crs  <- crs(rast(sample_file))

# Align shapefile to Raster CRS and convert to SpatVector for {terra}
FMCH_vect <- vect(st_transform(FMCH_range, target_crs))

# --- 3. DEFINE PROCESSING FUNCTION ---

process_pft_year <- function(year, base_path, study_area_vect) {
  
  # Identify year folder
  year_path <- file.path(base_path, paste0("ABoVE_PFT_", year))
  
  # Only files that contain 'Top_Cover' AND end in '.tif'
  tifs <- list.files(path = year_path, 
                     pattern = "Top_Cover.*\\.tif$", 
                     full.names = TRUE)
  
  # Safety check: ensure files were actually found
  if (length(tifs) == 0) {
    stop(paste("No Top_Cover .tif files found for year:", year))
  }
  
  # Load as SpatRaster stack
  s <- rast(tifs)
  
  # --- EXTRACT NAMES FROM FILENAMES ---
  # This takes "ABoVE_PFT_Top_Cover_Graminoid_2020.tif" and keeps only "Graminoid"
  clean_names <- basename(tifs) |> 
    gsub(pattern = paste0("ABoVE_PFT_Top_Cover_|_", year, ".*"), replacement = "")
  
  # Assign the cleaned names to the layers
  names(s) <- clean_names
  
  # Crop and mask
  s_masked <- crop(s, study_area_vect) |> 
    mask(study_area_vect)
  
  return(s_masked)
}
# --- 4. EXECUTION LOOP ---

for (yr in years_to_process) {
  message("Currently processing: ", yr, "...")
  
  # Process current year
  temp_stack <- process_pft_year(yr, root_path, FMCH_vect)
  
  # Save to disk (multi-layer GeoTIFF)
  out_name <- file.path(output_dir, paste0("PFT_masked_", yr, ".tif"))
  writeRaster(temp_stack, filename = out_name, overwrite = TRUE)
  
  # Clear memory before next year
  rm(temp_stack)
  gc()
}

message("Processing complete! Files are saved in: ", output_dir)

# Load the 2005 processed raster
pft_2005 <- rast("Clipped_PFT_Data/PFT_masked_2005.tif")

# Plot the whole stack (all layers at once)
plot(pft_2005)

# Load the 2020 processed raster
pft_2020 <- rast("Clipped_PFT_Data/PFT_masked_2020.tif")

# Plot the whole stack (all layers at once)
plot(pft_2020)
