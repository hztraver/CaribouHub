# ==============================================================================
# PROJECT: ABoVE PFT Processing for Fortymile Caribou Range
# PURPOSE: Load, Align, Crop, and Save PFT Rasters (2005-2020)
# ==============================================================================

library(sf)
library(terra)

# --- 1. SETTINGS & PATHS ---
# Years to process
years_to_process <- c(2005, 2010, 2015, 2020)

# Set output directory
output_dir <- "C:/Users/caeth/Documents/Data/ABOVE PFT 2005-2020/Clipped_PFT_Data"

# Define root path where ABoVE_PFT_YYYY folders are located
root_path  <- "C:/Users/caeth/Documents/Data/ABOVE PFT 2005-2020"

# --- 2. PREPARE SPATIAL DATA ---

# Load shapefile
fmch_range <- st_read("C:/Users/caeth/Documents/Data/FMCH range shapefiles (from Mike Suitor)/FMCH_Range_ABoVE_Aligned.shp")
plot(fmch_range)

# Grab Target CRS from a 2005 file to ensure 100% alignment
# (Adjust this path if your 2005 folder is named differently)
sample_2005_dir <- file.path(root_path, "ABoVE_PFT_2005")
sample_file <- list.files(path = sample_2005_dir, pattern = "\\.tif$", full.names = TRUE)[1]
target_crs  <- crs(rast(sample_file))

# Align and convert to SpatVector
FMCH_vect <- vect(st_transform(fmch_range, target_crs))

# --- 3. DEFINE PROCESSING FUNCTION ---

process_pft_year <- function(year, base_path, study_area_vect) {
  
# Identify year folder
year_path <- file.path(base_path, paste0("ABoVE_PFT_", year))

# Only files that contain 'Top_Cover' AND end in '.tif'
tifs <- list.files(path = year_path, 
                     pattern = "Top_Cover.*\\.tif$", 
                     full.names = TRUE)

# Ensure files were actually found
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

# Check the raw source folders to see why the output is too small
raw_2010 <- list.files("C:/Users/caeth/Documents/Data/ABOVE PFT 2005-2020/ABoVE_PFT_2010", pattern = "Top_Cover.*\\.tif$")
raw_2015 <- list.files("C:/Users/caeth/Documents/Data/ABOVE PFT 2005-2020/ABoVE_PFT_2015", pattern = "Top_Cover.*\\.tif$")


print(raw_2010)

print(raw_2015)
