library(terra)

#I have done this but uncomment if we need to for unprocessed sites
#Rename Katie's orthos to match my (bad) naming structure. 
#Rename Katie's files to match 2025/26 naming structure
# Grab all files in the directory (both .tif and .tif.aux)
#old_files <- list.files(in_path, full.names = TRUE)
#
#for (old_file in old_files) {
#  file_name <- basename(old_file)
#  
#  # Only process files that match the long string
#  if (grepl("transparent_reflectance", file_name)) {
#    
#    # Extract Site (as lowercase) and Year
#    parts <- strsplit(file_name, "_")[[1]]
#    site <- tolower(parts[1]) 
#    year <- substr(parts[2], 1, 4)
#    
#    # Extract the Band and extension
#    band_and_ext <- sub(".*reflectance_", "", file_name) 
#    
#    # Construct the clean name (e.g., brownscreek2019_red.tif)
#    new_name <- paste0(site, year, "_", band_and_ext)
#    new_file <- file.path(in_path, new_name)
#    
#    # Rename the file in place
#    file.rename(from = old_file, to = new_file)
#  }
#}

# Input Paths
in_path <- "C:/Data/Drone/OrndahlOrthos/"
site <- "BROWNSCREEK" #CHANGE THIS TO THE SITE YOU ARE PROCESSING
year <- "2019" #CHANGE THIS TO THE YEAR YOU ARE PROCESSING

# Output & Intermediate Paths
rgb_out <- paste0("C:/Data/Drone/EthierOrthos/", site, year, "_rgb_ortho.tif")
ms_out <- paste0("C:/Data/Drone/EthierOrthos/", site, year, "_ms_ortho.tif")

#construct full file paths
r_in    <- paste0(in_path, site, year, "_red.tif")
g_in    <- paste0(in_path, site, year, "_green.tif")
b_in    <- paste0(in_path, site, year, "_blue.tif")
re_in   <- paste0(in_path, site, year, "_red edge.tif")
nir_in   <- paste0(in_path, site, year, "_nir.tif")

#load R, G, B, NIR, RE orthos
red <- rast(r_in)
green <- rast(g_in)
blue <- rast(b_in)
NIR <- rast(nir_in)
RE <- rast(re_in)

#raster stack RGB
rgb_stack <- c(red, green, blue)

#raster stack MS
ms_stack <- c(NIR, RE)

#export RGB and MS raster
writeRaster(rgb_stack, filename = rgb_out, overwrite = TRUE)
writeRaster(ms_stack, filename = ms_out, overwrite = TRUE)
