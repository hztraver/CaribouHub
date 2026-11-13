library(terra)

# Input Paths
in_path <- "C:/Data/Drone/OrndahlOrthos/"
site <- "WICKERSHAM_" #CHANGE THIS TO THE SITE YOU ARE PROCESSING
year <- "2018" #CHANGE THIS TO THE YEAR YOU ARE PROCESSING

# Output & Intermediate Paths
siteyear <- "wickersham2018" #to follow 2025 naming structure
rgb_out <- paste0("C:/Data/Drone/OrthosClean/", siteyear, "_rgb_ortho.tif")
ms_out <- paste0("C:/Data/Drone/OrthosClean/", siteyear, "_ms_ortho.tif")

#construct full file paths
r_in  <- list.files(path = in_path, pattern = paste0("^", site, year, ".*_red\\.tif$"), full.names = TRUE)
g_in  <- list.files(path = in_path, pattern = paste0("^", site, year, ".*_green\\.tif$"), full.names = TRUE)
b_in  <- list.files(path = in_path, pattern = paste0("^", site, year, ".*_blue\\.tif$"), full.names = TRUE)
re_in <- list.files(path = in_path, pattern = paste0("^", site, year, ".*_red edge\\.tif$"), full.names = TRUE)
nir_in <- list.files(path = in_path, pattern = paste0("^", site, year, ".*_nir\\.tif$"), full.names = TRUE)

#load R, G, B, NIR, RE orthos
red <- rast(r_in)
blue <- rast(b_in)
green <- rast(g_in)
NIR <- rast(nir_in)
RE <- rast(re_in)

#raster stack RGB
rgb_stack <- c(red, green, blue)

#raster stack MS
ms_stack <- c(NIR, RE)

#export RGB and MS raster
writeRaster(rgb_stack, filename = rgb_out, overwrite = TRUE)
writeRaster(ms_stack, filename = ms_out, overwrite = TRUE)