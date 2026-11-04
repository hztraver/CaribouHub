library(terra)

#Set paths
in_path  <- "C:/Data/Drone/EthierOrthos/"
out_path <- "C:/Data/Drone/OrthosClean/"

siteyear <- "swede2025" #CHANGE THIS TO THE SITEYEAR YOU ARE PROCESSING

# paths to RGB and MS .tif
rgb_files <- paste0(in_path, siteyear, "_rgb_ortho.tif")
rgb_out <-  paste0(out_path, siteyear, "_rgb_ortho.tif")
ms_files  <- paste0(in_path, siteyear, "_ms_ortho.tif")
ms_out <-  paste0(out_path, siteyear, "_ms_ortho.tif")

# Process RGB + MS files 
rgb_rast <- rast(rgb_files)[[1:3]]
writeRaster(rgb_rast, filename = paste0(out_path, siteyear, "_rgb_ortho.tif"), overwrite = TRUE)

ms_rast <- rast(ms_files)[[3:4]]
writeRaster(ms_rast, filename = paste0(out_path, siteyear, "_ms_ortho.tif"), overwrite = TRUE)
