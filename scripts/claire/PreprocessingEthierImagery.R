library(terra)

ethier_preprocess_RGB <- function(site_name, year){
  #Set paths
  in_path  <- "C:/Data/Drone/EthierOrthos/"
  out_path <- "C:/Data/Drone/OrthosClean/"
  
  # paths to RGB and MS .tif
  rgb_files <- paste0(in_path, site_name, year, "_rgb_ortho.tif")
  rgb_out <-  paste0(out_path, site_name, year, "_rgb_ortho.tif")
  
  
  # Process RGB + MS files 
  rgb_rast <- rast(rgb_files)[[1:3]]
  writeRaster(rgb_rast, filename = paste0(out_path, site_name, year, "_rgb_ortho.tif"), overwrite = TRUE)
  
    return(RGB_rast)
  }

#Test Fucnction
ethier_preprocess_RGB(site_name, year)

#ETHIER MS PRE-PROCESS
ethier_preprocess_MS <- function(site_name, year){
  #Set paths
  in_path  <- "C:/Data/Drone/EthierOrthos/"
  out_path <- "C:/Data/Drone/OrthosClean/"
  
  # paths to RGB and MS .tif
  ms_files  <- paste0(in_path, site_name, year, "_ms_ortho.tif")
  ms_out <-  paste0(out_path, site_name, year, "_msce_ortho.tif")
  
  # Process RGB + MS files 
  ms_rast <- rast(ms_files)[[3:4]]
  writeRaster(ms_rast, filename = paste0(out_path, site_name, year, "_ms_ortho.tif"), overwrite = TRUE)
  
  return(MS_rast)
}

ethier_preprocess_MS(site_name, year)