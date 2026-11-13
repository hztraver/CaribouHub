library(terra)
library(sf)
library(raster)
library(randomForest)
library(ggplot2)
library(caret)
library(xgboost)
library(dplyr)
library(tidyverse)
library(RCSF)
library(sp)
library(lidR)
library(future) # for parallel processing

###################################################################
#Pre-process Ethier surveys (keep only RGB iin RGB and NIR and RE in MS)
###################################################################
ethier_preprocess_RGB <- function(site_name, year){
  #Set paths
  in_path  <- "C:/Data/Drone/EthierOrthos/"
  out_path <- "C:/Data/Drone/OrthosClean/"
  
  # paths to RGB 
  rgb_files <- paste0(in_path, site_name, year, "_rgb_ortho.tif")
  rgb_out <-  paste0(out_path, site_name, year, "_rgb_ortho.tif")
  
  
  # Process RGB 
  rgb_rast <- rast(rgb_files)[[1:3]]
  writeRaster(rgb_rast, filename = paste0(out_path, site_name, year, "_rgb_ortho.tif"), overwrite = TRUE)
  
  return(rgb_rast)
}

ethier_preprocess_MS <- function(site_name, year){
  #Set paths
  in_path  <- "C:/Data/Drone/EthierOrthos/"
  out_path <- "C:/Data/Drone/OrthosClean/"
  
  # paths to RGB and MS .tif
  ms_files  <- paste0(in_path, site_name, year, "_ms_ortho.tif")
  ms_out <-  paste0(out_path, site_name, year, "_ms_ortho.tif")
  
  # Process RGB + MS files 
  ms_rast <- rast(ms_files)[[3:4]]
  writeRaster(ms_rast, filename = paste0(out_path, site_name, year, "_ms_ortho.tif"), overwrite = TRUE)
  
  return(ms_rast)
}

#########################################################################
#Pre-process Orndahl surveys (keep only RGB iin RGB and NIR and RE in MS)
#########################################################################
orndahl_preprocess_RGB <- function(site_name, year){
  # Set paths
  in_path  <- "C:/Data/Drone/OrndahlOrthos/"
  out_path <- "C:/Data/Drone/OrthosClean/"
  
  # Paths to RGB out
  rgb_out <- paste0(out_path, site_name, year, "_rgb_ortho.tif")
  
  # Find raw single bands (converts site_name to uppercase for the search, e.g., "WICKERSHAM_")
  site_prefix <- paste0(toupper(site_name), "_")
  r_in <- list.files(in_path, pattern = paste0("^", site_prefix, year, ".*_red\\.tif$"), full.names = TRUE)
  g_in <- list.files(in_path, pattern = paste0("^", site_prefix, year, ".*_green\\.tif$"), full.names = TRUE)
  b_in <- list.files(in_path, pattern = paste0("^", site_prefix, year, ".*_blue\\.tif$"), full.names = TRUE)
  
  # Process RGB 
  rgb_rast <- c(rast(r_in), rast(g_in), rast(b_in))
  names(rgb_rast) <- c("Red", "Green", "Blue")
  
  writeRaster(rgb_rast, filename = rgb_out, overwrite = TRUE)
  
  return(rgb_rast)
}

orndahl_preprocess_MS <- function(site_name, year){
  # Set paths
  in_path  <- "C:/Data/Drone/OrndahlOrthos/"
  out_path <- "C:/Data/Drone/OrthosClean/"
  
  # Paths to MS out
  ms_out <- paste0(out_path, site_name, year, "_ms_ortho.tif")
  
  # Find raw single bands
  site_prefix <- paste0(toupper(site_name), "_")
  re_in  <- list.files(in_path, pattern = paste0("^", site_prefix, year, ".*_red edge\\.tif$"), full.names = TRUE)
  nir_in <- list.files(in_path, pattern = paste0("^", site_prefix, year, ".*_nir\\.tif$"), full.names = TRUE)
  
  # Process MS 
  ms_rast <- c(rast(re_in), rast(nir_in))
  names(ms_rast) <- c("RedEdge", "NIR")
  
  writeRaster(ms_rast, filename = ms_out, overwrite = TRUE)
  
  return(ms_rast)
}

###################################################################
#BUILD CHM FROM POINT CLOUD FOR ETHIER MODELS
###################################################################
chm_function <- function(site_name, year, dir = chm_dir) {
  # Setup paths
  raw_dir <- "C:/Data/Drone/PointClouds"
  ground_dir <- file.path(dir, "output", "GROUND")
  norm_dir <- file.path(dir, "output", "NORM")
  raster_dir <- file.path(dir, "output", "RASTER")
  
  # Read LAS files
  search_pattern <- paste0("^", site_name, "_pointcloud_", year, ".*")
  las_files <- list.files("C:/Data/Drone/PointClouds", pattern = search_pattern, full.names = TRUE)
  LCT <- readLAScatalog(las_files)
  
  #### GROUND CLASSIFICATION ####
  # classify ground/canopy from the filtered point cloud
  # using cloth simulation function
  
  ## options for reading in the LAS Catalog
  opt_chunk_buffer(LCT) = 10 # buffer each tile 10m
  opt_laz_compression(LCT) = TRUE # write output as .laz 
  
  # Point cloud thinning to speed things up by taking a random point within each 0.1m voxel
  # consider using higher/lower resolution voxel depending on point cloud density and computing power
  opt_filter(LCT) = "-thin_with_voxel 0.1" 
  
  # output naming convention will keep the tiling system
  opt_output_files(LCT) = paste0(dir, "\\output\\GROUND\\{*}_GROUND") 
  opt_progress(LCT) = TRUE
  opt_filter(LCT) = "-thin_with_voxel 0.1"
  print(LCT)
  # set up and execute task in parallel (2 cores) 
  plan(multisession, workers = 6L)
  
  # read in the LAS Catalog -> create new LAS Catalog of classified points called GROUND 
  GROUND = classify_ground(LCT, algorithm = csf(
    # in case there are steep slopes 
    sloop_smooth = TRUE,
    # minimum height threshold for canopy points
    class_threshold = 0.5,
    # larger resolution = coarser DTM
    cloth_resolution = 1,
    # rigidness of cloth - higher = more rigid
    rigidness = 3L,
    iterations = 500L,
    time_step = 1)) 
  
  # always stop the parallel processing!
  plan(sequential)
  
  #### CREATE A DEM FROM THE GROUND CLASSIFIED POINTS ####
  
  # read in the LAS Catalog of GROUND classified points
  #GROUND = readLAScatalog(folder = paste0(dir, "\\output\\GROUND\\"))
  opt_chunk_buffer(GROUND) = 15
  
  # drop noisy points below the ground
  opt_filter(GROUND) = "-drop_z_below -0.25"
  opt_output_files(GROUND) = "" # no LAS output
  opt_progress(GROUND) = TRUE
  
  # create a 1m DEM from the GROUND points 
  # using k-nn inverse distance weighting interpolation
  #DEM = grid_terrain(GROUND, res = 1, knnidw(), full_raster = TRUE)
  
  #### NORMALIZATION ####
  # normalize the point cloud to the ground classified points
  
  # read the classified point cloud in with high-res parameters
  ground_files <- list.files("C:/Data/Drone/CanopyHeightModels/output/GROUND", pattern = search_pattern, full.names = TRUE)
  GROUND = readLAScatalog(ground_files)
  plot(GROUND)
  # small buffer; we're just normalizing
  opt_chunk_buffer(GROUND) = 1
  opt_laz_compression(LCT) = TRUE
  
  # output normalized point cloud to NORM folder
  opt_output_files(GROUND) = paste0(dir, "\\output\\NORM\\{*}_NORM")
  opt_progress(GROUND) = TRUE
  
  # normalize the point cloud to the GROUND points
  # or alternatively to the rasterized DEM - a bit faster but less "exact"
  # NORM = normalize_height(LCT, DEM, knnidw(), na.rm = TRUE)
  plan(multisession, workers = 6L)
  NORM = normalize_height(GROUND, knnidw(), na.rm = TRUE)
  plan(sequential)
  
  ## Create rasterized CHM from normalized point cloud
  norm_files <- list.files("C:/Data/Drone/CanopyHeightModels/output/NORM", pattern = search_pattern, full.names = TRUE)
  NORM = readLAScatalog(norm_files)
  # no buffer
  opt_chunk_buffer(NORM) = 0
  opt_laz_compression(NORM) = TRUE
  
  # drop noisy points below the ground or floating in outer space
  opt_filter(NORM) = "-drop_z_below -.25 -drop_z_above 30"
  opt_output_files(NORM) = ""
  opt_progress(NORM) = TRUE
  
  # Create a 10cm CHM, expanding each point with a 50cm radius disc to fill holes
  plan(multisession, workers = 6L)
  CHM_max = rasterize_canopy(NORM, res = 0.10, algorithm = p2r(subcircle = 0.5))
  plan(sequential)
  
  ttops_chm <- locate_trees(CHM_max, lmf(ws = 5, hmin = 2))
  ttops_buffered <- st_buffer(ttops_chm, dist = 1)
  ttops_vect <- vect(ttops_buffered)
  
  chm_mask <- mask(CHM_max, ttops_vect, inverse = TRUE)
  
  # Export the CHM raster to a GeoTIFF file
  writeRaster(chm_mask, 
              filename = paste0(dir, "/output/RASTER/", site_name, year, "_CHM_TreeMasked", ".tif"), 
              overwrite = TRUE)
  return(chm_mask)
  
}

###################################################################
#MASK TREES FROM ORNDAHL CHMs
###################################################################
chm_tree_mask <- function(site_name, year) {
  
  # Set directory
  chm_dir <- "C:/Data/Drone/CanopyHeightModels"
  
  # Read CHM (converts site_name to ALL CAPS to match the raw file)
  chm_filename <- paste0(toupper(site_name), year, "_CHM_final.tif")
  chm_path <- file.path(chm_dir, chm_filename)
  CHM <- rast(chm_path)
  
  # Mask trees
  ttops_chm <- locate_trees(CHM, lmf(ws = 5, hmin = 2))
  ttops_buffered <- st_buffer(ttops_chm, dist = 1)
  ttops_vect <- vect(ttops_buffered)
  
  chm_mask <- mask(CHM, ttops_vect, inverse = TRUE)
  
  # Export the masked CHM (keeps standard site_name formatting for the output)
  out_path <- file.path(chm_dir, "output", "RASTER", paste0(site_name, year, "_CHM_TreeMasked.tif"))
  writeRaster(chm_mask, filename = out_path, overwrite = TRUE)
  
  return(chm_mask)
}

###################################################################
#ALIGN, CLIP & STACK RASTERS
###################################################################

align_rasters <- function(nir_path, rgb_path, chm_path, clip_extent, out_dir, site_name) {
  # This function aligns three rasters by clipping, cropping and reprojecting
  
  rgb_rast <- rast(rgb_path)
  nir_rast <- rast(nir_path)
  chm_rast <- rast(chm_path)
  
  nir_projected <- project(nir_rast, chm_rast, method = "bilinear")
  rgb_projected <- project(rgb_rast, chm_rast, method = "bilinear")
  
  # clip and resample rasters to the same resolution before stacking
  nir_aligned <- resample(nir_projected, rgb_projected, method="bilinear")
  chm_aligned <- resample(chm_rast, rgb_projected, method="bilinear")
  
  # Conditionally crop if the cutline exists
  if (!is.null(clip_extent) && file.exists(clip_extent)) {
    cutline <- vect(clip_extent)
    
    nir_final <- crop(nir_aligned, cutline, mask = TRUE)
    chm_final <- crop(chm_aligned, cutline, mask = TRUE)
    rgb_final <- crop(rgb_projected, cutline, mask = TRUE)
    
   } else {
    # If no cutline exists, just pass the aligned rasters forward
    nir_final <- nir_aligned
    chm_final <- chm_aligned
    rgb_final <- rgb_projected
    
   }
  
  # export the rasters
  nir_out <- paste0(out_dir, site_name, "_nir_aligned.tif")
  chm_out <- paste0(out_dir, site_name, "_chm_aligned.tif")
  rgb_out <- paste0(out_dir, site_name, "_rgb_aligned.tif")
  
  writeRaster(nir_final, nir_out, overwrite = TRUE)
  writeRaster(chm_final, chm_out, overwrite = TRUE)
  writeRaster(rgb_final, rgb_out, overwrite = TRUE)
  
  # Create raster stack from listed rasters
  list_rasters <- c(rgb_out, nir_out, chm_out)
  r <- rast(list_rasters) # your raster stack
  
  # Mask out everything >= 2 meters 
  r <- terra::mask(r, chm_final < 2, maskvalues = FALSE)
  
  return(r)
}

############################################################
#SET-UP, SPLIT & TEST THE TRAINING DATA
############################################################

train_the_data <- function(aligned_rasters, training_polys){
  
  # Extract directly from the SpatRaster object
  extracted_data <- terra::extract(aligned_rasters, training_polys, xy = TRUE, df = TRUE)
  
  # Assign Raster Values to Training Data (Keeping ONLY Classname)
  poly_attributes <- training_polys %>%
    st_drop_geometry() %>%
    mutate(ID = row_number()) %>%
    dplyr::select(ID, Classname) # This line strips out Classcode, RED, etc.
  
  combined_data <- extracted_data %>%
    left_join(poly_attributes, by = "ID")
  
  # Split dataset into test and training data
  set.seed(123) 
  parts <- as.vector(createDataPartition(combined_data$Classname, p = 0.75, list = FALSE))
  train_data <- combined_data[parts, ]
  test_data <- combined_data[-parts, ]
  
  # Must return multiple objects as a list
  return(list(train = train_data, test = test_data))
}

###################################################################################################
# Random Forests Classification 
###################################################################################################

train_rf_model <- function(train_data, predictors = c("Red", "Green", "Blue", "RedEdge", "NIR", "Z")) {
  
  # Remove missing values to prevent randomForest from throwing an error
  train_data <- train_data %>%
    drop_na()
  
  # Find the number of pixels in the smallest training class for balanced sampling
  min_n <- min(table(train_data$Classname))
  
  # Run Random Forest Model
  rf_model <- randomForest(
    x = train_data[, predictors], 
    y = as.factor(train_data$Classname),
    mtry = 2, 
    ntree = 1000, 
    importance = TRUE,
    sampsize = rep(min_n, length(unique(train_data$Classname))) 
  )
  
  # Check error convergence
  plot(rf_model, main = "Random Forest Error Convergence")
  
  return(rf_model)
}


##################################################################################################
# map PFT classification
###################################################################################################

predict_rf_raster <- function(raster_stack, rf_model, output_file, enable_plotting = "yes", num_cores = 2) {
  
  if (enable_plotting == "yes") {
    # Added cpkgs = "randomForest" to load the package on worker nodes
    rf_classification <- terra::predict(raster_stack, rf_model, 
                                        na.rm = TRUE, 
                                        cores = num_cores, 
                                        cpkgs = "randomForest")
    
    # Mask out the shadow class by setting those pixels to NA
    rf_classification[rf_classification == "shadow"] <- NA
    
    plot(rf_classification, main = "Random Forest Classification")
    
    # Save the output to disk
    terra::writeRaster(rf_classification, filename = output_file, overwrite = TRUE)
    
    return(rf_classification)
    
  } else {
    message("Raster exporting currently disabled")
    return(NULL)
  }
}

