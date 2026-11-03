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
#SET-UP SITE SPECIFIC FILES, INPUTS, OUTPUTS, ETC.
###################################################################

# Set input rasters
input_data <- "C:/Data/Drone/EthierOrthos" #for ethier orthos
#input_data <- "C:/Data/Drone/OrndahlOrthos" #for orndahl orthos

# Set training data shapefiles
training_dir <- "C:/Data/Drone/TrainingData/swede2025_trainingdata.shp"

enable_plotting = "yes"

# Set location of swede2025_classified output raster
output_data <- "C:/Data/Drone/ClassifiedOrthos/swede2025_classified.tif"

###################################################################
#BUILD CHM FROM POINT CLOUD
###################################################################
# Add 'dir' and 'site_name' as arguments
CHM_model <- function(LCT, dir, site_name) {
 
   #### GROUND CLASSIFICATION ####
  # classify ground/canopy from the filtered point cloud
  # using cloth simulation function
  
  ## options for reading in the LAS Catalog
  opt_chunk_buffer(LCT) = 10 
  opt_laz_compression(LCT) = TRUE 
  
  opt_filter(LCT) = "-thin_with_voxel 0.1" 
  
  # Now 'dir' is recognized as the text path you pass in
  opt_output_files(LCT) = paste0(dir, "/output/GROUND/{*}_GROUND") 
  opt_progress(LCT) = TRUE
  
  plan(multisession, workers = 2L)
  
  GROUND = classify_ground(LCT, algorithm = csf(
    sloop_smooth = TRUE,
    class_threshold = 0.5,
    cloth_resolution = 1,
    rigidness = 3L,
    iterations = 500L,
    time_step = 1)) 
  
  plan(sequential)
  
  #### CREATE A DEM FROM THE GROUND CLASSIFIED POINTS ####
  GROUND = readLAScatalog(folder = paste0(dir, "/output/GROUND/"))
  opt_chunk_buffer(GROUND) = 15
  
  opt_filter(GROUND) = "-drop_z_below -0.25"
  opt_output_files(GROUND) = "" 
  opt_progress(GROUND) = TRUE
  
  DEM = grid_terrain(GROUND, res = 1, knnidw(), full_raster = TRUE)
  
  #### NORMALIZATION ####
  GROUND = readLAScatalog(folder = paste0(dir, "/output/GROUND/"))
  
  opt_chunk_buffer(GROUND) = 1
  opt_laz_compression(LCT) = TRUE
  
  opt_output_files(GROUND) = paste0(dir, "/output/NORM/{*}_NORM")
  opt_progress(GROUND) = TRUE
  
  plan(multisession, workers = 2L)
  NORM = normalize_height(GROUND, knnidw(), na.rm = TRUE)
  plan(sequential)
  
  ## Create rasterized CHM from normalized point cloud
  NORM = readLAScatalog(paste0(dir, "/output/NORM/"))
  
  opt_chunk_buffer(NORM) = 0
  opt_laz_compression(NORM) = TRUE
  
  opt_filter(NORM) = "-drop_z_below -.25 -drop_z_above 30"
  opt_output_files(NORM) = ""
  opt_progress(NORM) = TRUE
  
  plan(multisession, workers = 2L)
  CHM_max = rasterize_canopy(NORM, res = 0.10, algorithm = p2r(subcircle = 0.5))
  plan(sequential)
  
  # Set any pixel with a height greater than 2.5 meters to NA
  CHM_max[CHM_max > 2.5] <- NA
  
  plot(CHM_max, col = height.colors(50), main = "Canopy Height Model")
  
  # Use the site_name variable here instead of hardcoding "swede2025"
  out_file <- paste0(dir, "/output/RASTER/", site_name, "_CHM.tif")
  writeRaster(CHM_max, filename = out_file, overwrite = TRUE)
}


###################################################################
#ALIGN, CLIP & STACK RASTERS
###################################################################

align_rasters <- function(nir_path, rgb_path, chm_path, clip_extent, out_dir, site_name) {
  # This function aligns three rasters by clipping, cropping and reprojecting
    
    rgb_rast <- rast(rgb_path)[[1:3]]
    nir_rast <- rast(nir_path)[[3:4]]
    chm_rast <- rast(chm_path)
    
    nir_projected <- project(nir_rast, chm_rast, method = "bilinear")
    rgb_projected <- project(rgb_rast, chm_rast, method = "bilinear")
    
    # clip and resample rasters to the same resolution before stacking
    nir_aligned <- resample(nir_projected, rgb_projected, method="bilinear")
    chm_aligned <- resample(chm_rast, rgb_projected, method="bilinear")
    
    #clip rasters to same extent
    cutline <- vect(clip_extent)
    
    nir_crop <- crop(nir_aligned, cutline, mask = TRUE)
    chm_crop <- crop(chm_aligned, cutline, mask = TRUE)
    rgb_crop <- crop(rgb_projected, cutline, mask = TRUE)
    
    ext(nir_crop)
    ext(chm_crop)
    ext(rgb_crop)
    
    #export the rasters
    nir_out <- paste(out_dir, site_name, "nir_aligned.tif")
    chm_out <- paste(out_dir, site_name, "chm_aligned.tif")
    rgb_out <- paste(out_dir, site_name, "rgb_aligned.tif")
    
    writeRaster(nir_crop, nir_out, overwrite = TRUE)
    writeRaster(chm_crop, chm_out, overwrite = TRUE)
    writeRaster(rgb_crop, rgb_out, overwrite = TRUE)
    
    # Create raster stack from listed rasters
    list_rasters <- c(rgb_out, nir_out, chm_out)
    r <- rast(list_rasters) # your raster stack
    # Mask out everything >= 2 meters 
    r <- terra::mask(r, chm_crop < 2, maskvalues = FALSE)
    return (r)
    
    }

############################################################
#SET-UP, SPLIT & TEST THE TRAINING DATA
############################################################

train_the_data <- function(aligned_rasters, training_polys){
  
  # Extract directly from the SpatRaster object
  extracted_data <- terra::extract(aligned_rasters, training_polys, xy = TRUE, df = TRUE)
  
  # Assign Raster Values to Training Data
  poly_attributes <- training_polys %>%
    st_drop_geometry() %>%
    mutate(ID = row_number())
  
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

###################################################################################################
# map PFT classification
###################################################################################################

predict_rf_raster <- function(raster_stack, rf_model, output_file, enable_plotting = "yes") {
  
  if (enable_plotting == "yes") {
    # Predict using terra instead of the older raster package
    rf_classification <- terra::predict(raster_stack, rf_model, na.rm = TRUE)
    
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