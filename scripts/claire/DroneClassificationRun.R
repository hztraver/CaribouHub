# ===================================================================
# 1. LOAD LIBRARIES & FUNCTIONS
# ===================================================================
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
library(future)

# Source original script containing the drone classificaton functions
source("C:/Users/caeth/Documents/CaribouHub/scripts/claire/DroneClassificationFunctions.R") 

# ===================================================================
# SET-UP SITE SPECIFIC FILES, INPUTS, & OUTPUTS
# ===================================================================
site_name <- "sixtymile"
year <- "2025"
enable_plots <- "yes"

# Global Directory (Required because your CHM_model function calls 'dir' internally)
dir <- "C:/Data/Drone/CanopyHeightModels"

# Input Paths
las_dir     <- paste0("C:/Data/Drone/PointClouds/", site_name, year)
rgb_path    <- paste0("C:/Data/Drone/EthierOrthos/", site_name, "_rgb_ortho.tif")
nir_path    <- paste0("C:/Data/Drone/EthierOrthos/", site_name, "_ms_ortho.tif")
clip_extent <- paste0("C:/Data/Drone/EthierOrthos/ClipExtent_", site_name, year, ".shp")
train_shp   <- paste0("C:/Data/Drone/TrainingData/", site_name, year, "_trainingdata.shp")

# Output & Intermediate Paths
chm_path    <- paste0(dir, "/output/RASTER/", site_name, "_CHM.tif")
aligned_dir <- "C:/Data/Drone/ProcessedRasters/" # Must end with '/'
output_path <- paste0("C:/Data/Drone/ClassifiedOrthos/", site_name, "_classified.tif")

# ===================================================================
# EXECUTE WORKFLOW
# ===================================================================

# Step 1: Build Canopy Height Model (CHM)
LCT <- readLAScatalog(las_dir)
CHM_model(LCT)

# Step 2: Align, Clip & Stack Rasters
aligned_rasters <- align_rasters(nir_path = nir_path, 
                                 rgb_path = rgb_path, 
                                 chm_path = chm_path, 
                                 clip_extent = clip_extent, 
                                 out_dir = aligned_dir, 
                                 site_name = site_name)

# Step 3: Set-Up, Split & Test Training Data
training_polys <- read_sf(train_shp)
trained_data <- train_the_data(aligned_rasters = aligned_rasters, 
                               training_polys = training_polys)

# Step 4: Train Random Forest Model
my_rf_model <- train_rf_model(train_data = trained_data$train)

# View Confusion Matrix
print(my_rf_model$confusion)

# Step 5: Map Classification & Export
classified_raster <- predict_rf_raster(raster_stack = aligned_rasters, 
                                       rf_model = my_rf_model, 
                                       output_file = output_path, 
                                       enable_plotting = enable_plots)