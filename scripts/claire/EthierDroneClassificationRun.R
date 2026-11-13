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
library(profvis)

# Source original script containing the drone classificaton functions
source("C:/Users/caeth/Documents/CaribouHub/scripts/claire/DroneClassificationFunctions.R") 

# ===================================================================
# SET-UP SITE SPECIFIC FILES, INPUTS, & OUTPUTS
# ===================================================================
site_name <- "sixtymile"
year <- "2025"
enable_plots <- "yes"

# Input Paths
#search_pattern <- paste0("^", site_name, "_pointcloud_", year, ".*\\.laz$")
#las_files <- list.files("C:/Data/Drone/PointClouds", pattern = search_pattern, full.names = TRUE)
rgb_path    <- paste0("C:/Data/Drone/OrthosClean/", site_name, year, "_rgb_ortho.tif")
nir_path    <- paste0("C:/Data/Drone/OrthosClean/", site_name, year, "_ms_ortho.tif")
clip_extent <- paste0("C:/Data/Drone/TrainingData/", site_name, year, "_cutline.shp")
train_shp   <- paste0("C:/Data/Drone/TrainingData/", site_name, year, "_trainingdata.shp")

# Output & Intermediate Paths
chm_dir <- "C:/Data/Drone/CanopyHeightModels"
chm_path    <- paste0(chm_dir, "/output/RASTER/", site_name, year, "_CHM_TreeMasked.tif") #Change for Orndahl sites
aligned_dir <- "C:/Data/Drone/ProcessedRasters/" # Must end with '/'
output_path <- paste0("C:/Data/Drone/ClassifiedOrthos/", site_name, year, "_classified.tif")

# ===================================================================
# EXECUTE WORKFLOW
# ===================================================================

# Step 1A: Pre-process Ethier imagery
#ONLY IF DOING 2025 OR 2026
ethier_preprocess_RGB(site_name, year)
ethier_preprocess_MS(site_name, year)

# Step 1: Build Canopy Height Model (CHM) and mask trees
chm_function(site_name, year, dir = chm_dir)

# Step 2: Align, Clip & Stack Rasters
aligned_rasters <- align_rasters(nir_path = nir_path, 
                                 rgb_path = rgb_path, 
                                 chm_path = chm_path, 
                                 clip_extent = clip_extent, 
                                 out_dir = aligned_dir, 
                                 site_name = site_name)

aligned_rasters
names(aligned_rasters)[4:5] <- c("RedEdge", "NIR")

# Step 3: Set-Up, Split & Test Training Data
training_polys <- read_sf(train_shp)
trained_data <- train_the_data(aligned_rasters = aligned_rasters, 
                               training_polys = training_polys)

# Step 4: Train Random Forest Model
my_rf_model <- train_rf_model(train_data = trained_data$train, predictors = c("Red", "Green", "Blue", "RedEdge", "NIR", "Z"))

# View Confusion Matrix
print(my_rf_model$confusion)

# Step 5: Map Classification & Export
classified_raster <- predict_rf_raster(raster_stack = aligned_rasters, 
                                       rf_model = my_rf_model, 
                                       output_file = output_path, 
                                       enable_plotting = enable_plots)
