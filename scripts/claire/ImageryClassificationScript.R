library(terra)
library(sf)
library(raster)
library(randomForest)
library(ggplot2)
library(caret)
library(xgboost)
library(dplyr)
library(tidyverse)

# Set location of input rasters
input_data <- "C:/Data/Drone/2018-2026 Orthos"

# Set location of training data shapefiles
training_dir <- "C:/Data/Drone/Training Data"

enable_plotting = "yes"

# Set location of swede2025_classified output raster
output_data <- "C:/Data/Drone/ClassifiedOrthos/swede2025_classified.tif"

###################################################################################################
# align and stack rasters
###################################################################################################

rgb_path <- "C:/Data/Drone/Ethier_orthos_2025/swede_rgb_ortho.tif"

rgb_rast <- rast(rgb_path)[[1:3]]
nir_rast <- rast("C:/Data/Drone/Ethier_orthos_2025/swede_ms_ortho.tif")[[3:4]]
chm_rast <- rast("C:/Data/Drone/CanopyHeightModels/output/RASTER/CHM_max_swede2025.tif")

nir_projected <- project(nir_rast, chm_rast, method = "bilinear")
rgb_projected <- project(rgb_rast, chm_rast, method = "bilinear")


# clip and resample rasters to the same resolution before stacking
nir_aligned <- resample(nir_projected, rgb_projected, method="bilinear")
chm_aligned <- resample(chm_rast, rgb_projected, method="bilinear")

#clip rasters to same extent
cutline <- vect("C:/Data/Drone/Ethier_orthos_2025/ClipExtent_swede2025.shp")

nir_crop <- crop(nir_aligned, cutline, mask = TRUE)
chm_crop <- crop(chm_aligned, cutline, mask = TRUE)
rgb_crop <- crop(rgb_projected, cutline, mask = TRUE)

ext(nir_crop)
ext(chm_crop)
ext(rgb_crop)

#export the rasters
nir_out <- "C:/Data/Drone/Processed_rasters/swede2025_ms_aligned4.tif"
chm_out <- "C:/Data/Drone/Processed_rasters/swede2025_chm_aligned4.tif"
rgb_out <- "C:/Data/Drone/Processed_rasters/swede2025_rgb_aligned5.tif"

writeRaster(nir_crop, nir_out, overwrite = TRUE)
writeRaster(chm_crop, chm_out, overwrite = TRUE)
writeRaster(rgb_crop, rgb_out, overwrite = TRUE)

# Create raster stack from listed rasters

###################################################################################################
# Set up test and training data 
###################################################################################################

# Create list of rasters in the 'input_data' folder
#raster_list <- list.files(input_data,  full.names = TRUE)

list_rasters <- c(rgb_out, nir_out, chm_out)

# Load data
r <- rast(list_rasters) # your raster stack
training_polys <- read_sf("C:/Data/Drone/Training Data/swede2025_trainingdata2.shp")
head(training_polys)
head(r)

# Extract pixel values
extracted_data <- terra::extract(r, training_polys, xy = TRUE, df = TRUE)
head(extracted_data)

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
head(train_data)
test_data <- combined_data[-parts, ]

###################################################################################################
# Random Forests Classification 
###################################################################################################

# Run Random Forests Model using training data
predictors <- c("Red", "Green", "Blue", "RedEdge", "NIR", "Z")

train_data <- train_data %>%
  drop_na()

# Find the number of pixels in the smallest training class for balanced sampling
min_n <- min(table(train_data$Classname))
min_n

rf_model <- randomForest(
  x = train_data[, predictors], 
  y = as.factor(train_data$Classname),
  mtry = 2, 
  ntree = 1000, 
  importance = TRUE,
  sampsize = rep(min_n, length(unique(train_data$Classname))) 
)

# Check error convergence
plot(rf_model)
rf_model

###################################################################################################
# write our badass model to a freaking map
###################################################################################################

# Predict Classes into Raster
if (enable_plotting == "yes") {
  rf_classification <- raster::predict(r, rf_model, type="response", index=1, na.rm=TRUE, progress="window")
  plot(rf_classification)
  writeRaster(rf_classification, filename= output_data)
} else {
  print("Raster exporting currently disabled")
}
 
