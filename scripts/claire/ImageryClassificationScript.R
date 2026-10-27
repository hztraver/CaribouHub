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

# Set directory for exported tables and figures
#table_output <- paste(sharepoint_root,"Cenovus - Documents/Wetland Research Program/Satellite Imagery Analysis/2023/ExportedTables", sep="")
#fig_output <- paste(sharepoint_root,"Cenovus - Documents/Wetland Research Program/Satellite Imagery Analysis/2023/ExportedFigures", sep="")

# Set directory for exported raster files and test data
#raster_output <- paste(sharepoint_root, "Cenovus - Documents/Wetland Research Program/Satellite Imagery Analysis/2023/Rasters", sep="")
#vector_output <- paste(sharepoint_root, "Cenovus - Documents/Wetland Research Program/Satellite Imagery Analysis/2023/Shapefiles", sep="")

# Turn figure and table exporting on or off - comment out "no" if you want plots, rasters, and tables exported
#enable_plotting <- "yes"
# enable_plotting <- "no"


###################################################################################################
# align and stack rasters
###################################################################################################


rgb_path <- "C:/Data/Drone/Ethier_orthos_2025/swede_rgb_ortho.tif"

rgb_rast <- rast(rgb_path)
nir_rast <- rast("C:/Data/Drone/Ethier_orthos_2025/swede_ms_ortho.tif")
chm_rast <- rast("C:/Data/Drone/CanopyHeightModels/output/RASTER/CHM_swede2025.tif")

crs(chm_rast)
crs(nir_rast)
crs(rgb_rast)

# clip and resample rasters to the same resolution before stacking
nir_aligned <- resample(nir_rast, rgb_rast, method="bilinear")
chm_aligned <- resample(chm_rast, rgb_rast, method="bilinear")

#export the rasters
nir_out <- "C:/Data/Drone/Processed_rasters/swede2025_ms_ortho_aligned.tif"
chm_out <- "C:/Data/Drone/Processed_rasters/swede2025_chm_aligned.tif"

writeRaster(nir_aligned, nir_out, overwrite = TRUE)
writeRaster(chm_aligned, chm_out, overwrite = TRUE)


# Create raster stack from listed rasters
raster_stack <- stack(c(rgb_path, nir_out, chm_out))


###################################################################################################
# Set up test and training data 
###################################################################################################

# Create list of rasters in the 'input_data' folder
raster_list <- list.files(input_data,  full.names = TRUE)


for (r_path in raster_list) {
  
  # Parse year and site name
  filename <- basename(r_path)
  year <- str_extract(filename, "\\d{4}")
  parts <- str_split(filename, "_")[[1]]
  site_name <- parts[2]
  
  # Construct the matching training path
  train_name <- paste0(site_name, year, "_trainingdata.shp")
  train_path <- file.path(training_dir, train_name)
  
  # Skip if the training file doesn't exist
  if (!file.exists(train_path)) {
    warning(paste("Missing training data for:", train_name))
    next 
  }
  
  # Load data
  r <- rast(r_path)
  training_polys <- read_sf(train_path)
  
  # Extract pixel values
  extracted_data <- terra::extract(r, training_polys, xy = TRUE, df = TRUE)
  
  print(paste0("Successfully processed: ", site_name, " (", year, ")"))
}

# Assign Raster Values to Training Data
poly_attributes <- training_polys %>%
  st_drop_geometry() %>%
  mutate(ID = row_number())

combined_data <- extracted_data %>%
  left_join(poly_attributes, by = "ID") %>%
  drop_na(Band_1, Band_2, Band_3, Band_4, Classname)

# Split dataset into test and training data
set.seed(123) 
parts <- as.vector(createDataPartition(combined_data$Classname, p = 0.75, list = FALSE))
train_data <- combined_data[parts, ]
test_data <- combined_data[-parts, ]

# Export test data shapefiles
#if (enable_plotting == "yes") {
#  TestData <- st_write(test, paste(vector_output, "/TestData_20231222.shp", sep = ""))
#} else {
#  print("Shapefile exporting currently disabled")
#}

# Export test and training data summary
#test_data_summary <- test %>% dplyr::count(id, Desc)
#training_data_summary <- train %>% dplyr::count(Desc)
#data_summary <- merge(test_data_summary, training_data_summary, by="Desc")
#names(data_summary) <- c("Class_desc", "Class_id", "test_count", "training_count")

# Export data summary table
#if (enable_plotting == "yes") {
#  write.csv(data_summary, paste(table_output, "/data_summary.csv", sep = ""), row.names = TRUE)
#} else {
#  print("Table exporting currently disabled")
#}

###################################################################################################
# Random Forests Classification 
###################################################################################################

# Run Random Forests Model using training data
predictors <- c("Band_1", "Band_2", "Band_3", "Band_4")

# Find the number of pixels in the smallest training class for balanced sampling
min_n <- min(table(train_data$Classname))

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

# Assign band names
band_labels <- c(
  "Band_1" = "Blue Band",
  "Band_2" = "Green Band",
  "Band_3" = "Red Band",
  "Band_4" = "NIR Band"
)

# Plot variable importance
rf_importance <- as.data.frame(rf_model$importance)
rf_importance$input_raster <- rownames(rf_importance)
rf_importance$sd <- as.data.frame(rf_model$importanceSD)$MeanDecreaseAccuracy

# Map labels securely by row name, then sort
rf_importance$label <- band_labels[rownames(rf_importance)]
rf_importance <- rf_importance[order(rf_importance$MeanDecreaseAccuracy), ]


rf_var_plot <- ggplot(rf_importance, aes(y = reorder(label, MeanDecreaseAccuracy), x = MeanDecreaseAccuracy)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = MeanDecreaseAccuracy - (sd * 2), 
                     xmax = MeanDecreaseAccuracy + (sd * 2)), 
                 linewidth = 0.4, 
                 height = 0.2) +
  labs(x = "Mean Decrease in Accuracy", y = "Input Data") +
  theme_bw() +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 11)
  )
plot(rf_var_plot)

ggsave("rf_var_plot.jpg",
       path = fig_output,
       dpi = 400,
       width = 400,
       height = 200,
       units = "mm")

# varImpPlot(rf.model, type=1)

# Drop Classcode column
test_data <- test_data %>% select(-Classcode)

# Assess the model using the test data. Generate a confusion matrix.
rf_pred <- predict(rf_model, test_data, type="response")
rf_pred <- as.data.frame(rf_pred)
rf_pred$test_data <- test_data$Classname
rf_confusion_matrix <- confusionMatrix(as.factor(rf_pred$test_data), as.factor(rf_pred$rf_pred))
rf_confusion_matrix

table_output <- "C:/Data"

# Export model test data summaries
if ("yes" == "yes") {
  write.csv(as.table(rf_confusion_matrix$table), paste(table_output, "/RF_confusion_matrix.csv", sep = ""), row.names = TRUE) # Export confusion matrix table
  write.csv(as.table(rf_confusion_matrix$byClass), paste(table_output, "/RF_class_measures.csv", sep = ""), row.names = TRUE) # Export confusion matrix table
  write.csv(as.table(rf_confusion_matrix$overall), paste(table_output, "/RF_overall_measures.csv", sep = ""), row.names = TRUE) # Export confusion matrix table
} else {
  print("Table exporting currently disabled")
}

# Predict Classes into Raster
if (enable_plotting == "yes") {
  rf_classification <- raster::predict(raster_stack, rf_model, type="response", index=1, na.rm=TRUE, progress="window")
  plot(rf_classification)
  writeRaster(rf_classification, filename=paste(raster_output, "/RandomForestPred_20240103.img", sep=""), format = "GTiff")
} else {
  print("Raster exporting currently disabled")
}



###################################################################################################
# Boosted Regression Trees Classification 
###################################################################################################
# Tidy the dataset
drop <- c("Desc", "geometry")
train_brt <- train[ , !(names(train) %in% drop)]
test_brt <- test[ , !(names(test) %in% drop)]

# Define predictor and response in test and training datasets
train_x <- data.matrix(train_brt[, 2:9])
train_y <- train_brt[, 1]
train_y <- train_y-1

test_x <- data.matrix(test_brt[, 2:9])
test_y <- test_brt[, 1]
test_y <- test_y-1

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

# Run BRT model
brt_model <- xgboost(data = xgb_train, nround = 3000, max_depth = 4, eta = 0.5, objective = "multi:softmax", num.class = 7)

# Assess model accuracy using test data
brt_pred <- predict(brt_model, xgb_test)
brt_confusion_matrix <- confusionMatrix(as.factor(brt_pred), as.factor(test_y))

if (enable_plotting == "yes") {
  write.csv(as.table(brt_confusion_matrix$table), paste(table_output, "/brt_confusion_matrix.csv", sep = ""), row.names = TRUE) # Export confusion matrix table
  write.csv(as.table(brt_confusion_matrix$byClass), paste(table_output, "/brt_class_measures.csv", sep = ""), row.names = TRUE) # Export confusion matrix table
  write.csv(as.table(brt_confusion_matrix$overall), paste(table_output, "/brt_overall_measures.csv", sep = ""), row.names = TRUE) # Export confusion matrix table
} else {
  print("Table exporting currently disabled")
}

# Set up function for prediction
xgbpred <- function(model, data, ...) {
  predict(model, newdata=as.matrix(data), ...)}

# Predict Classes into Raster
if (enable_plotting == "yes") {
  brt_classification <- predict(raster_stack, model=brt_model, fun=xgbpred, progress="window")
  plot(brt_classification)
  writeRaster(brt_classification, filename=paste(raster_output, "/BRTPred_20240103.img", sep=""), format = "GTiff")
} else {
  print("Raster exporting currently disabled")
}