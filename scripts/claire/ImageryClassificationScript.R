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
# Set up test and training data 
###################################################################################################

# Create list of rasters in the 'input_data' folder
raster_list <- list.files(input_data, pattern = "_arcprocessed\\.tif$", full.names = TRUE)

# Remove unwanted rasters from the lsit 
# raster.list <- raster.list[-1]

# Create raster stack from listed rasters
# Revisit once we have the NIR and RE orthos prepped
#raster_stack <- stack(raster_list)


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

# Plot variable importance
rf_importance <- as.data.frame(rf_model$importance)
rf_importance$input_raster <- rownames(rf_importance)
rf_importance$sd <- as.data.frame(rf_model$importanceSD)$MeanDecreaseAccuracy
rf_importance <- rf_importance[order(rf_importance$MeanDecreaseAccuracy),]
rf_importance$label <- c("AWI Wetland Type", "AWI Tree Cover", "50cm Panchromatic", "Blue Band", "NIR Band", "Green Band", "Red Band",  "NDVI")

rf_var_plot <- ggplot(rf_importance, aes(y= reorder(label, -MeanDecreaseAccuracy), x= MeanDecreaseAccuracy)) +
  geom_point() +
  geom_errorbarh(aes(xmin = MeanDecreaseAccuracy-(sd*2), xmax = MeanDecreaseAccuracy+(sd*2)), linewidth=0.2, height=0.2) +
  labs(x="Mean Decrease in Accuracy", y="Input Data") +
  # scale_y_discrete(labels = c("AWI Wetland Type",
  #                               "50cm Panchromatic",
  #                               "AWI Tree Cover",
  #                               "Blue Band",
  #                               "NIR Band",
  #                               "Green Band",
  #                               "NDVI",
  #                               "Red Band")) +
  scale_x_continuous(breaks = seq(0, 0.2, by=0.05), limits=c(0, 0.2), expand=c(0,0)) +
  theme_bw() +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5,"cm"))
plot(rf_var_plot)

ggsave("rf_var_plot.jpg",
       path = fig_output,
       dpi = 400,
       width = 400,
       height = 200,
       units = "mm")

# varImpPlot(rf.model, type=1)

# Assess the model using the test data. Generate a confusion matrix.
rf_pred <- predict(rf_model, test, type="response")
rf_pred <- as.data.frame(rf_pred)
rf_pred$test <- test$Desc
rf_confusion_matrix <- confusionMatrix(as.factor(rf_pred$test), as.factor(rf_pred$rf_pred))

# Export model test data summaries
if (enable_plotting == "yes") {
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