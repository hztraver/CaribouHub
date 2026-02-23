library(terra)
library(data.table)
library(ggplot2)

# --- 1. SETTINGS & PATHS ---
root_path  <- "C:/Users/caeth/Documents/Data/ABOVE PFT 2005-2020/AK_Yukon_PFT_TopCover_2032_1.1-20260222_205533"
output_dir <- file.path(root_path, "Clipped_PFT_Data")
setwd(root_path)

# --- 2. LOAD AND AGGREGATE ---
# Loading the multi-layer files we created
r1985 <- rast(file.path(output_dir, "PFT_masked_1985.tif"))
r2020 <- rast(file.path(output_dir, "PFT_masked_2020.tif"))

# Ensure we are looking at the same layers
# Note: 2020 has many layers, 1985 only has DeciduousShrub and tmLichenLight
common_layers <- c("DeciduousShrub", "tmLichenLight")
s1985 <- r1985[[common_layers]]
s2020 <- r2020[[common_layers]]

# Aggregate to 900m (fact=30) as per your method
message("Aggregating to 900m...")
s1985_900m <- terra::aggregate(s1985, fact = 30, fun = "mean")
s2020_900m <- terra::aggregate(s2020, fact = 30, fun = "mean")

# --- 3. CALCULATE SPATIAL DIFFERENCE ---
# Change = 2020 - 1985
pft_diff <- s2020_900m - s1985_900m
names(pft_diff) <- paste0("Diff_", names(s1985_900m))

# --- 4. CALCULATE ZONAL STATISTICS (Single Range) ---

# Ensure your single range map is loaded and projected
# FMCH_vect should be the SpatVector you created in the previous step
FMCH_range_proj <- project(FMCH_vect, crs(pft_diff))

# Get mean change for the entire range
# zonal() returns a data.frame with the mean of each layer in pft_diff
diff_summary <- terra::zonal(pft_diff, FMCH_range_proj, fun = "mean", na.rm = TRUE)

# Add a column to identify the area
diff_summary$Range <- "Fortymile_Expanded_Range"

print("Mean Percent Cover Change (1985 to 2020):")
print(diff_summary)

# --- 5. PREPARE SHARED SCALE LIMITS ---

# Calculate the global maximum absolute value across both layers 
# This ensures 0 is always the "white" middle point
max_val <- max(abs(minmax(pft_diff)), na.rm = TRUE)
shared_min <- -max_val
shared_max <-  max_val

message("To scale these the same in ArcGIS, use these limits:")
message("Minimum: ", shared_min)
message("Maximum: ", shared_max)

# --- 6. SAVE RESULTS FOR ARCGIS ---

# 1. Save Shrub Change
# We use 'clamp' to ensure no values fall outside our shared limits
shrub_export <- clamp(pft_diff[["Diff_DeciduousShrub"]], shared_min, shared_max)
writeRaster(shrub_export, 
            filename = file.path(output_dir, "Shrub_Percent_Change_1985_2020.tif"), 
            overwrite = TRUE)

# 2. Save Lichen Change
lichen_export <- clamp(pft_diff[["Diff_tmLichenLight"]], shared_min, shared_max)
writeRaster(lichen_export, 
            filename = file.path(output_dir, "Lichen_Percent_Change_1985_2020.tif"), 
            overwrite = TRUE)

# 3. Save the summary statistics
write.csv(diff_summary, file.path(output_dir, "FMCH_PFT_Change_Summary_1985_2020.csv"), row.names = FALSE)