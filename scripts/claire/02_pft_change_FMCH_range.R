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

# --- 5. SIMPLE PLOT (Balanced Shared Scale) ---

# 1. Determine the global range of values to ensure identical scales
# We find the max absolute value to keep zero in the center
max_val <- max(abs(minmax(pft_diff)), na.rm = TRUE)
shared_range <- c(-max_val, max_val)

# 2. Define a diverging color palette (Red = Decrease, White = No Change, Blue = Increase)
# You can swap "blue" and "red" depending on which direction you want to emphasize
diff_cols <- colorRampPalette(c("red", "white", "blue"))(100)

# 3. Plot with the shared scale
plot(pft_diff, 
     main = c("Deciduous Shrub Change (1985-2020)", 
              "Lichen Light Change (1985-2020)"),
     range = shared_range,  # This forces the same scale limits
     col = diff_cols,       # This applies the same colors
     nc = 2)                # Forces plots to be side-by-side (2 columns)

# --- 6. SAVE RESULTS ---
writeRaster(pft_diff, file.path(output_dir, "PFT_Difference_1985_2020_900m.tif"), overwrite = TRUE)
write.csv(diff_summary, file.path(output_dir, "FMCH_PFT_Change_Summary_1985_2020.csv"), row.names = FALSE)