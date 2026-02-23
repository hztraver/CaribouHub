library(sf)
library(terra)

# --- 1. SETUP ---
# Update this path if your working directory has changed
output_dir <- "Clipped_PFT_Data"
pft_2020_path <- file.path(output_dir, "PFT_masked_2020.tif")

# Load the 2020 raster
pft_2020 <- rast(pft_2020_path)

# Load the shapefile to get the target CRS (Alaska Albers)
FMCH_range <- st_read("Fortymile_Caribou_Expanded_Range_2013-2014_Final.shp")
target_crs <- st_crs(FMCH_range)$wkt

# --- 2. SUBSET & DOWNSAMPLE ---
# We subset first to save memory
selected_layers <- pft_2020[[c("DeciduousShrub", "tmLichenLight")]]

# Downsample by a factor of 10 to prevent "Fatal Error" during projection/rotation
# This reduces ~219 million pixels to ~2.2 million for plotting
pft_small <- aggregate(selected_layers, fact = 10, fun = "mean")

# --- 3. PROJECT & ROTATE ---
# Project to Alaska Albers (EPSG:3338)
pft_projected <- project(pft_small, target_crs)

# TRUE 90° CCW ROTATION: Transpose then Flip Horizontal
pft_rotated <- flip(t(pft_projected), direction = "vertical")

# --- 4. VISUALIZATION ---
# Set up a 1x2 plotting area
par(mfrow = c(1, 2))

# Plot Deciduous Shrub
plot(pft_rotated[[1]], 
     main = "Deciduous Shrub\n(%Top Cover)",
     axes = FALSE, 
     mar = c(2, 2, 4, 4))

# Plot tmLichen Light
plot(pft_rotated[[2]], 
     main = "Lichen\n(%Top Cover)",
     axes = FALSE, 
     mar = c(2, 2, 4, 4))

# Reset plotting parameters
par(mfrow = c(1, 1))

message("Plotting complete. Data was downsampled by factor 10 for stability.")


# --- 1. SETUP ---
# Update this path if your working directory has changed
output_dir <- "Clipped_PFT_Data"
pft_2005_path <- file.path(output_dir, "PFT_masked_2005.tif")

# Load the 2005 raster
pft_2005 <- rast(pft_2005_path)

# Load the shapefile to get the target CRS (Alaska Albers)
FMCH_range <- st_read("Fortymile_Caribou_Expanded_Range_2013-2014_Final.shp")
target_crs <- st_crs(FMCH_range)$wkt

# --- 2. SUBSET & DOWNSAMPLE ---
# We subset first to save memory
selected_layers_2005 <- pft_2005[[c("DeciduousShrub", "tmLichenLight")]]

# Downsample by a factor of 10 to prevent "Fatal Error" during projection/rotation
# This reduces ~219 million pixels to ~2.2 million for plotting
pft_small_2005 <- aggregate(selected_layers_2005, fact = 10, fun = "mean")

# --- 3. PROJECT & ROTATE ---
# Project to Alaska Albers (EPSG:3338)
pft_projected_2005 <- project(pft_small_2005, target_crs)

# TRUE 90° CCW ROTATION: Transpose then Flip Horizontal
pft_rotated_2005 <- flip(t(pft_projected_2005), direction = "vertical")

# --- 4. VISUALIZATION ---
# Set up a 1x2 plotting area
par(mfrow = c(1, 2))

# Plot Deciduous Shrub
plot(pft_rotated_2005[[1]], 
     main = "Deciduous Shrub\n(%Top Cover)",
     axes = FALSE, 
     mar = c(2, 2, 4, 4))

# Plot tmLichen Light
plot(pft_rotated_2005[[2]], 
     main = "Lichen\n(%Top Cover)",
     axes = FALSE, 
     mar = c(2, 2, 4, 4))

# Reset plotting parameters
par(mfrow = c(1, 1))

message("Plotting complete. Data was downsampled by factor 10 for stability.")
