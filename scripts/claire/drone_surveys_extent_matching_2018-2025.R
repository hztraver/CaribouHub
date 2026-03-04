library(terra)
library(sf)

# 1. Load the two TIFF files
r2025 <- rast("C:/Users/caeth/Documents/Data/sixtymile_rgb_ortho_2025.tif")
r2018 <- rast("C:/Users/caeth/Documents/Data/sixtymile_rgb_ortho_2018.tif")

# 2. Project 2018 to match 2025's grid exactly
# This aligns the pixel 'cells'
r2018_proj <- project(r2018, r2025, method = "bilinear")

# 3. Calculate the strict intersection of the two bounding boxes
# This finds the 'inner' shared rectangle
common_ext <- intersect(ext(r2025), ext(r2018_proj))

# 4. Crop both to that EXACT shared rectangle
# We use 'snap = "near"' to ensure we don't accidentally add a partial pixel row
r2025_final <- crop(r2025, common_ext, snap = "near")
r2018_final <- crop(r2018_proj, common_ext, snap = "near")

# 5. Mask out the jagged edges (Optional but recommended)
# This ensures that if a pixel is NA in one year, it's also NA in the other
r2025_final <- mask(r2025_final, r2018_final)
r2018_final <- mask(r2018_final, r2025_final)

# 6. Final verification
if (compareGeom(r2025_final, r2018_final)) {
  message("Success! Dimensions, CRS, and Extent match perfectly.")
  
  # Save to your current directory (check with getwd())
  writeRaster(r2025_final, "sixtymile_2025_clean.tif", overwrite=TRUE)
  writeRaster(r2018_final, "sixtymile_2018_clean.tif", overwrite=TRUE)
}
