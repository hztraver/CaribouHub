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
    
    return c(rgb_out, nir_out, chm_out)
    
    }

# test function

rgb <- "C:/Data/Drone/Ethier_orthos_2025/swede_rgb_ortho.tif"
nir <- "C:/Data/Drone/Ethier_orthos_2025/swede_ms_ortho.tif"
chm <- "C:/Data/Drone/CanopyHeightModels/output/RASTER/CHM_max_swede2025.tif"
cutline <-"C:/Data/Drone/Ethier_orthos_2025/ClipExtent_swede2025.shp"






aligned_rasters <- align_rasters(nir, rgb, chm, cutline, "C:/Data/Drone/", "Swede_2025")

############################################################

train_the_data <- function(aligned_rasters, training_data){

    # Load data
    r <- rast(aligned_rasters) # your raster stack
    training_polys <- read_sf(training_data)
    
    # Extract pixel values
    extracted_data <- terra::extract(r, training_polys, xy = TRUE, df = TRUE)
    
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
    
    return train_data, test_data
}

trained_data <- train_the_data(aligned_rasters, ..., ...)
