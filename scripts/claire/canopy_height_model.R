#### EXAMPLE LIDR PROCESSING WORKFLOW ####
library(RCSF)
library(sp)
library(sf)
library(terra)
library(lidR)
library(raster)
library(future) # for parallel processing

## create a folder to save outputs 
#dir = paste0("C:/Data/Drone/CanopyHeightModels")
#dir.create(paste0(dir, "/output"), recursive = TRUE)
##dir.create(paste0(dir,"/output/GROUND")) # ground classification
##dir.create(paste0(dir,"/output/NORM")) # normalized point cloud
##dir.create(paste0(dir,"/output/RASTER")) # rasterized DEM & CHM

dir <- "C:/Data/Drone/CanopyHeightModels"

#Set up site name & year 
#STUFF TO CHANGE
site_name <- "pinnell"
year <- "2026"

## read in the LAS Catalog 

chm_function <- function(site_name, year, dir) {
      # Setup paths
      raw_dir <- "C:/Data/Drone/PointClouds"
      ground_dir <- file.path(dir, "output", "GROUND")
      norm_dir <- file.path(dir, "output", "NORM")
      raster_dir <- file.path(dir, "output", "RASTER")
      
      # Read LAS files
      search_pattern <- paste0("^", site_name, "_pointcloud_", year, ".*")
      las_files <- list.files("C:/Data/Drone/PointClouds", pattern = search_pattern, full.names = TRUE)
      LCT <- readLAScatalog(las_files)

      #### GROUND CLASSIFICATION ####
      # classify ground/canopy from the filtered point cloud
      # using cloth simulation function
      
      ## options for reading in the LAS Catalog
      opt_chunk_buffer(LCT) = 10 # buffer each tile 10m
      opt_laz_compression(LCT) = TRUE # write output as .laz 
      
      # Point cloud thinning to speed things up by taking a random point within each 0.1m voxel
      # consider using higher/lower resolution voxel depending on point cloud density and computing power
      opt_filter(LCT) = "-thin_with_voxel 0.1" 
      
      # output naming convention will keep the tiling system
      opt_output_files(LCT) = paste0(dir, "\\output\\GROUND\\{*}_GROUND") 
      opt_progress(LCT) = TRUE
      opt_filter(LCT) = "-thin_with_voxel 0.1"
      print(LCT)
      # set up and execute task in parallel (2 cores) 
      plan(multisession, workers = 6L)
      
      # read in the LAS Catalog -> create new LAS Catalog of classified points called GROUND 
      GROUND = classify_ground(LCT, algorithm = csf(
        # in case there are steep slopes 
        sloop_smooth = TRUE,
        # minimum height threshold for canopy points
        class_threshold = 0.5,
        # larger resolution = coarser DTM
        cloth_resolution = 1,
        # rigidness of cloth - higher = more rigid
        rigidness = 3L,
        iterations = 500L,
        time_step = 1)) 
      
      # always stop the parallel processing!
      plan(sequential)
      
      #### CREATE A DEM FROM THE GROUND CLASSIFIED POINTS ####
      
      # read in the LAS Catalog of GROUND classified points
      #GROUND = readLAScatalog(folder = paste0(dir, "\\output\\GROUND\\"))
      opt_chunk_buffer(GROUND) = 15
      
      # drop noisy points below the ground
      opt_filter(GROUND) = "-drop_z_below -0.25"
      opt_output_files(GROUND) = "" # no LAS output
      opt_progress(GROUND) = TRUE
      
      # create a 1m DEM from the GROUND points 
      # using k-nn inverse distance weighting interpolation
      #DEM = grid_terrain(GROUND, res = 1, knnidw(), full_raster = TRUE)
      
      #### NORMALIZATION ####
      # normalize the point cloud to the ground classified points
      
      # read the classified point cloud in with high-res parameters
      ground_files <- list.files("C:/Data/Drone/CanopyHeightModels/output/GROUND", pattern = search_pattern, full.names = TRUE)
      GROUND = readLAScatalog(ground_files)
      plot(GROUND)
      # small buffer; we're just normalizing
      opt_chunk_buffer(GROUND) = 1
      opt_laz_compression(LCT) = TRUE
      
      # output normalized point cloud to NORM folder
      opt_output_files(GROUND) = paste0(dir, "\\output\\NORM\\{*}_NORM")
      opt_progress(GROUND) = TRUE
      
      # normalize the point cloud to the GROUND points
      # or alternatively to the rasterized DEM - a bit faster but less "exact"
      # NORM = normalize_height(LCT, DEM, knnidw(), na.rm = TRUE)
      plan(multisession, workers = 6L)
      NORM = normalize_height(GROUND, knnidw(), na.rm = TRUE)
      plan(sequential)
      
      ## Create rasterized CHM from normalized point cloud
      norm_files <- list.files("C:/Data/Drone/CanopyHeightModels/output/NORM", pattern = search_pattern, full.names = TRUE)
      NORM = readLAScatalog(norm_files)
      # no buffer
      opt_chunk_buffer(NORM) = 0
      opt_laz_compression(NORM) = TRUE
      
      # drop noisy points below the ground or floating in outer space
      opt_filter(NORM) = "-drop_z_below -.25 -drop_z_above 30"
      opt_output_files(NORM) = ""
      opt_progress(NORM) = TRUE
      
      # Create a 10cm CHM, expanding each point with a 50cm radius disc to fill holes
      plan(multisession, workers = 6L)
      CHM_max = rasterize_canopy(NORM, res = 0.10, algorithm = p2r(subcircle = 0.5))
      plan(sequential)
      
      ttops_chm <- locate_trees(CHM_max, lmf(ws = 5, hmin = 2))
      ttops_buffered <- st_buffer(ttops_chm, dist = 1)
      ttops_vect <- vect(ttops_buffered)
      
      chm_mask <- mask(CHM_max, ttops_vect, inverse = TRUE)
      
      # Export the CHM raster to a GeoTIFF file
      writeRaster(chm_mask, 
                  filename = paste0(dir, "/output/RASTER/", site_name, year, "_CHM_TreeMasked", ".tif"), 
                  overwrite = TRUE)
      return(chm_mask)

}

chm_function(site_name = site_name, year = year, dir = dir)
