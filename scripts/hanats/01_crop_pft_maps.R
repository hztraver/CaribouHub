library(terra)
library(dplyr)

lichen.dir = "E:/Caribou/Macander_Lichen_PFT/" # folder with PFT layers
lichen.layers = list.files(path = lichen.dir, pattern = "*.tif$") # List all the .tif files in the directory

# get the projection information for one of the layers
lichen.1985 = rast("E:/Caribou/Macander_Lichen_PFT/ABoVE_PFT_Top_Cover_tmLichenLight_1985.tif")
lichen.crs = crs(lichen.1985)

# shapefiles for Porcupine caribou total , summer and winter range
# project to match the PFT layer
total.range = vect("D:/Yukon/caribou_shapefiles/PorcupineCaribouHerdRange/NOS_TS_C_180701_Caribou_PorcupineTotalRange.shp") %>% project(., lichen.crs)
summer = vect("D:/Yukon/caribou_shapefiles/PorcupineCaribouHerdRange/NOS_TS_C_180701_Caribou_PorcupineSummerRange.shp") %>% project(., lichen.crs)
winter = vect("D:/Yukon/caribou_shapefiles/PorcupineCaribouHerdRange/NOS_TS_C_180701_Caribou_PorcupineWinterRange.shp") %>% project(., lichen.crs)

# Clip the lichen percent cover layers to the total ,summer and winter range
for (layer in lichen.layers){
  
  print(layer)
  
  r = rast(paste0(lichen.dir, layer))
  
  # extract the functional type and year from the file name as a string to use in the output filename 
  pft.year = gsub("ABoVE_PFT_Top_Cover_tm", "", layer) 
  
  # new folder for cropped tifs
  out.dir = paste0("E:/Caribou/lichen_clipped/")
  
  # crop each layer and write to new output folder
  crop(r, total.range, mask = T, filename = paste0(out.dir, "totalrange_", pft.year))
  crop(r, summer, mask = T, filename = paste0(out.dir, "summerrange_", pft.year))
  crop(r, winter, mask = T, filename = paste0(out.dir, "winterrange_", pft.year))
  
  # manually clear the memory after each loop 
  gc(verbose = F)
}
