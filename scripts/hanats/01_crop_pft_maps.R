library(terra)
library(dplyr)

pft.dir = "E:/Caribou/Macander_TopCover_PFT/" # folder with PFT layers
pft.layers = list.files(path = pft.dir, pattern = "*.tif$") # List all the .tif files in the directory

# get the projection information for one of the layers
lichen.1985 = rast("E:/Caribou/Macander_TopCover_PFT/ABoVE_PFT_Top_Cover_tmLichenLight_2020.tif")
lichen.crs = crs(lichen.1985)

# shapefiles for Porcupine caribou total , summer and winter range
# project to match the PFT layer
total.range = vect("D:/Yukon/caribou_shapefiles/PorcupineCaribouHerdRange/NOS_TS_C_180701_Caribou_PorcupineTotalRange.shp") %>% project(., lichen.crs)
summer = vect("D:/Yukon/caribou_shapefiles/PorcupineCaribouHerdRange/NOS_TS_C_180701_Caribou_PorcupineSummerRange.shp") %>% project(., lichen.crs)
winter = vect("D:/Yukon/caribou_shapefiles/PorcupineCaribouHerdRange/NOS_TS_C_180701_Caribou_PorcupineWinterRange.shp") %>% project(., lichen.crs)

# Clip the percent cover layers to the total ,summer and winter range
years = c("1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")
pft.names = c("BroadleafTree", "ConiferTree", "DeciduousShrub", "EvergreenShrub", "Forb", "Graminoid", "LichenLight")
  
for (year in years){
  
  print(year)
  
  # subset to all pft maps for a given time period
  layers = grep(year, pft.layers, value = T)
  
  # stack the rasters
  rstack = rast(paste0(pft.dir, layers))
  names(rstack) = pft.names
  
  # new folder for cropped tifs
  out.dir = paste0("E:/Caribou/pft_clipped/")
  
  # crop each layer and write to new output folder
  print("cropping total range")
  crop(rstack, total.range, mask = T, filename = paste0(out.dir, "totalrange_", year, ".tif"))
  
  print("cropping summer range")
  crop(rstack, summer, mask = T, filename = paste0(out.dir, "summerrange_", year, ".tif"))
  
  print("cropping winter range")
  crop(rstack, winter, mask = T, filename = paste0(out.dir, "winterrange_", year, ".tif"))
  
  # manually clear the memory after each loop 
  gc(verbose = F)
}
