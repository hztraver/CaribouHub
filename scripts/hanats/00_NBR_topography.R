library(terra)
library(dplyr)
library(data.table)

#### dNBR ####
#### Project dNBR and resample to consistent grid 
paths = list.files(path = "D:/Caribou/NBR", pattern = "*.tif$")
dir = "D:/Caribou/NBR/"

## template raster 
## Template raster using porcupine and fortymile extent
r = rast("D:/Caribou/pft_clipped/Porcupine/aggregate_1km/winterrange_2020_900m.tif")
rf = rast("D:/Caribou/pft_clipped/Fourtymile/aggregate_1km/Fourtymile_totalrange_2020_900m.tif")
temp = merge(r, rf)

for(path in paths) {
  
  print(path)
  
  r = rast(paste0(dir, path)) 
  r = r %>% project(., temp) %>% resample(., temp)
  r = r / 100000
  
  out.name = gsub(".tif", "_900m.tif", path)
  writeRaster(r, filename = paste0(dir, out.name), overwrite = T)
  
  gc(verbose = F)
  
}

#### Merge dNBR
paths = paste0(dir, list.files(path = "D:/Caribou/NBR", pattern = "900m.*\\.tif$"))
raster_collection = sprc(lapply(paths, rast))

rc = mosaic(raster_collection, fun = "max")
NAflag(rc) <- 0

writeRaster(rc, filename = paste0(dir, "dNBR_2000_2005.tif"), overwrite = T)

#### TOPOGRAPHY ####
r = rast("C:/Users/hanats/Downloads/Porcupine_DEM.tif") %>% project(., temp) %>% resample(., temp)
r2 = rast("C:/Users/hanats/Downloads/Fortymile_DEM.tif") %>% project(., temp) %>% resample(., temp)

raster_collection = sprc(r, r2)
rc = mosaic(raster_collection, fun = "max")
NAflag(rc) <- 0

writeRaster(rc, filename = "D:/Caribou/DEM/combined_DEM.tif", overwrite = T)