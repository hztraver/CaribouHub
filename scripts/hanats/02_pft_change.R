library(terra)
library(dplyr)
library(data.table)
library(magrittr)

#### SUMMARY STATISTICS FOR TOTAL PFT COVER ####
#### Calculate overall percent cover in total,  summer and winter range 

# Folder of lichen percent cover 30 m layers
dir = "E:/Caribou/pft_clipped/"
pft.layers = list.files(path = "E:/Caribou/pft_clipped", pattern = "*.tif$")

## Aggregate 30 m percent cover to 900 m spatial resolution to reduce file size
## since we can just sum percent cover in larger areas without losing information 
# it will be faster to plot and summarize the data at a coarser resolution 
for (layer in pft.layers){
  
  print(layer)
  
  # folder for 900 m layers
  layer.out = gsub(".tif", "_900m.tif", layer)
  path.out = paste0("E:/Caribou/pft_clipped/aggregate_1km/", layer.out)
  
  # aggregate pixels by a factor of 30 and sum percent cover values 
  r = rast(paste0(dir, layer)) %>% terra::aggregate(., fact = 30, fun = "mean", cores = 4)
  
  writeRaster(r, filename = path.out, overwrite = T)
  gc(verbose = F)
}

## Sum the 900 m layers to get overall percent cover in total, summer and winter range
## for each time period
dir.900m = "E:/Caribou/pft_clipped/aggregate_1km/"
list.900m = list.files(path = "E:/Caribou/pft_clipped/aggregate_1km/", pattern = "*.tif$")
time.periods = c("1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")
pft.names = c("BroadleafTree", "ConiferTree", "DeciduousShrub", "EvergreenShrub", "Forb", "Graminoid", "LichenLight")

## Combine range shapefiles into one 
summer = vect("E:/Caribou/pft_clipped/projected_range/summer_range_macander.shp")
winter = vect("E:/Caribou/pft_clipped/projected_range/winter_range_macander.shp")
total = vect("E:/Caribou/pft_clipped/projected_range/total_range_macander.shp")

range = rbind(total, summer, winter)
range$RANGE = c("Total", "Summer", "Winter")

# Calculate total area of ranges (tota, summer, winter) in km2
range.area = data.frame(Range = range$RANGE, Area_km2 = expanse(range, unit = "km"))

# Write the total percent cover to a data frame
# create first column of data frame with PFT names and the three range areas
dt = data.table(PFT = rep(pft.names, 3), range = rep(c("total", "summer", "winter"), each = 7))

# Note that an alternative method to get means across different ranges is use terra:zonal(fun = "mean") 
# with the raster & range boundary polygon, instead of writing the raster values to a data.table.
# but the zonal method is slower because calculating a mean on a raster is much slower than operations on a data.table

for (time in time.periods) {
  
  print(time)
  
  # Percent cover layers for summer range, total range, winter range (in that order)
  layers = grep(time, list.900m, value = T)

  # get the raster of percent cover for total range
  # extract values as a data.table and calculate a mean
  total = rast(paste0(dir.900m, layers[2])) %>% values() %>% na.omit() %>% as.data.table()
  percent.total = colMeans(total, na.rm = T) %>% as.data.table() 
  
  # summer range mean percent cover 
  summer = rast(paste0(dir.900m, layers[1])) %>% values() %>% na.omit() %>% as.data.table()
  percent.summer = colMeans(summer, na.rm = T) %>% as.data.table() 
  
  # winter range mean percent cover
  winter = rast(paste0(dir.900m, layers[3])) %>% values() %>% na.omit() %>% as.data.table()
  percent.winter = colMeans(winter, na.rm = T) %>% as.data.table() 
  
  # stack mean cover values and append to data table 
  percent.cover = rbind(percent.total, percent.summer, percent.winter)
  out = data.frame(percent.cover)
  out = set_names(out, paste0("percent_cover_", time))
  dt = cbind(dt, out)
  
  # clear memory 
  gc(verbose = F)
}

write.csv(dt, "E:/Caribou/pft_clipped/porcupine_pft_percent_change.csv")
