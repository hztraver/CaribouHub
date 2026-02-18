library(terra)
library(dplyr)
library(data.table)
library(magrittr)

#### SUMMARY STATISTICS FOR TOTAL PERCENT LICHEN COVER
#### Calculate overall percent cover in total,  summer and winter range 

# Folder of lichen percent cover 30 m layers
dir = "E:/Caribou/lichen_clipped/"
lichen.layers = list.files(path = "E:/Caribou/lichen_clipped", pattern = "*.tif$")

## Aggregate 30 m percent cover to 900 m spatial resolution to reduce file size
## since we can just sum percent cover in larger areas without losing information 
# it will be faster to plot and summarize the data at a coarser resolution 
for (layer in lichen.layers){
  
  print(layer)
  
  # folder for 900 m layers
  layer.out = gsub(".tif", "_900m.tif", layer)
  path.out = paste0("E:/Caribou/lichen_clipped/aggregate_1km/", layer.out)
  
  # aggregate pixels by a factor of 30 and sum percent cover values 
  # then divide by the number of pixels to get percent cover within each 900 m cell
  # (alternatively just use fun = "mean", but I thought this made intuitive sense)
  r = rast(paste0(dir, layer)) %>% terra::aggregate(., fact = 30, fun = "sum")
  r = r/900
  
  writeRaster(r, filename = path.out, overwrite = T)
  gc(verbose = F)
}

## Sum the 900 m layers to get overall percent cover in total, summer and winter range
## for each time period
dir.900m = "E:/Caribou/lichen_clipped/aggregate_1km/"
list.900m = list.files("E:/Caribou/lichen_clipped/aggregate_1km/")
time.periods = c("1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")

# Calculate total area of ranges (tota, summer, winter) in km2
total.range.area = expanse(rast(paste0(dir.900m, "totalrange_LichenLight_1985_900m.tif")), unit = "km")$area 
summer.area = expanse(rast(paste0(dir.900m, "summerrange_LichenLight_1985_900m.tif")), unit = "km")$area 
winter.area = expanse(rast(paste0(dir.900m, "winterrange_LichenLight_1985_900m.tif")), unit = "km")$area 

range.area = c(total.range.area, summer.area, winter.area)

# Write the total percent cover to a data frame
dt = data.frame(range = c("total", "summer", "winter"), area_km2 = range.area)

for (time in time.periods) {
  
  print(time)
  
  # Percent cover maps for summer range, total range, winter range (in that order)
  layers = grep(time, list.900m, value = T)
  
  total = rast(paste0(dir.900m, layers[2])) %>% values() %>% na.omit() %>% as.data.table()
  percent.total = mean(total$cover)
  
  summer = rast(paste0(dir.900m, layers[1])) %>% values() %>% na.omit() %>% as.data.table()
  percent.summer = mean(summer$cover)
  
  winter = rast(paste0(dir.900m, layers[3])) %>% values() %>% na.omit() %>% as.data.table()
  percent.winter = mean(winter$cover)
  
  percent.cover = c(percent.total, percent.summer, percent.winter)
  out = data.frame(percent.cover)
  out = set_names(out, paste0("percent_cover_", time))
  dt = cbind(dt, out)
}

dt
write.csv(dt, "E:/Caribou/lichen_clipped/lichen_percent_change.csv")
