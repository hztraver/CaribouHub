library(terra)
library(dplyr)
library(data.table)
library(magrittr)

dir = "I:/PFT_FMCH range/"
layers = list.files(dir, pattern = "*.tif$")

l2010 = rast("I:/PFT_FMCH range/PFT_masked_2005.tif")

## aggregate to 900m 
for (layer in layers){
  
  print(layer)
  
  r = rast(paste0(dir, layer)) %>% terra::aggregate(., fact = 30, fun = "mean", cores = 4)
  
  layer.out = gsub(".tif", "_900m.tif", layer)
  writeRaster(r, filename = paste0(dir, layer.out))
  
  gc(verbose = F)
  
}

## get total percent cover
layers.900m = list.files(dir, pattern = "*_900m.tif$")
pft = names(rast("I:/PFT_FMCH range/PFT_masked_2020_900m.tif"))
years = c("2005", "2010", "2015", "2020")

dt = data.table(PFT = pft)

for (year in years) {
  
  print(year)
  
  layer = grep(year, layers.900m, value = T)
  
  dt.year = rast(paste0(dir, layer)) %>% values() %>% na.omit() %>% as.data.table()
  dt.means = dt.year %>% colMeans(., na.rm = T) %>% as.data.table()
  names(dt.means) = paste0("percent_cover_", year)
  
  dt = cbind(dt, dt.means)
  
  gc(verbose=F)
}

write.csv(dt, "I:/PFT_FMCH range/total_percent_cover.csv")
