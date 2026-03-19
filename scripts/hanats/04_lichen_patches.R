library(terra)
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

#### LICHEN CHANGE & PATCH DYNAMICS ####

## Use 900 m resolution layers for speed and testing
dir.900m = "E:/Caribou/lichen_clipped/aggregate_1km/"
list.900m = list.files(path = "E:/Caribou/lichen_clipped/aggregate_1km/", pattern = "*.tif$")
time.periods = c("1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")

#### LICHEN CHANGE ####
## Baseline lichen cover 1985 in winter range
## How many pixels have high lichen cover? 
## Of these areas with high lichen cover how much is retained over time?
## Are patches of high lichen cover connected? 

## Bin pixels by lichen presence and high cover in the winter range
## Summarize lichen cover in 900 m cells 
time.periods = c("1985", "2020")
cover_bins = c("< 1%", "1-5%", "5-10%" ,"10-20%", ">20%")

dt = data.frame(Cover = cover_bins)

for(time in time.periods){
  
  print(time)
  
  path = paste0("E:/Caribou/pft_clipped/Porcupine/aggregate_1km/winterrange_", time, "_900m.tif")
  lichen = rast(path)$LichenLight
  
  lichen.dt = lichen %>% values() %>% na.omit() %>% as.data.table() %>%
    mutate(cover_class = cut(LichenLight, breaks = c(-1, 1, 5, 10, 20, 60)))
  
  lichen.tab = table(lichen.dt$cover_class) / nrow(lichen.dt)
  lichen.tab = data.frame(lichen.tab)
  lichen.tab = data.frame(lichen.tab$Freq)
  lichen.tab = set_names(lichen.tab, paste0("percent_", time))
  
  dt = cbind(dt, lichen.tab)
}

### Generate lichen patches with > 15% cover
## use 900 m data for for testing
year = "2020"

l = rast(paste0("E:/Caribou/pft_clipped/Porcupine/aggregate_1km/winterrange_", year ,"_900m.tif"), lyrs = "LichenLight" )
l = ifel(l >= 15, 1, NA)
l.patches = patches(l, directions = 4, values = TRUE) %>% as.polygons(., round = TRUE)

## calculate some patch metrics
l.patches$AREA_KM = expanse(l.patches, unit = "km")
l.patches$PERIM_KM = perim(l.patches) / 1000
l.patches$SHAPE = l.patches$PERIM_KM / l.patches$AREA_KM

## nearest distance between patch centroids
near = nearest(l.patches, lines=T, centroids = T)
l.patches$NEAR_DIST = perim(near) / 1000

## subset to patches > 1 cell 
l.patches = terra::subset(l.patches, AREA_KM > 1, NSE=TRUE)

writeVector(l.patches, paste0("E:/Caribou/pft_clipped/Porcupine/patches/winterrange_", year ,"_patches.shp"), overwrite = T)

### Combine patch data and plot
years = c(1985, 2020)
dt = data.table()

for (year in years) {
  
  out = vect(paste0("E:/Caribou/pft_clipped/Porcupine/patches/winterrange_", year ,"_patches.shp")) %>% 
    values() %>% na.omit() %>% as.data.table()
  
  out$YEAR = year
  dt = rbind(dt, out)
  
}

dt %>% ggplot(aes(x = AREA_KM, fill = as.factor(YEAR), color = as.factor(YEAR))) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.3)+
  facet_wrap(~YEAR)+
  xlim(1,20)+
  theme_bw()

dt %>% group_by(YEAR) %>% 
  summarise(n_patches = n(),
            total_area = sum(AREA_KM, na.rm = T),
            mean_area = mean(AREA_KM, na.rm=T),
            se_area = sd(AREA_KM, na.rm=T) / mean(AREA_KM, na.rm=T),
            mean_nn_dist = mean(NEAR_DIST),
            mean_shape = mean(SHAPE))

### Intersect with fire history
