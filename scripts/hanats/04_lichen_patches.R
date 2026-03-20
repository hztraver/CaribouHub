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
            mean_shape = mean(SHAPE),
            se_shape = sd(SHAPE) / mean(SHAPE))

### Intersect lichen 1985 with fire history
fires = vect("D:/CaribouShapefiles/_Fire_history_merged/Fires_AK_YK.shp")
l20 = vect("E:/Caribou/pft_clipped/Porcupine/patches/winterrange_2020_patches.shp", what = "geoms") %>% project(., fires)
l85 = vect("E:/Caribou/pft_clipped/Porcupine/patches/winterrange_1985_patches.shp", what = "geoms") %>% project(., fires)

# Area of new lichen patches
gain = erase(l20, l85)
sum(expanse(gain, unit = "km"))
length(gain)

# Area of lichen patch losses
loss = erase(l85, l20)
sum(expanse(loss, unit = "km"))
length(loss)

# Intersection of losses with fires
fl = terra::intersect(loss, fires)
fl$INTERSECT_AREA = expanse(fl, unit = "km")
# total area in fires
sum(fl$INTERSECT_AREA)
fl$FIRE_YEAR = as.numeric(fl$FIRE_YEAR)

year_labels = c("1940-1985", "1985-2000", "2000-2015", "2015-2025")
dt = fl %>% values() %>% as.data.table() %>%
  mutate(year_class = cut(FIRE_YEAR, breaks = c(1940, 1985, 2000 , 2015, 2025), labels = year_labels))

## Lichen patch losses in fires by fire year
dt %>% group_by(year_class) %>% summarise(area_in_fire = sum(INTERSECT_AREA, na.rm=T))
