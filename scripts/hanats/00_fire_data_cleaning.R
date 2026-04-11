library(terra)
library(dplyr)
library(magrittr)
library(ggplot2)

#### MERGE YUKON AND ALASKA FIRE HISTORY ####
yk = vect("D:/CaribouShapefiles/Yukon_Fires/Yukon_Fires.shp") %>%
  terra::subset("FIRE_YEAR")
yk$SOURCE = "Yukon"

ak = vect("D:/CaribouShapefiles/Alaska_Fires/Alaska_fire_history.shp") %>%
  terra::subset("FIREYEAR") %>%
  project(., yk)
names(ak) <- "FIRE_YEAR"
ak$SOURCE = "Alaska"

dt = rbind(values(yk), values(ak))

merged = rbind(yk, ak)
merged$FIRE_YEAR = dt$FIRE_YEAR
merged$AREA_KM2 = expanse(merged, unit = "km", transform = T)
## writeVector(merged, "D:/CaribouShapefiles/_Fire_history_merged/Fires_AK_YK.shp", overwrite=T)

### edit geometries of cross-border fires in arc ###

#### MERGE CARIBOU HERDS ####
tesh = vect("E:/Caribou/pft_clipped/projected_range/tesh_totalrange.shp", what = "geom") %>% project(., fires)
tesh$HERD = "Teshekpuk"

forty = vect("E:/Caribou/pft_clipped/projected_range/fourtymile_totalrange.shp", what = "geom") %>% project(., fires)
forty$HERD = "Fortymile"

western = vect("E:/Caribou/pft_clipped/projected_range/western_totalrange.shp", what = "geom") %>% project(., fires)
western$HERD = "Western Arctic"

porc = vect("E:/Caribou/pft_clipped/projected_range/total_range_macander.shp", what = "geom") %>% project(., fires)
porc$HERD = "Porcupine"

herds = rbind(tesh, forty, western, porc)
#writeVector(herds, "D:/CaribouShapefiles/YK_AK_herd_range/YK_AK_herd_range.shp")

#### INTERSECT FIRE HISTORY WITH HERDS ####
fires = vect("D:/CaribouShapefiles/_Fire_history_merged/Fires_AK_YK.shp")
herds = vect("D:/CaribouShapefiles/YK_AK_herd_range/YK_AK_herd_range.shp")

f.herds = terra::intersect(fires, herds)
f.herds$INTERSECT_AREA_KM = expanse(f.herds, unit = "km")

dt = f.herds %>% values() %>% as.data.table()
dt$FIRE_YEAR = as.numeric(dt$FIRE_YEAR)

## Add fire size as class 
dt = dt %>% mutate(FIRE_SIZE_CLASS = cut(INTERSECT_AREA_KM, breaks = c(0, 200, 10000), labels = c("< 200 km", "> 200 km")),
                   FIRE_YEAR_CLASS = cut(FIRE_YEAR, breaks = c(0, 1985, 2005, 2025), labels = c("Pre-1985", "1985-2005", "2000-2025")))

dt %>% group_by(HERD, FIRE_YEAR_CLASS) %>% summarise(total_area_km = sum(INTERSECT_AREA_KM))

#### Plot ####
png("D:/Caribou/figures/Porcupine_Fortymile_Fire_History.png", width = 9, height = 6, unit = "in", res = 300)
dt %>% filter(FIRE_YEAR < 2026) %>% filter(INTERSECT_AREA_HA > 100) %>%
  filter(HERD %in% c("Porcupine", "Fortymile")) %>%
  ggplot(., aes(x = FIRE_YEAR, fill = FIRE_CLASS)) +
  geom_bar(alpha = 0.8, color = "black", linewidth = 0.3)+
  scale_x_continuous(breaks = seq(1940, 2026, 5))+
  facet_wrap(~HERD)+
  labs(fill = "Fire Area")+
  scale_fill_brewer(palette = 'Reds')+
  xlab("Year")+
  ylab("Number of Fires")+
  theme_bw()+
  theme(axis.text = element_text(size = 11, angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill = "white", color = "black"))
dev.off()
