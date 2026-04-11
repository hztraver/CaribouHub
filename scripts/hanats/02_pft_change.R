library(terra)
library(dplyr)
library(data.table)
library(magrittr)

#### SUMMARY STATISTICS FOR TOTAL PFT COVER ####
#### Calculate overall percent cover in total,  summer and winter range 

# Folder of lichen percent cover 30 m layers
dir = "D:/Caribou/pft_clipped/"
herds = c("Fourtymile", "Teshekpuk", "Western", "Porcupine")

## Aggregate 30 m percent cover to 900 m spatial resolution to reduce file size
## since we can just sum percent cover in larger areas without losing information 
# it will be faster to plot and summarize the data at a coarser resolution 
for (herd in herds){
  
  print(herd)
  
  herd.path = paste0(dir, herd, "/")
  pft.layers = list.files(path = herd.path, pattern = "*.tif$")
  
  for (layer in pft.layers){
    
    print(layer)
    
    # folder for 900 m layers
    layer.out = gsub(".tif", "_900m.tif", layer)
    path.out = paste0(herd.path,"/aggregate_1km/",layer.out)
    
    # aggregate pixels by a factor of 30 and sum percent cover values 
    r = rast(paste0(herd.path, layer)) %>% terra::aggregate(., fact = 30, fun = "mean", cores = 4)
    
    writeRaster(r, filename = path.out, overwrite = T)
    gc(verbose = F)
  }
  
}

## Sum the 900 m layers to get overall percent cover in total, summer and winter range
## for each time period
dir.900m = "D:/Caribou/pft_clipped/Teshekpuk/aggregate_1km/"
list.900m = list.files(path = dir.900m, pattern = "*.tif$")
time.periods = c("1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")
pft.names = c("BroadleafTree", "ConiferTree", "DeciduousShrub", "EvergreenShrub", "Forb", "Graminoid", "LichenLight")

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

write.csv(dt, "E:/Caribou/pft_clipped/teshekpuk_pft_percent_change.csv")

# Calculate total area of ranges (tota, summer, winter) in km2
## Combine porcupine range shapefiles into one 
summer = vect("E:/Caribou/pft_clipped/projected_range/summer_range_macander.shp")
winter = vect("E:/Caribou/pft_clipped/projected_range/winter_range_macander.shp")
total = vect("E:/Caribou/pft_clipped/projected_range/total_range_macander.shp")

range = rbind(total, summer, winter)
range$RANGE = c("Total", "Summer", "Winter")
range.area = data.frame(Range = range$RANGE, Area_km2 = expanse(range, unit = "km"))

#### Figures showing overall change in PFT across ranges
library(ggplot2)
library(tidyr)
library(vroom)
library(forcats)

## All herds
# files = list.files("E:/Caribou/pft_clipped/", pattern = "*.csv$", full.names = T)
# dt = vroom(files)
# write.csv(dt, "E:/Caribou/pft_clipped/all_herds_PFT_change.csv")

dt = read.csv("D:/Caribou/pft_clipped/all_herds_PFT_change.csv")

# set colors for plot
pft_colors = c("#BCEE68", "#008B00", "#FF8C00", "#8B4513", "#104E8B", "#00B2EE", "#FFC125")

total = dt %>% 
  pivot_longer(., cols = starts_with("percent_cover"), names_to = "Year", values_to = "Percent_cover") %>%
  mutate(Year = as.numeric(gsub("percent_cover_", "", Year))) %>%
  mutate(range = factor(range)) %>% 
  mutate(range = fct_recode(range, "Summer" = "summer", "Winter" = "winter", "Total" = 'total'))

## Plot Porcupine caribou summer and winter range 
png("E:/Caribou/figures/Porcupine_PFT.png", width = 6, height = 4, unit = "in", res = 300)
total %>% filter(., herd == "Porcupine") %>% filter(range != "Total") %>%
ggplot(., aes(x = Year, y = Percent_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  facet_wrap(~range)+
  xlab("Year")+
  ylab("Percent cover")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))
dev.off()

## Summer & Winter ranges 
png("E:/Caribou/figures/barren_ground_PFT.png", width = 8, height = 6, unit = "in", res = 300)
total %>% filter(herd != "Fourtymile") %>% filter(range != "Total") %>%
  ggplot(., aes(x = Year, y = Percent_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  facet_wrap(~range + herd)+
  xlab("Year")+
  ylab("Percent cover")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))
dev.off()

## Total range including Fourtymile
png("D:/Caribou/figures/Porc_Forty_total_range_PFT.png", width = 6, height = 4, unit = "in", res = 300)
total %>% filter(range == "Total") %>% filter(herd %in% c("Fortymile", "Porcupine")) %>%
  ggplot(., aes(x = Year, y = Percent_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  facet_wrap(~herd)+
  xlab("Year")+
  ylab("Percent cover")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12), legend.position = "none")
dev.off()

#### Relative cover 
## drop PFTs where cover in 1985 < 1.1 %
relative = dt %>% filter(percent_cover_1985 > 1.1) %>% 
  mutate(across(percent_cover_1985:percent_cover_2020, ~ .x/percent_cover_1985)) %>%
  pivot_longer(., cols = starts_with("percent_cover"), names_to = "Year", values_to = "Relative_cover") %>%
  mutate(Year = as.numeric(gsub("percent_cover_", "", Year))) %>%
  mutate(range = factor(range)) %>% 
  mutate(range = fct_recode(range, "Summer" = "summer", "Winter" = "winter", "Total" = 'total'))

## Plot Porcupine caribou summer and winter range 
png("E:/Caribou/figures/Porcupine_rel_PFT.png", width = 6, height = 4, unit = "in", res = 300)
relative %>% filter(., herd == "Porcupine") %>% filter(range != "Total") %>%
  ggplot(., aes(x = Year, y = Relative_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~range)+
  xlab("Year")+
  ylab("Cover relative to 1985")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))
dev.off()

## Summer & Winter ranges
png("E:/Caribou/figures/barren_ground_rel_PFT.png", width = 8, height = 6, unit = "in", res = 300)
relative %>% filter(herd != "Fourtymile") %>% filter(range != "Total") %>%
  ggplot(., aes(x = Year, y = Relative_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~range + herd)+
  xlab("Year")+
  ylab("Cover relative to 1985")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))
dev.off()

## Total range including Fourtymile
png("D:/_Presentations/fig.png", width = 6, height = 4, unit = "in", res = 300)
relative %>% filter(range == "Total") %>% 
  filter(herd %in% c("Fortymile", "Porcupine")) %>%
  filter(PFT %in% c("DeciduousShrub", "LichenLight")) %>%
  ggplot(., aes(x = Year, y = Relative_cover, colour = PFT))+
  geom_point(size = 3)+
  scale_color_manual(values = pft_colors)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_line(alpha = 0.4)+
  facet_wrap(~herd)+
  xlab("Year")+
  ylab("Cover relative to 1985")+
  theme_classic()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
dev.off()



