library(terra)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(forcats)

#### LICHEN PERCENT CHANGE BURNED VS UNBURNED ####
## Template raster using porcupine and fortymile extent
r = rast("D:/Caribou/pft_clipped/Porcupine/aggregate_1km/winterrange_2020_900m.tif")
rf = rast("D:/Caribou/pft_clipped/Fourtymile/aggregate_1km/Fourtymile_totalrange_2020_900m.tif")
r.all = merge(r, rf)

## Raster for fire year 
fires = vect("D:/CaribouShapefiles/_Fire_history_merged/Fires_AK_YK.shp") %>% project(., r.all)
fires$FIRE_YEAR = as.numeric(fires$FIRE_YEAR)
f.rast = rasterize(fires, r.all, field = "FIRE_YEAR", background = 0) 

## Raster for herd 
herds = vect("D:/CaribouShapefiles/YK_AK_herd_range/YK_AK_herd_range.shp") %>% project(., r.all) 
h.rast = rasterize(herds, r.all, field = "HERD", fun = "min")

# Extract values for lichen cover and burn year for 1985 and 2020
years = c("1985", "2020")
cover_bins = c("< 1%", "1-5%", "5-10%" ,"10-20%", ">20%")
year_bins = c("Unburned", "Pre-1985", "1985-2000", "2000-2020")
dt = data.frame(Cover = cover_bins)

for(year in years){
  
  print(year)
  
  p = rast(paste0("D:/Caribou/pft_clipped/Porcupine/aggregate_1km/winterrange_", year, "_900m.tif"), lyrs = "LichenLight") 
  f = rast(paste0("D:/Caribou/pft_clipped/Fourtymile/aggregate_1km/Fourtymile_totalrange_", year, "_900m.tif"), lyrs = "LichenLight")
  
  all = merge(p, f)
  
  lichen.dt = c(all, f.rast, h.rast) %>% values() %>% na.omit() %>% as.data.table() %>%
    mutate(cover_class = cut(LichenLight, breaks = c(-1, 1, 5, 10, 20, 60), labels = cover_bins),
           year_class = cut(FIRE_YEAR, breaks = c(-1, 0, 1985, 2000, 2021), labels = year_bins),
           Year = year)
  
  write.csv(lichen.dt, paste0("D:/Caribou/pft_cover_tables/lichen_burned_", year, ".csv"))

}

# combine tables with herd, fire year, lichen cover for 1985 and 2020
dt = list.files("D:/Caribou/pft_cover_tables/", pattern = "lichen_burned.*\\.csv$", full.names = T) %>%
  lapply(fread) %>% 
  rbindlist() %>% 
  na.omit()

# Make bins for fire year
year_bins = c("Unburned", "Pre-1985", "1985-2000", "2000-2020")
dt$year_class = factor(dt$year_class, levels = year_bins)
# binary burn unburn
dt$burn_binary = ifelse(dt$year_class == "Unburned", "Unburned", "Burned")

# Make herd a factor 
herd_names = c("Fortymile", "Porcupine")
dt$HERD = factor(dt$HERD, labels = herd_names)

dt %>% group_by(HERD, Year, burn_binary) %>% 
  summarise(percent_cover = mean(LichenLight), 
            total_area_km2 = (n()*0.81), 
            lichen_area = (percent_cover / 100) * total_area_km2) 

dt %>% group_by(HERD, Year, cover_class) %>%
  summarise(cover_n = n(), .groups = "drop_last") %>%
  mutate(prop = cover_n / sum(cover_n))

dt %>% ggplot(., aes(x = burn_binary, y = LichenLight, fill = as.factor(Year))) +
  geom_boxplot(outlier.alpha = 0) + 
  ylab("Lichen Percent Cover")+
  xlab("Fire year")+
  ylim(c(0, 25))+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12), 
        legend.position = "bottom")

#### RECOVERY OF PIXELS BURNED 2000-2005 ####

## mask to find fires 2000-2005
## mask pfts and recovery covariates to only get pixels burned in this period
f.rast = ifel(f.rast >= 2000 & f.rast <= 2005, 1, NA)

## topography and dNBR covariates 
# these are static so extract pixel values and join to recovery values
topo = rast("D:/Caribou/DEM/combined_DEM.tif")  %>% project(., r.all) %>% resample(., r.all) %>% mask(., f.rast) %>% mask(., r.all[[1]])
dnbr = rast("D:/Caribou/NBR/dNBR_2000_2005.tif") %>% project(., r.all) %>% resample(., r.all) %>% mask(., f.rast) %>% mask(., r.all[[1]])
covars = c(topo, dnbr) %>% values() %>% as.data.table() %>% na.omit()

write.csv(covars, "D:/Caribou/pft_cover_tables/fire_recovery_2004/_covariates.csv")

### extract pft values for burned pixels over years
years = seq(1985, 2020, by = 5)

for(year in years) {
  
  print(year)
  
  p = rast(paste0("D:/Caribou/pft_clipped/Porcupine/aggregate_1km/winterrange_", year, "_900m.tif"))
  f = rast(paste0("D:/Caribou/pft_clipped/Fourtymile/aggregate_1km/Fourtymile_totalrange_", year, "_900m.tif"))
  
  pfts = merge(p, f) %>% mask(., f.rast) %>% mask(., dnbr) %>% mask(., topo) %>% ## mask using covariates 
    values() %>% as.data.table() %>% na.omit()
  
  pfts$YEAR = year
  dt.long = pfts %>% pivot_longer(., cols = BroadleafTree:LichenLight, names_to = "PFT", values_to = "Percent")
  
  write.csv(dt.long, paste0("D:/Caribou/pft_cover_tables/fire_recovery_2004/pft_burned_", year, ".csv"))
  
}

#### Plot change in pfts within pixels ####
## Combine years
pft_colors = c("#008B00", "#FF8C00", "#8B4513", "#104E8B", "#00B2EE", "#FFC125")

dt = list.files("D:/Caribou/pft_cover_tables/fire_recovery_2004/", pattern = "pft_burned.*\\.csv$", full.names = T) %>%
  lapply(fread) %>% 
  rbindlist()

dt.means = dt %>% group_by(YEAR, PFT) %>% summarise(year_mean = mean(Percent, na.rm=T), .groups = "drop") 

png("D:/_Presentations/fig2.png", width = 4, height = 4, unit = "in", res = 300)
dt.means  %>% filter(PFT %in% c("DeciduousShrub", "LichenLight")) %>%
  ggplot(., aes(x = YEAR, y = year_mean, color = PFT)) +
  geom_vline(xintercept = 2002.5, color = "red", linetype = "dashed")+
  scale_color_manual(values = pft_colors)+
  geom_point(size = 3) +
  geom_line() +
  ylab("Percent Cover") +
  xlab("Year")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 16), 
        legend.position = "none")
dev.off()

png("D:/Caribou/figures/pft_recovery_trends.png", width = 6, height = 6, unit = "in", res = 300)
dt %>% filter(PFT != "BroadleafTree") %>%
  ggplot(., aes(x = as.factor(YEAR), y = Percent, fill = PFT, alpha = 0.3)) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "grey")+
  geom_boxplot(outlier.alpha = 0) +
  scale_fill_manual(values = pft_colors)+
  facet_wrap(~PFT)+
  coord_cartesian(ylim = c(0, 40))+
  ylab("Percent Cover") +
  xlab("Year")+
  theme_bw()+
  theme(axis.text = element_text(size = 11, angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12), 
        legend.position = "none")
dev.off()

#### COMPILE LICHEN RECOVERY METRICS AND COVARIATES ####
## Lichen recovery summary statistics
pre = dt %>% filter(PFT == "LichenLight") %>% filter(YEAR < 2000) %>% group_by(V1) %>% summarise(mean_pre = mean(Percent))
post05 = dt %>% filter(PFT == "LichenLight") %>% filter(YEAR == 2005)
post10 = dt %>% filter(PFT == "LichenLight") %>% filter(YEAR == 2010) 
post20 = dt %>% filter(PFT == "LichenLight") %>% filter(YEAR == 2020)
shrub85 = dt %>% filter(PFT == "DeciduousShrub") %>% filter(YEAR < 2000) %>% group_by(V1) %>% summarise(mean_pre = mean(Percent))
shrub20 = dt %>% filter(PFT == "DeciduousShrub") %>% filter(YEAR == 2020)
tree85 = dt %>% filter(PFT == "ConiferTree") %>% filter(YEAR < 2000) %>% group_by(V1) %>% summarise(mean_pre = mean(Percent))

## Contruct recovery from pft data
rec = data.table(cbind(pre, post05$Percent, post10$Percent, post20$Percent, shrub85$mean_pre, shrub20$Percent, tree85$mean_pre))
names(rec) <- c("V1","mean_lich_pre", "lich_percent_05", "lich_percent_10", "lich_percent_20", "shrub_pre", "shrub_20", "tree_pre")

## dNBR & elevation covariates
cov = fread("D:/Caribou/pft_cover_tables/fire_recovery_2004/_covariates.csv")
rec = cbind(rec, cov$elevation, cov$nd)

rec = rec %>% filter(mean_lich_pre > 5) %>% mutate(end_recovered = (lich_percent_20 / mean_lich_pre) * 100)
mean(rec$end_recovered)

write.csv(rec, "D:/Caribou/pft_cover_tables/fire_recovery_2004/_recovery_metrics.csv")

#### MODELING RECOVERY ####
e = ggplot(aes(x = end_recovered, y = ..density..), data = rec)+ 
  geom_histogram(binwidth = 5, color = "black", alpha = 0.3)+ 
  geom_vline(xintercept = mean(rec$end_recovered), color = "red", linetype = "dashed")+
  xlim(c(0,150))+
  xlab("% Recovery by 2020")+
  ylab("Density")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 16))

a = ggplot(aes(y = end_recovered, x = V3), data = rec)+
  geom_point(alpha = 0.1, size = 1, color = "#B22222")+
  geom_smooth(method = "lm", se = FALSE, color = "#363636", linewidth = 1.5)+
  ylab("Lichen percent recovery")+
  xlab("dNBR (fire severity)")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 16))

b = ggplot(aes(y = end_recovered, x = V2), data = rec)+
  geom_point(alpha = 0.1, size = 1, color = "dodgerblue3")+
  geom_smooth(method = "lm", se = FALSE, color = "#363636", linewidth = 1.5)+
  ylab("Lichen percent recovery")+
  xlab("Elevation (m)")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 16))

c = ggplot(aes(y = end_recovered, x = (shrub_20 - shrub_pre)), data = rec)+
  geom_point(alpha = 0.1, size = 1, color = "#008B00")+
  geom_smooth(method = "lm", se = FALSE, color = "#363636", linewidth = 1.5)+
  ylab("Lichen percent recovery")+
  xlab("Shrub percent change")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 16))

d = ggplot(aes(y = end_recovered, x = mean_lich_pre), data = rec)+
  geom_point(alpha = 0.1, size = 1, color = "#FF8C00")+
  geom_smooth(method = "lm", se = FALSE, color = "#363636", linewidth = 1.5)+
  ylab("Lichen percent recovery")+
  xlab("Pre-fire lichen cover")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 16))

f = ggplot(aes(y = end_recovered, x = tree_pre), data = rec)+
  geom_point(alpha = 0.1, size = 2)+
  geom_smooth(method = "lm")+
  ylab("Lichen percent recovery")+
  xlab("Pre-fire tree cover")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 16))

library(gridExtra)

png("D:/Caribou/figures/lichen_recovery.png", height = 8, width = 10, res = 300, units = "in")
grid.arrange(e,a,b,c,d,f, nrow = 2)
dev.off()

png("D:/_Presentations/fig3.png", height = 8, width = 8, res = 300, units = "in")
grid.arrange(a,b,c,d, nrow = 2)
dev.off()

ggplot(aes(x = (shrub_20 - shrub_pre), y = V2), data = rec)+
  geom_point(alpha = 0.1, size = 2)+
  geom_smooth(method = "lm")+
  xlab("Shrub change")+
  ylab("elevation")+
  theme_classic()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 16))
