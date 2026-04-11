library(terra)
library(dplyr)

# get the projection information for one of the layers
lichen.1985 = rast("E:/Caribou/Macander_TopCover_PFT/ABoVE_PFT_Top_Cover_tmLichenLight_2020.tif")
lichen.crs = crs(lichen.1985)

dir = "D:/CaribouShapefiles/herd_ranges/"
out = "E:/Caribou/pft_clipped/projected_range/"

# western
west = vect(paste0(dir, "WesternArcticCaribouHerdRange/NOS_TS_C_180701_Caribou_WesternWinterRange.shp")) %>%
  project(., lichen.crs) %>%
  writeVector(., filename = paste0(out, "western_winterrange.shp"))

# fourtymile
fourty = vect(paste0(dir, "FMCH_ExpandedRange_Aligned/FMCH_Range_ABoVE_Aligned.shp")) %>%
  project(., lichen.crs) %>%
  writeVector(., filename = paste0(out, "fourtymile_totalrange.shp"))

# teshekpuk
tesh = vect(paste0(dir, "TeshekpukCaribouHerdRange/NOS_TS_C_180701_Caribou_TeshekpukWinterRange.shp")) %>%
  project(., lichen.crs) %>%
  writeVector(., filename = paste0(out, "tesh_winterrange.shp"))

