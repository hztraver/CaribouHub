library(terra)
library(randomForest)
library(supercells)
library(furrr)
library(future)

#Set parallel computing
plan(multisession, workers = 4)

#load orthomosaics
sixtymile_2018 <- rast("C:/Users/caeth/Documents/Data/orthomosaics/sixtymile_rgb_ortho_2018.tif")
sixtymile_2025 <- rast("C:/Users/caeth/Documents/Data/orthomosaics/sixtymile_rgb_ortho_2025.tif")
##Add NIR and RE ortho here

sixtymile_2018 <- project(sixtymile_2018, crs(sixtymile_2025))

#Raster stack RGB + RE, NIR

names(sixtymile_2018) <- c("Red", "Green", "")