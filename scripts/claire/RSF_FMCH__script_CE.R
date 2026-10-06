# Fortymile Caribou RSF Script
# Isla Myers-Smith
# Adapted from https://terpconnect.umd.edu/~egurarie/teaching/SpatialModelling_AKTWS2018/6_RSF_SSF.html
#install.packages(c("vctrs", "dplyr", "tidyterra"), type = "binary")
# Packages
library(tidyverse)
library(sf)
library(terra)
library(lubridate)
library(gganimate)
library(viridis)
library(gifski)
#install.packages("tidyterra")
library(tidyterra)
#install.packages("adehabitatHR")
library(adehabitatHR)

# Load data
FMCH_obs <- read.csv("C:/Users/caeth/Documents/Data/Video Collar Data/gps_shifted_output.csv") 
  
# Load the spatial data file
elevation <- readRDS(file = "C:/Users/caeth/Downloads/data_for_spatial_analysis_class/data_for_spatial_analysis_class/elevation.rds")
agb_shrub     <- rast("C:/Users/caeth/Documents/Data/ABOVE PFT 2005-2020/pft_agb_deciduousshrub_p50_2010.tif")
cover_shrub   <- rast("C:/Users/caeth/Documents/Data/ABOVE PFT 2005-2020/ABoVE_PFT_2020/ABoVE_PFT_Top_Cover_DeciduousShrub_2020.tif")

# Project and resample agb and cover to match elevation
agb_resamp   <- project(agb_shrub, elevation, method = "bilinear")
cover_resamp <- project(cover_shrub, elevation, method = "bilinear")

# Run univariate models
m_elevation <- glm(Used ~ scale(elevation), family = binomial, data = data_rsf)
m_biomass   <- glm(Used ~ scale(shrub_AGB), family = binomial, data = data_rsf)
m_cover     <- glm(Used ~ scale(shrub_cover), family = binomial, data = data_rsf)

# Check p-values (looking for P < 0.25)
summary(m_elevation)
summary(m_biomass)
summary(m_cover)

# raster stack
predictors_stack <- c(elevation, agb_resamp, cover_resamp)

# rename to match GLM formula variables
names(predictors_stack) <- c("elevation", "shrub_AGB", "shrub_cover")

# define study area
study_area <- ext(-146, -137, 63, 65.5)
xmin <- study_area[1]; xmax <- study_area[2]; ymin <- study_area[3]; ymax <- study_area[4]

study_area_polygon <- st_sfc(st_polygon(list(matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin), ncol = 2, byrow = TRUE))))
study_area_sf <- st_as_sf(study_area_polygon)
st_crs(study_area_sf) <- 4326

# Crop rasterstack using study area
predictors_cropped <- crop(predictors_stack, vect(study_area_sf))

# Convert the cropped stack to a data frame for plotting
predictors_df <- as.data.frame(predictors_cropped, xy = TRUE) |> 
  rename(longitude = x, latitude = y)

# Sample background points inside study area
background <- st_sample(x = study_area_sf, size = nrow(FMCH_obs)) 
background_vect <- vect(background)
background_df   <- as.data.frame(background_vect, geom = "XY") |> 
  rename(longitude = x, latitude = y)

ggplot() +
  geom_raster(data = predictors_df, aes(x = longitude, y = latitude, fill = elevation)) +
  geom_point(data = FMCH_obs, aes(x = longitude, y = latitude), color = "pink", size = 0.05) +
  geom_spatvector(data = background_vect, color = "orange", size = 0.05) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

ggplot() +
  geom_raster(data = predictors_df, aes(x = longitude, y = latitude, fill = shrub_AGB)) +
  scale_fill_viridis_c(option = "mako", name = "AGB (g m⁻²)") +
  geom_point(data = FMCH_obs, aes(x = longitude, y = latitude), color = "pink", size = 0.1) +
  geom_spatvector(data = background_vect, color = "orange", size = 0.1, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", subtitle = "Predictor 2: 2010 Deciduous Shrub AGB") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot() +
  geom_raster(data = predictors_df, aes(x = longitude, y = latitude, fill = shrub_cover)) +
  scale_fill_viridis_c(option = "magma", name = "Cover (%)") +
  geom_point(data = FMCH_obs, aes(x = longitude, y = latitude), color = "pink", size = 0.1) +
  geom_spatvector(data = background_vect, color = "orange", size = 0.1, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", subtitle = "Predictor 3: 2020 Deciduous Shrub Cover") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Combine Used (caribou) and Available (background) points into one training frame
data_rsf <- data.frame(
  x = c(FMCH_obs$longitude, background_df$longitude), 
  y = c(FMCH_obs$latitude, background_df$latitude), 
  Used = c(rep(TRUE, nrow(FMCH_obs)), rep(FALSE, nrow(background_df)))
)

# Extract values from all 3 layers 
extracted_covariates <- terra::extract(predictors_cropped, data_rsf[, c("x", "y")])

# Append the extracted variables directly into data_rsf model dataframe
data_rsf$elevation   <- extracted_covariates$elevation
data_rsf$shrub_AGB   <- extracted_covariates$shrub_AGB
data_rsf$shrub_cover <- extracted_covariates$shrub_cover

# Clean out any points that fell outside the raster boundaries (NA values)
data_rsf <- data_rsf |> 
  filter(!is.na(elevation) & !is.na(shrub_AGB) & !is.na(shrub_cover))

# Diagnostic boxplots for the new environmental variables
boxplot(elevation ~ Used, data = data_rsf, main = "Elevation Selection")
boxplot(shrub_AGB ~ Used, data = data_rsf, main = "Shrub AGB Selection")
boxplot(shrub_cover  ~ Used, data = data_rsf, main = "Shrub Top Cover Selection")

# Fit the multi-predictor RSF model using logistic regression
# scale() normalizes the differing units (m vs g m-2 vs %) for fair coefficient comparison
RSF_fit <- glm(Used ~ scale(elevation) + scale(shrub_AGB) + scale(shrub_cover), 
               data = data_rsf, family = "binomial")

summary(RSF_fit)

# Visualize effect sizes (coefficients) using sjPlot
require(sjPlot)
plot_model(RSF_fit)

# Isolate the exact environmental inputs required by the model formula
model_inputs <- data_rsf |> 
  select(elevation, shrub_AGB, shrub_cover)

# Predict selection probability (0 to 1) for each individual training point
fmch_rsf <- data_rsf |>
  mutate(predicted_prob = predict(RSF_fit, newdata = model_inputs, type = "response")) |>
  select(x, y, predicted_prob)

# Convert the data frame back to a SpatVector using WGS84 coordinates
points_vect <- vect(fmch_rsf, geom = c("x", "y"), crs = "EPSG:4326")

# Define a template raster using 0.5-degree resolution size
template_raster <- rast(ext(points_vect), resolution = 0.5, crs = "EPSG:4326")

# Rasterize the point predictions by computing the mean value per cell block
rasterized_model <- rasterize(points_vect, template_raster, field = "predicted_prob", fun = "mean")


# Fit the model including quadratic (squared) terms for your predictors
RSF_quadratic <- glm(Used ~ poly(elevation, 2, raw = TRUE) + 
                       poly(shrub_AGB, 2, raw = TRUE) + 
                       poly(shrub_cover, 2, raw = TRUE), 
                     data = data_rsf, family = "binomial")

summary(RSF_quadratic)
# Convert the rasterized point model to a data frame for plotting
rasterized_model_df <- as.data.frame(rasterized_model, xy = TRUE) |> 
  rename(longitude = x, latitude = y, model = mean)

# This works for both the quadratic GLM and the GAM!
plot_model(RSF_quadratic, type = "eff", terms = "shrub_cover")
plot_model(RSF_quadratic, type = "eff", terms = "shrub_AGB")
plot_model(RSF_quadratic, type = "eff", terms = "elevation")

# Plot the intermediate rasterized point map
ggplot() +
  geom_raster(data = rasterized_model_df, aes(x = longitude, y = latitude, fill = model)) +
  scale_fill_viridis_c(option = "magma", name = "Mean Prob") +
  geom_point(data = FMCH_obs, aes(x = longitude, y = latitude), color = "pink", size = 0.1) +
  geom_spatvector(data = background_vect, color = "orange", size = 0.1, alpha = 0.3) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Predict RSF values across the entire landscape domain 
# This blends elevation, AGB, and cover into a single selection probability raster layer
rsf_prediction_raster <- predict(predictors_cropped, RSF_fit, type = "response")

# Final Integrated Visualization Output
ggplot() +
  # Plot the continuous multi-predictor selection surface layer
  geom_spatraster(data = rsf_prediction_raster) +
  scale_fill_viridis_c(option = "magma", name = "Selection\nProbability") +
  
  # Overlay available background locations
  geom_spatvector(data = background_vect, color = "orange", size = 0.05, alpha = 0.2) +
  
  # Overlay observed caribou points
  geom_point(data = FMCH_obs, aes(x = longitude, y = latitude), 
             color = "hotpink", size = 0.2) +
  
  # Formatting, Labels, and Styles
  theme_minimal() +
  labs(
    title = "Fortymile Caribou Multi-Predictor RSF Map",
        x = "Longitude",
    y = "Latitude",
    caption = "Pink = Observed Points | Orange = Background Points\nCoordinates tracked in WGS84 (EPSG:4326)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


library(brms)

# Fit the Bayesian logistic regression RSF model
RSF_bayes <- brm(
  formula = Used ~ scale(elevation) + scale(shrub_AGB) + scale(shrub_cover),
  data = data_rsf,
  family = bernoulli(link = "logit"),
  prior = c(
    prior(normal(0, 5), class = "Intercept"),
    prior(normal(0, 2.5), class = "b")
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 123
)

# View the summary of posterior estimates
summary(RSF_bayes)

# Plot posterior distributions and trace plots for diagnostics
plot(RSF_bayes)

#Visualize:
library(patchwork)

# 1. Shrub Top Cover Plot (Legend hidden for a cleaner stack)
p1 <- ggplot(data_rsf, aes(x = shrub_cover, fill = Used, color = Used)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  scale_fill_manual(values = c("FALSE" = "orange", "TRUE" = "hotpink"), labels = c("Available", "Used")) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "hotpink"), labels = c("Available", "Used")) +
  labs(x = "Shrub Top Cover (%)", y = "Density") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

# 2. Shrub AGB Plot (Legend hidden)
p2 <- ggplot(data_rsf, aes(x = shrub_AGB, fill = Used, color = Used)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  scale_fill_manual(values = c("FALSE" = "orange", "TRUE" = "hotpink"), labels = c("Available", "Used")) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "hotpink"), labels = c("Available", "Used")) +
  labs(x = "Deciduous Shrub AGB (g m⁻²)", y = "Density") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "none"
  )

# 3. Elevation Plot (Keep legend here so it applies to the whole stack)
p3 <- ggplot(data_rsf, aes(x = elevation, fill = Used, color = Used)) +
  geom_density(alpha = 0.4, linewidth = 0.8) +
  scale_fill_manual(values = c("FALSE" = "orange", "TRUE" = "hotpink"), labels = c("Available", "Used")) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "hotpink"), labels = c("Available", "Used")) +
  labs(x = "Elevation (m)", y = "Density", fill = "Status", color = "Status") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )

# Stack vertically using patchwork and add main titles
stacked_density_plots <- (p1 / p2 / p3) + 
  plot_annotation(
    title = "Fortymile Caribou Habitat Selection",
     )

# Display the final stacked figure
stacked_density_plots

plot_model(RSF_quadratic, type = "eff", terms = "shrub_cover")

#Visualize raster stack

# 1. Elevation Plot
p_elev <- ggplot() +
  geom_spatraster(data = predictors_cropped, aes(fill = elevation)) +
  scale_fill_viridis_c(option = "magma", name = "Elevation (m)", na.value = "transparent") +
  theme_minimal() +
  labs(title = "Elevation", x = "Longitude", y = "Latitude") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# 2. Shrub AGB Plot
p_agb <- ggplot() +
  geom_spatraster(data = predictors_cropped, aes(fill = shrub_AGB)) +
  scale_fill_viridis_c(option = "mako", name = "AGB (g m⁻²)", na.value = "transparent") +
  theme_minimal() +
  labs(title = "Deciduous Shrub AGB", x = "Longitude", y = "Latitude") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# 3. Shrub Cover Plot
p_cover <- ggplot() +
  geom_spatraster(data = predictors_cropped, aes(fill = shrub_cover)) +
  scale_fill_viridis_c(option = "rocket", name = "Cover (%)", na.value = "transparent") +
  theme_minimal() +
  labs(title = "Shrub Top Cover", x = "Longitude", y = "Latitude") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Combine side-by-side using patchwork
(p_elev | p_agb | p_cover) + 
  plot_annotation(
    title = "Fortymile Caribou Environmental Predictors",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

# Extract all conditional effects automatically from the brms model
effects_list <- conditional_effects(RSF_bayes)

# 1. Shrub Top Cover Plot (Now first)
p_cover <- ggplot(effects_list$shrub_cover, aes(x = shrub_cover, y = estimate__)) +
  geom_line(color = "hotpink", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "hotpink", alpha = 0.2) +
  labs(
    title = "Shrub Top Cover",
    x = "Shrub Cover (Scaled)",
    y = "Predicted Probability of Use"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11), panel.grid = element_blank())

# 2. Shrub Biomass (AGB) Plot (Now middle)
p_agb <- ggplot(effects_list$shrub_AGB, aes(x = shrub_AGB, y = estimate__)) +
  geom_line(color = "hotpink", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "hotpink", alpha = 0.2) +
  labs(
    title = "Shrub Biomass",
    x = "Shrub AGB (Scaled)",
    y = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11), panel.grid = element_blank())

# 3. Elevation Plot (Now last)
p_elev <- ggplot(effects_list$elevation, aes(x = elevation, y = estimate__)) +
  geom_line(color = "hotpink", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "hotpink", alpha = 0.2) +
  labs(
    title = "Elevation",
    x = "Elevation (Scaled)",
    y = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11), panel.grid = element_blank())

# Combine side-by-side using patchwork in the new order
presentation_figure <- (p_cover | p_agb | p_elev) + 
  plot_annotation(
    title = "Caribou Habitat Selection Across Key Environmental Gradients",
    subtitle = "Model-predicted probability of use based on Bayesian regression results",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10)
    )
  )

# Display the final reordered figure
presentation_figure
