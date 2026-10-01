library(terra)
library(sf)
library(tidyverse)
library(lubridate)
library(broom)
library(broom.mixed) 
library(brms)        

# =========================================================
# 1. SPATIAL DATA PROCESSING
# =========================================================
# Load classified orthos
raster_files <- list.files(path = "C:/Data/Drone data/Classified Orthos", 
                           pattern = "\\.tif$", 
                           full.names = TRUE)

# Build the metadata table 
file_inventory <- tibble(raster_path = raster_files) %>%
  mutate(
    filename = basename(raster_path),
    year = str_extract(filename, "\\d{4}"),
    site_name = str_remove_all(filename, "^classified_|_\\d{4}\\.tif$"),
    time_period = if_else(year %in% c("2018", "2019"), "T1", "T2")
  )

# Load the FMCH range shapefile to calculate the shift
FMCH_range <- st_read("C:/Data/FMCH range shapefiles (from Mike Suitor)/Fortymile_Caribou_Expanded_Range_2013-2014_Final/Fortymile_Caribou_Expanded_Range_2013-2014_Final.shp")

get_site_footprint <- function(raster_path, site_name) {
  r <- rast(raster_path)
  poly <- as.polygons(ext(r), crs = crs(r))
  sf_poly <- st_as_sf(poly) %>%
    st_transform(3338) %>%  # Project directly to Alaska Albers
    mutate(site = site_name)
  return(sf_poly)
}

drone_sites_3338 <- file_inventory %>%
  select(raster_path, site_name) %>%
  pmap(get_site_footprint) %>%
  bind_rows()

# Shift GPS collar data
gps_data <- read_csv("C:/Users/caeth/Documents/Data/Video Collar Data/dc_JANEdata_eating_modlocs.csv")

gps_processed <- gps_data %>%
  mutate(t_ = ymd_hms(t_)) %>%
  arrange(id, t_) %>%
  group_by(id) %>%
  mutate(time_diff_hrs = as.numeric(difftime(lead(t_), t_, units = "hours"))) %>%
  ungroup() %>%
  mutate(time_diff_hrs = if_else(time_diff_hrs > 3 | is.na(time_diff_hrs), 0, time_diff_hrs))

# Create spatial object and project to Alaska Albers
FMCH_pts_sf <- st_as_sf(gps_processed, coords = c("x_", "y_"), crs = 4269)
FMCH_pts_3338 <- st_transform(FMCH_pts_sf, 3338)
FMCH_range_3338 <- st_transform(FMCH_range, 3338)

# Apply the centroid shift
range_center  <- st_centroid(st_union(FMCH_range_3338))
points_center <- st_centroid(st_union(FMCH_pts_3338))
shift_x <- st_coordinates(range_center)[1] - st_coordinates(points_center)[1]
shift_y <- st_coordinates(range_center)[2] - st_coordinates(points_center)[2]

gps_shifted_3338 <- FMCH_pts_3338
st_geometry(gps_shifted_3338) <- st_geometry(FMCH_pts_3338) + c(shift_x, shift_y)
st_crs(gps_shifted_3338) <- 3338 

# Buffer the drone sites by 5km 
drone_sites_buffered <- st_buffer(drone_sites_3338, dist = 5000)

# Intersect the shifted GPS points with the buffered drone sites
gps_in_sites <- st_join(gps_shifted_3338, drone_sites_buffered, left = FALSE)

# Sum foraging hours
site_residence <- gps_in_sites %>%
  st_drop_geometry() %>%
  filter(behavior == "Eating") %>% 
  group_by(site) %>%
  summarise(
    total_residence_hours = sum(time_diff_hrs, na.rm = TRUE),
    total_gps_fixes = n()
  )

# Fill zeros for unused sites
all_site_residence <- drone_sites_3338 %>%
  st_drop_geometry() %>%
  left_join(site_residence, by = "site") %>%
  replace_na(list(total_residence_hours = 0, total_gps_fixes = 0))

model_data <- cover_change %>%
  left_join(all_site_residence, by = "site")

# =========================================================
# 2. BAYESIAN MODELING (brms)
# =========================================================
# Group the data by PFT and run a Bayesian model for each
pft_models <- model_data %>%
  filter(!is.na(pft_name)) %>%
  group_by(pft_name) %>%
  nest() %>%
  mutate(
    # Fit the Bayesian regression
    model = map(data, ~ brm(change_percent ~ total_residence_hours, 
                            data = .x, 
                            family = gaussian(),
                            chains = 4, 
                            iter = 2000, 
                            warmup = 1000,
                            seed = 123,
                            silent = 2, 
                            refresh = 0)), # Silences the massive text outputs
    
    # Extract the coefficients (Estimate, Error, and 95% Credible Intervals)
    tidied = map(model, ~ tidy(.x, effects = "fixed", conf.int = TRUE)),
    
    # Extract the Bayesian R-squared
    bayesian_r2 = map(model, ~ as.data.frame(bayes_R2(.x)))
  )

# Extract the slope and 95% Credible Intervals for the 'total_residence_hours' predictor
pft_coefficients <- pft_models %>%
  unnest(tidied) %>%
  filter(term == "total_residence_hours") %>%
  select(pft_name, estimate, std.error, conf.low, conf.high) %>%
  arrange(estimate)

pft_coefficients

# Extract the overall Bayesian R-squared for each model
pft_model_stats <- pft_models %>%
  unnest(bayesian_r2) %>%
  select(pft_name, Estimate, Est.Error, Q2.5, Q97.5) %>%
  rename(R2_Estimate = Estimate)

pft_model_stats

# =========================================================
# 3. BAYESIAN DIAGNOSTICS
# =========================================================
# Loop through each PFT model to visually check Markov Chain convergence
for(i in 1:nrow(pft_models)) {
  
  current_pft <- pft_models$pft_name[i]
  current_model <- pft_models$model[[i]]
  
  # Generates trace plots for the Bayesian chains
  plot(current_model, ask = FALSE) 
  
  # Pauses the loop so you can inspect
  readline(prompt = paste("Showing MCMC traces for", current_pft, "- Press [Enter] to continue..."))
}

# =========================================================
# 4. VISUALIZATION
# =========================================================
# Plot the frequentist linear regression line over the data 
# (Plotting true Bayesian posterior ribbons requires tidybayes, so we stick to smooth_lm for visual ease)
ggplot(model_data %>% filter(!is.na(pft_name)), 
       aes(x = total_residence_hours, y = change_percent, color = pft_name, fill = pft_name)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", alpha = 0.2, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_wrap(~pft_name, scales = "free_y") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  labs(
    title = "Impact of Foraging Residence Time on PFT Change",
    x = "Caribou Foraging Residence Time (Hours)",
    y = "PFT Cover Change (%)"
  )

# Extract the slope, Credible Intervals, AND the Rhat diagnostic
pft_coefficients <- pft_models %>%
  unnest(tidied) %>%
  filter(term == "scaled_residence_hours") %>%
  # We add the 'rhat' column to the extraction
  select(pft_name, estimate, std.error, conf.low, conf.high, rhat) %>%
  arrange(estimate)

pft_coefficients