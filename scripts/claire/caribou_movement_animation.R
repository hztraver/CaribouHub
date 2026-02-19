library(tidyverse)
library(sf)
#install.packages("gganimate")
library(gganimate)
library(gifski)

# Load data
FMCH_eating <- read.csv("dc_JANEdata_eating_modlocs.csv")

# Convert to a spatial object
# Use 4326 for WGS84 (GPS standard)
FMCH_eating_sf <- st_as_sf(FMCH_eating, coords = c("x_", "y_"), crs = 4326)

# Transform to study's CRS (Canada Albers)
target_crs <- st_crs(FMCH_range)
FMCH_eating_sf <- st_transform(FMCH_eating_sf, target_crs)

# Extract coordinates back to columns for ggplot
FMCH_eating_anim <- FMCH_eating_sf %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

anim <- ggplot(FMCH_eating_anim, aes(x = x, y = y, color = id, group = id)) +
  # Add your range boundary as a background
  geom_sf(data = FMCH_range, inherit.aes = FALSE, fill = "grey90", color = "white") +
  
  # Caribou points
  geom_point(size = 3) +
  
  # This adds the "trail" behind the caribou
  shadow_wake(wake_length = 0.1, alpha = FALSE) +
  
  # Updated to use your 'julian' column
  # frame_time will now display the Julian day
  transition_time(julian) +
  
  # Added rounding to the title so the Julian day shows as a whole number
  labs(title = 'Fortymile Caribou Movement: Julian Day {round(frame_time, 0)}',
       x = 'Easting', y = 'Northing',
       color = "Caribou ID") +
  theme_minimal()

# Render the animation
# Note: Increasing nframes to match your Julian range (e.g., 365) 
# makes the movement look much smoother.
animate(anim, nframes = 300, fps = 15, width = 800, height = 600, renderer = gifski_renderer())
