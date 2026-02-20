library(sf)
library(ggplot2)
library(gganimate)
library(dplyr)
library(lubridate)
library(viridis)


# Load data
FMCH_eating <- read.csv("C:/Users/caeth/Documents/Data/Video Collar Data/dc_JANEdata_eating_modlocs.csv")
FMCH_range <- st_read("C:/Users/caeth/Documents/Data/FMCH range shapefiles (from Mike Suitor)/Fortymile_Caribou_Expanded_Range_2013-2014_Final/Fortymile_Caribou_Expanded_Range_2013-2014_Final.shp")



# --- STEP 1: SPATIAL ASSIGNMENT & PROJECTION ---
# Start with NAD83 (4269) for the raw decimal degrees
FMCH_pts_sf <- st_as_sf(FMCH_eating, coords = c("x_", "y_"), crs = 4269)

# Project both to Alaska Albers (3338) for analysis
FMCH_range_3338 <- st_transform(FMCH_range, 3338)
FMCH_pts_3338   <- st_transform(FMCH_pts_sf, 3338)

# --- STEP 2: FIX THE 275-MILE DISPARITY ---
range_center  <- st_centroid(st_union(FMCH_range_3338))
points_center <- st_centroid(st_union(FMCH_pts_3338))

shift_x <- st_coordinates(range_center)[1] - st_coordinates(points_center)[1]
shift_y <- st_coordinates(range_center)[2] - st_coordinates(points_center)[2]

# Apply the shift to geometry only
FMCH_pts_corrected <- FMCH_pts_3338
st_geometry(FMCH_pts_corrected) <- st_geometry(FMCH_pts_3338) + c(shift_x, shift_y)
st_crs(FMCH_pts_corrected) <- 3338 

# --- STEP 3: PREP FOR ANIMATION ---
FMCH_ready <- FMCH_pts_corrected %>%
  mutate(
    year = year(t_),
    date_only = as.Date(t_), # Create a Date-only column for smoother transitions
    easting = st_coordinates(.)[,1],
    northing = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry()

# --- STEP 4: ANIMATION FUNCTION ---
animate_fmch <- function(target_year) {
  
  plot_data <- FMCH_ready %>% filter(year == target_year)
  
  anim <- ggplot() +
    # Background Range
    geom_sf(data = FMCH_range_3338, fill = "gray95", color = "gray80") +
    
    # Points colored by Elevation
    geom_point(data = plot_data, 
               aes(x = easting, y = northing, color = elev, group = id), 
               size = 2.5, alpha = 0.8) +
    
    scale_color_viridis_c(option = "mako", direction = -1, name = "Elevation (m)") +
    
    shadow_wake(wake_length = 0.1, alpha = FALSE) +
    
    # Use the actual date for the transition
    transition_time(date_only) +
    
    coord_sf(datum = st_crs(4326)) +
    
    # Format the title to show Day, Month, Year
    labs(title = paste(target_year, 'Fortymile Movement: {format(frame_time, "%d %b %Y")}'),
         subtitle = 'Axes: Lat/Long Degrees | Color: Elevation (m)',
         x = 'Longitude (°W)', 
         y = 'Latitude (°N)') +
    theme_minimal()
  
  animate(anim, nframes = 300, fps = 15, width = 800, height = 600, renderer = gifski_renderer())
}

# --- STEP 5: RENDER ---
anim_2018 <- animate_fmch(2018)
anim_2019 <- animate_fmch(2019)

anim_2018
anim_2019