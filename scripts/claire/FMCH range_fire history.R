library(sf)
library(dplyr)

# --- 1. LOAD DATA ---
# (Assuming files are in your setwd() or providing full paths)
ak_fire <- st_read("C:/Users/caeth/Documents/Data/Alaska_fire_history.shp")
yt_fire <- st_read("C:/Users/caeth/Documents/Data/Fire_History.shp")
FMCH_range <- st_read("Fortymile_Caribou_Expanded_Range_2013-2014_Final.shp")

# --- 2. STANDARDIZE COLUMNS ---

# Alaska usually uses FIREYEAR and FIRENAME
ak_clean <- ak_fire %>%
  # rename_with handles case sensitivity (e.g., FireYear vs FIREYEAR)
  select(Year = matches("FIREYEAR|FIRE_YEAR"), 
         Name = matches("FIRENAME|FIRE_NAME")) %>%
  st_transform(st_crs(FMCH_range))

# Yukon often uses FIRE_YEAR but might use FIRENAME without an underscore
yt_clean <- yt_fire %>%
  select(Year = matches("FIREYEAR|FIRE_YEAR"), 
         Name = matches("FIRENAME|FIRE_NAME")) %>%
  st_transform(st_crs(FMCH_range))

# --- 3. MERGE AND CLIP ---

# Combine now that columns are identical
fire_combined <- rbind(ak_clean, yt_clean)

# Fix geometry errors (essential for st_intersection)
fire_combined <- st_make_valid(fire_combined)

message("Clipping fire history to FMCH range...")
# Note: intersection can be slow with large fire datasets
fmch_fire_history <- st_intersection(fire_combined, FMCH_range)

# --- 4. SAVE ---
st_write(fmch_fire_history, "FMCH_Fire_History_Clipped.shp", delete_dsn = TRUE)

# --- 5. VISUAL CHECK ---
# Plotting range boundary first
plot(st_geometry(FMCH_range), border = "black", lwd = 2, main = "FMCH Fire History")
# Adding fire perimeters in red
plot(st_geometry(fmch_fire_history), col = "red", border = "darkred", add = TRUE)
