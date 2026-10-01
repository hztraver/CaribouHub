library(terra)
library(tidyverse)

raster_files <- list.files(path = "C:/Data/Drone data/Classified Orthos", 
                           pattern = "\\.tif$", 
                           full.names = TRUE)

# Build the metadata table
file_inventory <- tibble(raster_path = raster_files) %>%
  mutate(
    filename = basename(raster_path),
    
    # Extract the 4-digit year
    year = str_extract(filename, "\\d{4}"),
    
    # Remove "classified_" from the start AND "_2019.tif" from the end all at once
    site_name = str_remove_all(filename, "^classified_|_\\d{4}\\.tif$"),
    
    # Assign T1 or T2 based on the year
    time_period = if_else(year %in% c("2018", "2019"), "T1", "T2")
  )

# Look at the first few rows to ensure it extracted everything correctly
head(file_inventory)

# Function to calculate percent cover from a single raster
get_percent_cover <- function(raster_path, site_name, time_period) {
  
  r <- rast(raster_path)
  
  class_counts <- freq(r, usena = FALSE) %>%
    as_tibble() %>%
    # Bulletproof filter: drops any row containing the word "shadow" (ignoring case)
    filter(!str_detect(value, "(?i)shadow")) 
  
  total_pixels <- sum(class_counts$count)
  
  result <- class_counts %>%
    mutate(
      percent_cover = (count / total_pixels) * 100,
      site = site_name,
      time = time_period
    ) %>%
    select(site, time, pft_class = value, percent_cover)
  
  return(result)
}

# Run the loop and combine results
all_cover_data <- file_inventory %>%
  select(raster_path, site_name, time_period) %>%
  pmap_dfr(get_percent_cover)

# Calculate change
cover_change <- all_cover_data %>%
  rename(pft_name = pft_class) %>% 
  group_by(site, time, pft_name) %>%
  summarise(percent_cover = sum(percent_cover), .groups = "drop") %>%
  pivot_wider(
    names_from = time, 
    values_from = percent_cover,
    values_fill = list(percent_cover = 0) 
  ) %>%
  mutate(change_percent = T2 - T1)

cover_change

#test changes