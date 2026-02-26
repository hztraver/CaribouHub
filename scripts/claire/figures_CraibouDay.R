library(ggplot2)
library(dplyr)
library(lubridate)

setwd("~/Data/Video Collar Data")

FMCH_eating <- read.csv("dc_JANEdata_eating_modlocs.csv")
FMCH_diet <- read.csv("GPSCC_diet_FCH.csv")

# 1. Calculate the monthly mean and standard deviation
FMCH_monthly_summary <- FMCH_eating %>%
  mutate(
    date_posix = as.POSIXct(t_, format = "%Y-%m-%d %H:%M:%S"),
    year = year(date_posix),
    # Convert numeric month to full name factor
    month_name = month(month2, label = TRUE, abbr = FALSE)
  ) %>%
  group_by(year, month_name) %>%
  summarize(
    mean_shrub = mean(allDecShrub, na.rm = TRUE),
    sd_shrub = sd(allDecShrub, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Plot the summary
ggplot(FMCH_monthly_summary, aes(x = month_name, y = mean_shrub, group = year)) +
  geom_errorbar(aes(ymin = mean_shrub - sd_shrub, ymax = mean_shrub + sd_shrub), 
                width = 0.2, color = "darkgray") +
  geom_point(size = 3, color = "darkgreen") +
  geom_line(color = "darkgreen", linewidth = 1) +
  facet_wrap(~year) +
  labs(
    title = "Monthly Mean Deciduous Shrub Cover",
    subtitle = "Points represent the mean; bars represent ±1 Standard Deviation",
    x = "Month",
    y = "Mean Shrub Cover Proportion",
    caption = "Data: FMCH Video Collar Analysis"
  ) +
  theme_classic() # Removes grids and provides a clean look


library(ggplot2)
library(dplyr)
library(scales) # for percentage formatting

library(ggplot2)
library(dplyr)
library(scales)

library(ggplot2)
library(dplyr)
library(scales)

# 1. Load the dataset
fmch_diet <- read.csv("GPSCC_diet_FCH.csv")

# 2. Regroup PFTs and Prepare Data
diet_summary <- fmch_diet %>%
  filter(Year %in% c(2018, 2019),
         Month %in% 5:8) %>%
  mutate(
    # Regroup specific PFTs into "Herbaceous"
    # Note: Includes common casing variations to ensure all are captured
    PFT.grouped = case_when(
      PFT.final %in% c("Equisetum", "Forb", "Graminoid", "graminoid", 
                       "Moss", "moss", "Ground-level veg", "Ground-level vegetation",
                       "Mushroom", "mushroom") ~ "Herbaceous",
      TRUE ~ PFT.final
    ),
    # Maintain the chronological order for abbreviations found in your CSV
    Month_Name = factor(MonthAbb, levels = c("May", "Jun", "Jul", "Aug"))
  ) %>%
  # Group by the NEW PFT.grouped column
  group_by(Year, Month_Name, PFT.grouped) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# 3. Create the two-panel plot
ggplot(diet_summary, aes(x = Month_Name, y = proportion, fill = PFT.grouped)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  
  # Split by Year
  facet_wrap(~Year, ncol = 2) +
  
  # Formatting
  labs(
    title = "Proportional Summer Diet",
    subtitle = "Herbaceous: Equisetum, Forb, Graminoid, Moss, Ground-level veg, Mushroom",
    x = "Month",
    y = "Proportion of Diet",
    fill = "Vegetation Type",
    caption = "Data: FMCH Video Collar Analysis"
  ) +
  
  # Standardize scales and labels
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("May" = "May", "Jun" = "June", "Jul" = "July", "Aug" = "August")) +
  
  # Aesthetics
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid = element_blank()
  )


library(ggplot2)
library(dplyr)
library(scales)

# 1. Load the activity dataset
fmch_activity <- read.csv("GPSVCC_activityFCH.csv")

# 2. Prepare the data
activity_summary <- fmch_activity %>%
  # Filter for the specific summer window and years
  filter(Year %in% c(2018, 2019),
         Month %in% 5:8) %>%
  mutate(
    # Map numeric months to full names and ensure they are in chronological order
    Month_Name = factor(Month, 
                        levels = 5:8, 
                        labels = c("May", "June", "July", "August"))
  ) %>%
  # Group by Year, Month, and Activity to calculate proportions
  group_by(Year, Month_Name, Activity) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# 3. Create the two-panel activity plot
ggplot(activity_summary, aes(x = Month_Name, y = proportion, fill = Activity)) +
  # Stacked bar plot showing relative time budget
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  
  # Separate by Year
  facet_wrap(~Year, ncol = 2) +
  
  # Formatting
  labs(
    title = "Caribou Activity (May - August)",
    subtitle = "Proportion of time spent in different behavioral states",
    x = "Month",
    y = "Proportion of Activity",
    fill = "Activity Type",
    caption = "Data: FMCH Video Collar Analysis"
  ) +
  
  # Professional styling
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# 4. Optional: Save the figure
ggsave("FMCH_Activity_Proportion_18_19.png", width = 10, height = 6, dpi = 300)






# --- 1. Load Data ---
setwd("~/Data/Video Collar Data")
eating_df <- read.csv("dc_JANEdata_eating_modlocs.csv")
diet_df   <- read.csv("GPSCC_diet_FCH.csv")

# --- 2. Data Preparation ---

# Prep Diet Data
diet_summary <- diet_df %>%
  filter(Year %in% c(2018, 2019), Month %in% 5:8) %>%
  mutate(
    PFT.grouped = case_when(
      PFT.final %in% c("Equisetum", "Forb", "Graminoid", "graminoid", 
                       "Moss", "moss", "Ground-level veg", "Ground-level vegetation",
                       "Mushroom", "mushroom") ~ "Herbaceous",
      TRUE ~ PFT.final
    ),
    Month_Name = factor(Month, levels = 5:8, labels = c("May", "Jun", "Jul", "Aug"))
  ) %>%
  group_by(Year, Month_Name, PFT.grouped) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# Prep Elevation and Shrub Data (from the 'eating' dataset)
eating_summary <- eating_df %>%
  mutate(
    date_posix = as.POSIXct(t_, format = "%Y-%m-%d %H:%M:%S"),
    Year = year(date_posix),
    Month_Name = month(date_posix, label = TRUE, abbr = TRUE)
  ) %>%
  filter(Year %in% c(2018, 2019), month(date_posix) %in% 5:8) %>%
  group_by(Year, Month_Name) %>%
  summarize(
    mean_shrub = mean(allDecShrub, na.rm = TRUE),
    sd_shrub = sd(allDecShrub, na.rm = TRUE),
    mean_ele = mean(elev, na.rm = TRUE), # Change 'ele' to your actual elevation column name
    sd_ele = sd(elev, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. Plotting Function ---
create_year_bundle <- function(target_year) {
  
  # A. Diet Plot
  p1 <- ggplot(filter(diet_summary, Year == target_year), 
               aes(x = Month_Name, y = proportion, fill = PFT.grouped)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste(target_year, "Diet"), x = "", y = "Diet %", fill = "Type") +
    theme_classic() + 
    theme(legend.position = "right", legend.text = element_text(size = 7))
  
  # B. Elevation Plot
  p2 <- ggplot(filter(eating_summary, Year == target_year), 
               aes(x = Month_Name, y = mean_ele, group = 1)) +
    geom_line(color = "darkblue") +
    geom_point() +
    geom_errorbar(aes(ymin = mean_ele - sd_ele, ymax = mean_ele + sd_ele), width = 0.1) +
    labs(title = "Elevation", x = "", y = "Elevation (m)") +
    theme_classic()
  
  # C. Shrub Cover Plot
  p3 <- ggplot(filter(eating_summary, Year == target_year), 
               aes(x = Month_Name, y = mean_shrub, group = 1)) +
    geom_line(color = "darkgreen", linewidth = 1) +
    geom_point(size = 2, color = "darkgreen") +
    geom_errorbar(aes(ymin = mean_shrub - sd_shrub, ymax = mean_shrub + sd_shrub), 
                  width = 0.1, color = "darkgray") +
    labs(title = "Shrub Cover", x = "Month", y = "Shrub Prop.") +
    theme_classic() 
  
  # CHANGE: Using | to combine Diet, Elevation, and Shrub horizontally
  return(p1 | p2 | p3)
}

# --- Final Output ---

# Generate the horizontal rows for each year
row_2018 <- create_year_bundle(2018)
row_2019 <- create_year_bundle(2019)

# CHANGE: Using / to stack the 2018 row on top of the 2019 row
final_plot <- row_2018 / row_2019

# Apply shared formatting and display
final_plot + 
  plot_layout(guides = "collect") + # Combines legends to save space
  plot_annotation(title = "Fortymile Caribou Herd: 2018 vs 2019 Comparison",
                  subtitle = "Diet, Elevation, and Shrub Cover Trends")