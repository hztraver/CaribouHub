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
