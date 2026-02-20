library(ggplot2)
library(dplyr)

# 1. Calculate the monthly mean and standard deviation
FMCH_monthly_summary <- FMCH_eating %>%
  # Ensure we group by year AND month to get distinct summaries
  # Extract year from t_ if you haven't already
  mutate(year = lubridate::year(as.POSIXct(t_, format = "%Y-%m-%d %H:%M:%S"))) %>%
  group_by(year, month2) %>%
  summarize(
    mean_shrub = mean(allDecShrub, na.rm = TRUE),
    sd_shrub = sd(allDecShrub, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Plot the summary
ggplot(FMCH_monthly_summary, aes(x = factor(month2), y = mean_shrub, group = year)) +
  # Add the error bars (Standard Deviation)
  geom_errorbar(aes(ymin = mean_shrub - sd_shrub, ymax = mean_shrub + sd_shrub), 
                width = 0.2, color = "darkgray") +
  
  # Add the mean points
  geom_point(size = 3, color = "darkgreen") +
  
  # Connect the means with a line to show the seasonal trend
  geom_line(color = "darkgreen", size = 1) +
  
  # Separate by Year
  facet_wrap(~year) +
  
  # Formatting
  labs(
    title = "Monthly Mean Deciduous Shrub Cover",
    subtitle = "Points represent the mean; bars represent ±1 Standard Deviation",
    x = "Month",
    y = "Mean Shrub Cover Proportion",
    caption = "Data: FMCH Video Collar Analysis"
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)
library(lubridate)

# 1. Calculate the monthly mean and standard deviation for elevation
FMCH_elev_summary <- FMCH_eating %>%
  mutate(year = year(as.POSIXct(t_, format = "%Y-%m-%d %H:%M:%S"))) %>%
  group_by(year, month2) %>%
  summarize(
    mean_elev = mean(elev, na.rm = TRUE), # Using 'elev' as requested
    sd_elev = sd(elev, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Plot the summary for elevation
ggplot(FMCH_elev_summary, aes(x = factor(month2), y = mean_elev, group = year)) +
  # Add the error bars (Standard Deviation of elevation)
  geom_errorbar(aes(ymin = mean_elev - sd_elev, ymax = mean_elev + sd_elev), 
                width = 0.2, color = "darkgray") +
  
  # Add the mean points (using a different color to distinguish from shrub cover)
  geom_point(size = 3, color = "royalblue") +
  
  # Connect the means with a line
  geom_line(color = "royalblue", size = 1) +
  
  # Separate by Year (Crucial for comparing 2025 field season results)
  facet_wrap(~year) +
  
  # Formatting
  labs(
    title = "Monthly Mean Elevation Use",
    subtitle = "Points represent the mean; bars represent ±1 Standard Deviation",
    x = "Month",
    y = "Mean Elevation (m)",
    caption = "Data: FMCH Video Collar Analysis"
  ) +
  theme_minimal()
