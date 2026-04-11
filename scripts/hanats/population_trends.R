library(ggplot2)

# Create dataframe
df <- data.frame(
  Year = c(2013, 2017, 2025),
  Mean = c(197228, 218457, 143135),
  CI_low = c(168667, 202106, 122974),
  CI_high = c(225789, 234808, 173087),
  Herd = "Porcupine")

df2 <- data.frame(
  Year = c(2017, 2022, 2023, 2024),
  Mean = c(83659, 37971, 34727,  29351),
  CI_low = c(78138, 36452, 33156, 28714),
  CI_high = c(89180,39490, 36299, 29988),
  Herd = "Fortymile")

dt = rbind(df, df2)

# Plot
png("D:/_Presentations/fig0.png", height = 6, width = 6, units = "in", res = 300)
ggplot(dt, aes(x = Year, y = Mean, color = Herd)) +
  scale_color_manual(values = c("Blue", "Black"))+
  # facet_wrap(~Species, scales = "fixed")+
  geom_point(size = 6) +
  geom_line()+
  #geom_hline(yintercept = 115000, color = "orange", linetype = "dashed", linewidth = 1.3)+
  #geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.5) +
  scale_x_continuous(breaks = seq(2012, 2025, by = 2))+
  labs(x = "Year",y = "Population",) +
  theme_classic()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14),
        legend.position = "none")
dev.off()
