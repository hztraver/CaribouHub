library(ggplot2)
library(tidyr)
library(forcats)

#### FIGURES SHOWING PFT CHANGE FOR ALASKA/YUKON HERDS ####
dt = read.csv("D:/CaribouHub/data/all_herds_PFT_change.csv")

# set colors for plot
pft_colors = c("#BCEE68", "#008B00", "#FF8C00", "#8B4513", "#104E8B", "#00B2EE", "#FFC125")

total = dt %>% 
  pivot_longer(., cols = starts_with("percent_cover"), names_to = "Year", values_to = "Percent_cover") %>%
  mutate(Year = as.numeric(gsub("percent_cover_", "", Year))) %>%
  mutate(range = factor(range)) %>% 
  mutate(range = fct_recode(range, "Summer" = "summer", "Winter" = "winter", "Total" = 'total'))

## Plot Porcupine caribou summer and winter range 
total %>% filter(., herd == "Porcupine") %>% filter(range != "Total") %>%
  ggplot(., aes(x = Year, y = Percent_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  facet_wrap(~range)+
  xlab("Year")+
  ylab("Percent cover")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

## Summer & Winter ranges 
total %>% filter(herd != "Fourtymile") %>% filter(range != "Total") %>%
  ggplot(., aes(x = Year, y = Percent_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  facet_wrap(~range + herd)+
  xlab("Year")+
  ylab("Percent cover")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

## Total range including Fourtymile
total %>% filter(range == "Total") %>%
  ggplot(., aes(x = Year, y = Percent_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  facet_wrap(~herd)+
  xlab("Year")+
  ylab("Percent cover")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

#### Relative cover 
## drop PFTs where cover in 1985 < 1.1 %
relative = dt %>% filter(percent_cover_1985 > 1.1) %>% 
  mutate(across(percent_cover_1985:percent_cover_2020, ~ .x/percent_cover_1985)) %>%
  pivot_longer(., cols = starts_with("percent_cover"), names_to = "Year", values_to = "Relative_cover") %>%
  mutate(Year = as.numeric(gsub("percent_cover_", "", Year))) %>%
  mutate(range = factor(range)) %>% 
  mutate(range = fct_recode(range, "Summer" = "summer", "Winter" = "winter", "Total" = 'total'))

## Plot Porcupine caribou summer and winter range 
relative %>% filter(., herd == "Porcupine") %>% filter(range != "Total") %>%
  ggplot(., aes(x = Year, y = Relative_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~range)+
  xlab("Year")+
  ylab("Cover relative to 1985")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

## Summer & Winter ranges 
relative %>% filter(herd != "Fourtymile") %>% filter(range != "Total") %>%
  ggplot(., aes(x = Year, y = Relative_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_line(alpha = 0.4)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~range + herd)+
  xlab("Year")+
  ylab("Cover relative to 1985")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))

## Total range including Fourtymile
relative %>% filter(range == "Total") %>%
  ggplot(., aes(x = Year, y = Relative_cover, colour = PFT))+
  geom_point(size = 2.5)+
  scale_color_manual(values = pft_colors)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_line(alpha = 0.4)+
  facet_wrap(~herd)+
  xlab("Year")+
  ylab("Cover relative to 1985")+
  theme_bw()+
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14),
        strip.text = element_text(size = 12))