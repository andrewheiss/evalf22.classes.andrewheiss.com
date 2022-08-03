library(tidyverse)
library(scales)

# Download from https://faculty.chicagobooth.edu/george.wu/research/marathon/data.htm
marathons_raw <- read_csv("~/Downloads/master_marathon.csv")

marathons <- marathons_raw %>% 
  select(chiptime) %>% 
  # Only those under 7 hours
  filter(chiptime < (7 * 60)) %>% 
  # Only use 10% of the data for tinkering with the plot
  # sample_frac(0.1) %>% 
  # Convert to seconds so that time_format() works correctly
  mutate(chiptime = chiptime * 60)

half_hours <- tibble(line = seq(2, 7, 0.5) * 60)

ggplot(marathons, aes(x = chiptime)) +
  geom_histogram(binwidth = 1 * 60, boundary = 0, fill = "#2ECC40") +
  geom_vline(data = half_hours, aes(xintercept = line * 60), color = "white") +
  scale_x_time(labels = time_format("%H:%M"),
               breaks = half_hours$line * 60) +
  scale_y_continuous(labels = comma, expand = c(0, 0)) +
  labs(x = "Finish time (each bar is one minute)", y = "Number of finishers",
       title = "Distribution of marathon finishing times",
       subtitle = paste0("N = ", comma(nrow(marathons))),
       caption = "Eric J. Allen, Patricia M. Dechow, Devin G. Pope, George Wu (2017)\nReference-Dependent Preferences: Evidence from Marathon Runners.\nManagement Science 63(6):1657-1672. https://doi.org/10.1287/mnsc.2015.2417") +
  theme_classic(base_size = 28, base_family = "Fira Sans Condensed") +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = rel(0.4)))

ggsave(here::here("img", "10", "marathons.png"), 
       width = 40/3, height = 7.5, units = "in", type = "cairo", dpi = 150)
