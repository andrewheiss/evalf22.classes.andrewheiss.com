library(tidyverse)
library(gganimate)
# Single loess window with 0.75 span

fun_linear <- function(x) 10 + 4 * x
fun_loess <- function(x) 100 + 30 * sin(5 * x) + 0.2 * x^2 - 0.002 * x^3

set.seed(1234)
df_params <- tibble(id = 1:400) %>% 
  mutate(x_uniform = runif(n(), min = 0, max = 100)) %>% 
  mutate(y_loess = fun_loess(x_uniform) + rnorm(n(), 0, 50)) %>% 
  mutate_at(vars(starts_with("y_")),
            list(effect = ~ifelse(x_uniform >= 75, . + 200, .)))

data_at_each_x <- df_params %>%
  select(x_uniform, y_loess) %>% 
  # Expand the data so all the variables are repeated for every possible x
  crossing(center = unique(df_params$x_uniform)) %>%
  # Do all these calculations for each possible x
  group_by(center) %>%
  # Find the distance between each x and the current group's x
  mutate(dist = abs(x_uniform - center)) %>% 
  # Scale the distance by the maximum of the distance
  mutate(scaled_distance_full_data = rank(dist) / n()) %>% 
  # Only look at values that are within the span
  filter(scaled_distance_full_data <= 0.25) %>% 
  # Calculate the triple cubic weight
  # See https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd144.htm and
  # https://www.itl.nist.gov/div898/handbook/pmd/section1/dep/dep144.htm
  mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3) %>% 
  # Put each of these groups into a nested list column
  nest() %>% 
  # Run a weighted linear model on each nested data frame and find the predicted
  # value from the model for that x
  mutate(output = map(data, ~lm(y_loess ~ x_uniform, data = ., weight = weight))) %>% 
  mutate(model = map2(output, center, ~augment(.x, newdata = tibble(x_uniform = .y))))

# Unnest the full data
full_data <- data_at_each_x %>% 
  select(center, data) %>% 
  unnest(data)

# Unnest just the predicted values
point_data <- data_at_each_x %>% 
  select(center, model) %>% 
  unnest(model)

# Calculate a single loess fit for the regular data
single_fit <- augment(loess(y_loess ~ x_uniform, 
                            data = df_params, 
                            degree = 1, span = 0.25))

# Plot everything!
plot_loess_window <- ggplot(full_data, aes(x = x_uniform, 
                                           y = y_loess)) +
  # Faded points from original data
  geom_point(data = df_params, 
             size = 5.5, alpha = 0.5, color = "black", fill = "white", pch = 21) +
  # Points with transparency based on loess weight
  geom_point(aes(alpha = weight), size = 5, color = "#FF851B") +
  # Single loess fit
  geom_line(aes(y = .fitted), data = single_fit, size = 2, color = "#B10DC9") +
  # Linear model for each possible x value
  geom_smooth(aes(group = center, weight = weight), color = "#001f3f", size = 1.5,
              formula = y ~ x, method = "lm", se = FALSE) +
  # Predicted point from linear model for each possible x value
  geom_point(aes(y = .fitted, group = x_uniform),
             data = point_data, color = "#FFDC00", size = 3) +
  # Vertical line showing current x value
  geom_vline(aes(xintercept = center), lty = "31", size = 0.5, color = "black") +
  # Formatting stuff
  labs(x = NULL, y = NULL) +
  guides(alpha = FALSE) +
  theme_bw(base_size = 28, base_family = "Fira Sans Condensed") +
  theme(legend.position = "bottom") +
  # gganimate stuff
  transition_manual(center)

# animated_loess_window_gif <- animate(plot_loess_window, 
#                                      width = 1280, height = 720, res = 150)
# anim_save(animated_loess_window_gif, 
#           filename = here::here("img", "10", "loess_window.gif"))

# Can't use anim_save() with videos?
# https://github.com/thomasp85/gganimate/issues/340
animated_loess_window_mp4 <- animate(
  plot_loess_window, 
  width = 1280*2, height = 720*2, res = 150,
  renderer = av_renderer(here::here("img", "10", "loess_window.mp4")))
