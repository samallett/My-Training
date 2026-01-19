### Simulate FEV1 difference in means with added SEM distribution curve and 2-sided critical region
library(tidyverse)
library(gganimate)

# ----- Parameters -----
n <- 250
sd_val <- 370
alpha <- 0.05
nsim <- 500

set.seed(123)

# Theoretical SD and critical value
sd_diff <- sqrt(2 * sd_val^2 / n)
cv <- qnorm(1 - alpha/2, mean = 0, sd = sd_diff)

# Simulated point estimates (under H0)
results <- tibble(
  sim = 1:nsim,
  diff = rnorm(nsim, mean = 0, sd = sd_diff),
  # color = if_else(diff > cv | diff < -cv , "reject", "accept")
  color = if_else(diff > cv, "reject", "accept")
)

# Density curve + shading
x_vals <- seq(-4 * sd_diff, 4 * sd_diff, length.out = 2000)

df_plot <- tibble(
  x = x_vals,
  density = dnorm(x_vals, mean = 0, sd = sd_diff),
  reject1 = x_vals >= cv,
  reject2 = x_vals <= -cv
)

# ----- Animated plot -----
p_anim <- ggplot() +
  # theoretical density curve
  # geom_line(
  #   data = df_plot,
  #   aes(x = x, y = density),
  #   linewidth = 0.1,
  #   color = "black"
  # ) +
  # shaded rejection region
  # geom_area(
  #   data = df_plot %>% filter(reject1),
  #   aes(x = x, y = density),
  #   fill = "#dd7e0e",
  #   alpha = 0.1
  # ) +
  # geom_area(
  #   data = df_plot %>% filter(reject2),
  #   aes(x = x, y = density),
  #   fill = "#dd7e0e",
  #   alpha = 0.1
  # ) +
  annotate(
    "rect",
    xmin = 65,
    xmax = 130,
    ymin = 0,
    ymax = 2,
    fill = "#dd7e0e",
    alpha = 0.1
  ) +
  # annotate(
  #   "rect",
  #   xmin = -130,
  #   xmax = -65,
  #   ymin = 0,
  #   ymax = 2,
  #   fill = "#dd7e0e",
  #   alpha = 0.1
  # ) + 
  geom_point(data = results, aes(x = diff, y = 1, fill = color, color = color),  size = 8, shape = 21) +  
  
  scale_color_manual(
    values = c("accept" = "#41ab5d", "reject" = "#e31a1c")
  ) +
  scale_fill_manual(
    values = c("accept" = "#FCF7E4", "reject" = "#e31a1c")
  ) +
  scale_x_continuous(" ",breaks=seq(-100, 100, by=100), limits = c(-130, 130)) +
  scale_y_continuous(" ", limits = c(0, 2)) +  # set x-axis range
  geom_vline(xintercept = cv, linetype = "dashed", color = "#ad640b", linewidth = 0.1) +
  # geom_vline(xintercept = -cv, linetype = "dashed", color = "#ad640b", linewidth = 0.1) +
  geom_hline(yintercept = 0, color = "#595959", size = 0.3) +
  theme_minimal(base_size = 24) +
  theme(
    axis.text.y = element_blank(),    # remove y-axis tick labels
    axis.ticks.y = element_blank(),    # remove y-axis ticks
    plot.background = element_rect(fill = "#FCF7E4", color = NA),   # background color
    panel.background = element_rect(fill = "#FCF7E4", color = NA),
    legend.position = "none") +
  transition_states(sim, transition_length = 0, state_length = 1) +
  ease_aes("linear")

anim_out <- animate(p_anim, nframes=100, fps=5, width=700, height=500, duration = 75, renderer = gifski_renderer(loop = TRUE))
anim_save("fev_sim04.gif", anim_out)
