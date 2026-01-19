library(tidyverse)
library(gganimate)

# parameters
N <- 12
K <- 6
n <- 6
nsim <- 150

# exact PMF
pmf_df <- tibble(
  successes = 0:n,
  probability = dhyper(successes, m = K, n = N - K, k = n)
)

# simulate
set.seed(456)
sim_df <- tibble(
  draw = 1:nsim,
  x = rhyper(nsim, m = K, n = N - K, k = n),
  y = 0
) %>%
  mutate(col = if_else(x < 5, "blue", "red"))

# animation
p_anim <- ggplot() +
 
  # All bars with default outline
  geom_col(
    data = pmf_df %>% filter(successes < 5),
    aes(x = factor(successes), y = probability),
    fill = "#CCD8E6",
    color = "#4E75A3",
    linewidth = 0.1,
    width = 0.6
  ) +
  
  # Last bar with red outline
  geom_col(
    data = pmf_df %>% filter(successes >= 5),
    aes(x = factor(successes), y = probability),
    fill = "#CCD8E6",
    color = "#e31a1c",
    linewidth = 0.4,
    width = 0.6
  ) +
  
  # geom_col(
  #   data = pmf_df,
  #   aes(x = factor(successes), y = probability),
  #   fill = "#CCD8E6",
  #   color = "#4E75A3",
  #   linewidth = 0.1,
  #   width = 0.6
  # ) +
  
  scale_y_continuous(expand = c(0, 0)) +  
  
  geom_point(
    data = sim_df,
    aes(x = factor(x), y = 0.01, group = draw, color = col),
    size = 5
  ) +
  annotate(
    "rect",
    xmin = 5.5,
    xmax = 7.5,
    ymin = 0,
    ymax = max(pmf_df$probability) * 1.05,
    fill = "#e31a1c",
    alpha = 0.08
  ) +
  scale_color_identity() +
  transition_states(
    draw,
    transition_length = 0,
    state_length = 0.4     # each dot stays for 0.4 seconds
  ) +
  enter_fade() +
  exit_fade() +
  ease_aes("linear") +
 
  labs(
    title = " ",
    x = " ",
    y = " "
  ) +
  theme_minimal(base_size = 24) +
  theme(
    panel.background = element_rect(fill = "#FCF7E4", color = NA),
    plot.background  = element_rect(fill = "#FCF7E4", color = NA),
    panel.grid       = element_line(color = "#DAE3EE"),
    axis.line.y        = element_line(color = "black"),
    axis.ticks       = element_line(color = "black")
  )

anim_out <- animate(p_anim, nframes=100, fps=10, width=700, height=500, duration = 75, renderer = gifski_renderer(loop = TRUE))
anim_save("tea_test4.gif", anim_out)

