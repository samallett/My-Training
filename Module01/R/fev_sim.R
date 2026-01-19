### Simulate FEV1 difference in means with added SEM distribution curve

library(tidyverse)
library(gganimate)

set.seed(123)

# ---- Simulation parameters ----
n_trials <- 20
n_per_arm <- 250
placebo_mean <- 0
active_mean <- 100
sd_value <- 370

# ---- Simulate trials ----
sim_trials <- map_df(1:n_trials, function(t) {
  tibble(
    trial = t,
    arm = rep(c("Placebo", "Active"), each = n_per_arm),
    value = c(
      rnorm(n_per_arm, mean = placebo_mean, sd = sd_value),
      rnorm(n_per_arm, mean = active_mean, sd = sd_value)
    )
  )
})

# ---- Compute trial-level mean differences ----
trial_results <- sim_trials %>%
  group_by(trial, arm) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = arm,
    values_from = c(mean, sd, n)
  ) %>%
  mutate(
    diff = mean_Active - mean_Placebo
  )

# ---- Prepare data for animation ----
trial_results_anim <- trial_results %>%
  arrange(trial) %>%
  mutate(
    trial_factor = factor(trial)
  )

# ---- Fit normal distribution parameters ----
# 
sem = sqrt(2*(sd_value**2)/n_per_arm)
diff_sd <- sd(trial_results$diff)

# ---- Animated plot ----
p_anim <- ggplot(trial_results_anim, aes(x = diff)) +
   # stat_function(fun = dnorm,
   #              args = list(mean = active_mean, sd = sem),
   #              linewidth = 0.1) +
  geom_point(x = 100, y = 2, color = "#DD7E0E", size = 10) +
  geom_point(aes(y = 1), color = "#DD7E0E", size = 10) +
  # geom_hline(yintercept = 0, color = "#595959", size = 0.3) +
  geom_hline(yintercept = 1, color = "#cccccc", size = 0.3, linetype = "dashed") +
  geom_hline(yintercept = 2, color = "#cccccc", size = 0.3, linetype = "dashed") +
  # geom_vline(xintercept = 100,
  #            color = "#DD7E0E",
  #            linetype = "dashed",
  #            linewidth = 1) +
  labs(
    title = " ",
    x = " ",
    y = NULL   # remove y-axis label
  ) +
  scale_x_continuous(limits = c(-50, 250)) +  # set x-axis range
  scale_y_continuous(limits = c(0, 3)) +  # set y-axis range
  theme_minimal(base_size = 24) +
  theme(
    axis.text = element_blank(),    # remove y-axis tick labels
    axis.ticks = element_blank(),    # remove y-axis ticks
    plot.background = element_rect(fill = "#FCF7E4", color = NA),   # background color
    panel.background = element_rect(fill = "#FCF7E4", color = NA)) +
  transition_states(
    trial_factor,
    transition_length = 5,
    state_length = 5
  )

# Render animation (slower)
animate(p_anim, fps = 2, width = 350, height = 250)

anim_out <- animate(p_anim, nframes=100, fps=5, width=700, height=500, renderer = gifski_renderer(loop = TRUE))
anim_save("fev_sim.gif", anim_out)

