library(tidyverse)

set.seed(1908)  # Gosset published as "Student" in 1908

# Simulate Population 
population <- tibble(
  saccharine = rnorm(
    n = 200000,
    mean = 133.0,  # historically realistic target
    sd   = 1.5     # plausible batch-to-batch variability
  )
)

true_mean <- mean(population$saccharine)

# Guinness Experiment
n        <- 8      # Gosset often worked with n between 4 and 8
n_trials <- 10000

experiments <- tibble(trial = 1:n_trials) %>%
  mutate(
    sample = map(trial, ~ sample(population$saccharine, n))
  ) %>%
  mutate(
    mean = map_dbl(sample, mean),
    sd   = map_dbl(sample, sd),
    se   = sd / sqrt(n),
    
    t_lo = mean - qt(0.975, df = n - 1) * se,
    t_hi = mean + qt(0.975, df = n - 1) * se
  )

# Is precision within +/- 0.5 ?

precision_check <- experiments %>%
  summarise(
    within_half_degree = mean(abs(mean - true_mean) <= 0.5)
  )

interval_precision <- experiments %>%
  summarise(
    interval_width_ok =
      mean(t_lo <= true_mean - 0.5 & t_hi >= true_mean + 0.5)
  )

interval_precision

experiments %>%
  ggplot(aes(x = mean)) +
  geom_histogram(bins = 50, fill = "grey80", colour = "white") +
  geom_vline(xintercept = true_mean, linewidth = 1) +
  geom_vline(xintercept = true_mean + c(-0.5, 0.5),
             linetype = "dashed") +
  labs(
    title = "Sampling Distribution of Mean Degrees Saccharine",
    subtitle = paste(
      "Target =", round(true_mean, 1),
      "| Sample size n =", n,
      "| Precision ±0.5 degrees"
    ),
    x = "Sample mean",
    y = "Frequency"
  ) +
  theme_minimal()



