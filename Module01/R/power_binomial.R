library(tidyverse)

# Parameters
n <- 20
p_null <- 0.25
p_alt  <- 0.60
alpha  <- 0.025

# Calculate critical value:
# smallest k such that P(X >= k | p = p_null) <= alpha
crit_df <- tibble(k = 0:n) %>%
  mutate(
    tail_prob = map_dbl(
      k,
      ~ pbinom(.x - 1, size = n, prob = p_null, lower.tail = FALSE)
    )
  )

critical_value <- crit_df %>%
  filter(tail_prob <= alpha) %>%
  slice(1) %>%
  pull(k)

critical_value

# Create data for plotting

df <- tibble(
  k = 0:n,
  null = dbinom(k, size = n, prob = p_null),
  alternative = dbinom(k, size = n, prob = p_alt)
) %>%
  pivot_longer(
    cols = c(null, alternative),
    names_to = "hypothesis",
    values_to = "probability"
  )

# Power
power <- sum(dbinom(critical_value:n, size = n, prob = p_alt))

# Produce plot

ggplot(df, aes(x = k, y = probability, fill = hypothesis)) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  # Power region
  geom_col(
    data = df %>%
      filter(hypothesis == "alternative", k >= critical_value),
    aes(x = k, y = probability),
    fill = "grey60",
    alpha = 0.6,
    width = 0.7
  ) +
  
  geom_vline(
    xintercept = critical_value - 0.5,
    linetype = "dotted"
  ) +
  
  labs(
    title = "Exact Binomial Test: Null vs Alternative",
    subtitle = paste0(
      "One-sided α = ", alpha,
      "; Critical value ≥ ", critical_value,
      "; Power ≈ ", round(power, 2)
    ),
    x = "Number of responses out of 25",
    y = "Probability",
    fill = "Hypothesis"
  ) +
  
  theme_minimal(base_size = 13)

