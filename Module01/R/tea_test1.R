### Plot the pmf for hypergeometric dsitribution (tea test)
library(tidyverse)

# parameters
N <- 8
K <- 4
n <- 4

# theoretical PMF (exact) using dhyper
pmf_df <- tibble(
  successes = 0:n,
  probability = dhyper(successes, m = K, n = N - K, k = n)
)

p <- ggplot(data = pmf_df, aes(x = factor(successes), y = probability)) +
  
  # All bars with default outline
  geom_col(
    data = pmf_df, 
    aes(x = factor(successes), y = probability),
    fill = "#CCD8E6",
    color = "#4E75A3",
    linewidth = 0.1,
    width = 0.6
  ) +
  # annotate(
  #   "rect",
  #   xmin = 5.5,
  #   xmax = 7.5,
  #   ymin = 0,
  #   ymax = max(pmf_df$probability) * 1.05,
  #   fill = "#dd7e0e",
  #   alpha = 0.1
  # ) +
  
   scale_y_continuous(expand = c(0, 0)) +  
  
  labs(
    title = " ",
    x = " ",
    y = " "
  ) +
  theme_minimal(base_size = 48) +
  theme(
    panel.background = element_rect(fill = "#FCF7E4", color = NA),
    plot.background  = element_rect(fill = "#FCF7E4", color = NA),
    panel.grid       = element_line(color = "#DAE3EE"),
    axis.line.y        = element_line(color = "black"),
    axis.ticks       = element_line(color = "black")
  )

ggsave(p, filename = "tea_test1.png", width = 18, height = 16)