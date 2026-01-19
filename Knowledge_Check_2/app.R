library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Hypothesis Testing by Simulation (Perfect Squares, 2-sided)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample size per group (n):",
                  min = 20, max = 1000, value = 250, step = 10),
      sliderInput("sd_val", "Population SD:",
                  min = 50, max = 1000, value = 370, step = 10),
      sliderInput("true_diff", "True treatment difference:",
                  min = -200, max = 300, value = 100, step = 10),
      
      br(),
      textOutput("power_text"),
      textOutput("calc_power_text"),
      br(),
      textOutput("cv_text"),
      
      helpText("Green = reject H0, Red = accept H0")
    ),
    
    mainPanel(
      plotOutput("plot", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  
  # Automatically generate 1000 simulations whenever inputs change
  sims <- reactive({
    alpha <- 0.05
    sd_diff <- sqrt(2 * input$sd_val^2 / input$n)
    
    # 2-sided critical values
    cv <- qnorm(1 - alpha/2, mean = 0, sd = sd_diff)
    
    # Decision function for 2-sided test
    decision_fun <- function(diff) ifelse(diff > cv | diff < -cv, "reject", "accept")
    
    # Generate 1000 simulations
    n_sim <- 1000
    diffs <- rnorm(n_sim, mean = input$true_diff, sd = sd_diff)
    tibble(
      sim = 1:n_sim,
      diff = diffs,
      decision = decision_fun(diffs)
    )
  })
  
  # Simulated power
  output$power_text <- renderText({
    df <- sims()
    sim_power <- mean(df$decision == "reject")
    paste0("Simulated power: ", round(sim_power,3))
  })
  
  # Calculated theoretical power
  output$calc_power_text <- renderText({
    alpha <- 0.05
    sd_diff <- sqrt(2 * input$sd_val^2 / input$n)
    
    cv <- qnorm(1 - alpha/2, mean = 0, sd = sd_diff)
    calc_power <- pnorm(-cv, mean = input$true_diff, sd = sd_diff) +
      (1 - pnorm(cv, mean = input$true_diff, sd = sd_diff))
    paste0("Calculated theoretical power: ", round(calc_power,3))
  })
  
  # Show critical values in left panel
  output$cv_text <- renderText({
    alpha <- 0.05
    sd_diff <- sqrt(2 * input$sd_val^2 / input$n)
    cv <- qnorm(1 - alpha/2, mean = 0, sd = sd_diff)
    paste0("Critical values: ±", round(cv, 2))
  })
  
  # Plot
  output$plot <- renderPlot({
    df <- sims()
    
    alpha <- 0.05
    sd_diff <- sqrt(2 * input$sd_val^2 / input$n)
    
    # Always 2-sided
    cv <- qnorm(1 - alpha/2, mean = 0, sd = sd_diff)
    cv_low <- -cv
    
    # Axis limits
    margin <- 5 * sd_diff
    x_min <- min(-margin, input$true_diff - margin)
    x_max <- max(margin, input$true_diff + margin)
    
    # Adaptive bin width
    bin_width <- sd_diff / 5
    df <- df %>%
      mutate(diff_bin = round(diff / bin_width) * bin_width) %>%
      group_by(diff_bin) %>%
      mutate(y_index = row_number()) %>%
      ungroup()
    
    # Prepare squares
    square_list <- df %>%
      mutate(
        ymin = y_index - 1,
        ymax = y_index
      )
    
    # Calculate square width for perfect squares
    y_range <- max(max(square_list$ymax, na.rm = TRUE), 20)
    x_range <- x_max - x_min
    square_width <- (1 * x_range) / y_range
    square_list <- square_list %>%
      mutate(
        xmin = diff_bin - square_width/2,
        xmax = diff_bin + square_width/2
      )
    
    # Plot
    p <- ggplot() +
      # Rejection regions
      geom_rect(aes(xmin=cv, xmax=x_max, ymin=0, ymax=y_range),
                fill="#dd7e0e", alpha=0.2) +
      geom_rect(aes(xmin=cv_low, xmax=x_min, ymin=0, ymax=y_range),
                fill="#dd7e0e", alpha=0.2) +
      
      # Null line
      geom_vline(xintercept=0, linetype="dashed", color="#595959") +
      
      # Draw squares
      geom_rect(data = square_list,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = decision)) +
      scale_fill_manual(values = c("accept" = "#e31a1c", "reject" = "#41ab5d")) +
      
      coord_fixed(ratio = 1, xlim = c(x_min, x_max), ylim = c(0, y_range + 1)) +
      
      theme_minimal(base_size = 16) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "#FCF7E4", color = NA),
        plot.background = element_rect(fill = "#FCF7E4", color = NA)
      ) +
      xlab("Estimated treatment effect")
    
    print(p)
  })
}

shinyApp(ui=ui, server=server)
