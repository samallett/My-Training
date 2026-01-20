library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Hypothesis Testing by Simulation"),
  
  # CSS for layout
  tags$style(HTML("
    .flex-row { display: flex; align-items: stretch; }
    .flex-col { flex: 1; padding: 10px; min-height: 700px; }
    .plot-col { display: flex; flex-direction: column; min-height: 700px; }
    .plot-col .plot-output { flex: 1; }
  ")),
  
  div(class = "flex-row",
      # Sidebar
      div(class = "flex-col",
          
          # Top greyed panel for SD and alpha
          div(style="background:#f0f0f0; padding:10px; border-radius:5px; margin-bottom:10px;",
              textInput("sd_val", "Population SD:", value = "370"),
              radioButtons("alpha", "Significance level (alpha):",
                           choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                           selected = 0.05)
          ),
          
          sliderInput("n", "Sample size per group (n):",
                      min = 20, max = 1000, value = 250, step = 10),
          
          sliderInput("true_diff", "True treatment difference:",
                      min = -200, max = 300, value = 100, step = 10),
          
          radioButtons(
            "sim_mode",
            "Simulation mode:",
            choices = c("Single simulation", "1000 simulations"),
            selected = "Single simulation"
          ),
          
          actionButton("resimulate", "Resimulate"),
          
          br(),
          htmlOutput("power_text"),
          br(),
          htmlOutput("cv_text"),
          helpText("Green = reject H0, Red = accept H0")
      ),
      
      # Plot area
      div(class = "flex-col plot-col",
          div(class = "plot-output",
              plotOutput("plot", height = "100%")
          )
      )
  )
)

server <- function(input, output, session) {
  
  # Reactive SD from free text input
  sd_diff_reactive <- reactive({
    sd_val <- as.numeric(input$sd_val)
    if (is.na(sd_val) || sd_val <= 0) sd_val <- 370
    sqrt(2 * sd_val^2 / input$n)
  })
  
  # Critical value
  critical_value <- reactive({
    qnorm(1 - as.numeric(input$alpha)/2, mean = 0, sd = sd_diff_reactive())
  })
  
  # Reactive values for simulations
  single_sim <- reactiveVal()
  sims_1000 <- reactiveVal()
  
  # Function to generate simulations
  generate_sim <- reactive({
    sd_diff <- sd_diff_reactive()
    cv <- critical_value()
    
    if (input$sim_mode == "Single simulation") {
      new_diff <- rnorm(1, mean = input$true_diff, sd = sd_diff)
      decision <- ifelse(new_diff > cv | new_diff < -cv, "reject", "accept")
      tibble(diff = new_diff, decision = decision)
    } else {
      n_sim <- 1000
      diffs <- rnorm(n_sim, mean = input$true_diff, sd = sd_diff)
      decision <- ifelse(diffs > cv | diffs < -cv, "reject", "accept")
      tibble(sim = seq_len(n_sim), diff = diffs, decision = decision)
    }
  })
  
  # Reactive for sliders / inputs
  observe({
    # Trigger new simulations whenever sliders or inputs change
    input$n
    input$true_diff
    input$sim_mode
    input$sd_val
    input$alpha
    single_sim(generate_sim())
    sims_1000(generate_sim())
  })
  
  # Resimulate button
  observeEvent(input$resimulate, {
    single_sim(generate_sim())
    sims_1000(generate_sim())
  })
  
  # Reactive: return current simulations
  sims <- reactive({
    if (input$sim_mode == "Single simulation") {
      single_sim()
    } else {
      sims_1000()
    }
  })
  
  # Simulated power as percentage
  output$power_text <- renderUI({
    if (input$sim_mode == "1000 simulations") {
      df <- sims()
      power_val <- round(mean(df$decision == "reject") * 100)
      HTML(paste0('<span style="color:darkblue; font-size:20px;">Simulated power: ', power_val, '%</span>'))
    } else {
      return(NULL)
    }
  })
  
  # Critical value text, dark blue, larger
  output$cv_text <- renderUI({
    cv_val <- round(critical_value(), 2)
    HTML(paste0('<span style="color:darkblue; font-size:18px;">Critical values: ±', cv_val, '</span>'))
  })
  
  # Plot
  output$plot <- renderPlot({
    
    df <- sims()
    sd_diff <- sd_diff_reactive()
    cv <- critical_value()
    cv_low <- -cv
    
    margin <- 5 * sd_diff
    x_min <- min(-margin, input$true_diff - margin)
    x_max <- max(margin, input$true_diff + margin)
    
    # Determine max_y
    if (input$sim_mode == "Single simulation") {
      max_y <- 20
      y_center <- max_y / 2
    } else {
      bin_width <- sd_diff / 5
      df2 <- df %>%
        mutate(diff_bin = round(diff / bin_width) * bin_width) %>%
        group_by(diff_bin) %>%
        mutate(y_index = row_number()) %>%
        ungroup()
      max_y <- max(df2$y_index, 20)
    }
    
    # Rejection text positions
    text_y <- max_y - 0.5
    text_right_x <- (cv + x_max) / 2
    text_left_x <- (x_min + cv_low) / 2
    
    # Base plot
    base_plot <- ggplot() +
      geom_rect(aes(xmin = cv, xmax = x_max, ymin = 0, ymax = max_y),
                fill = "#dd7e0e", alpha = 0.2) +
      geom_rect(aes(xmin = cv_low, xmax = x_min, ymin = 0, ymax = max_y),
                fill = "#dd7e0e", alpha = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#595959") +
      annotate("text", x = text_right_x, y = text_y, 
               label = "Reject the null hypothesis", color = "red", size = 4, hjust = 0.5) +
      annotate("text", x = text_left_x, y = text_y, 
               label = "Reject the null hypothesis", color = "red", size = 4, hjust = 0.5) +
      scale_fill_manual(values = c("accept" = "#e31a1c", "reject" = "#41ab5d")) +
      coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, max_y)) +
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
    
    # Single simulation
    if (input$sim_mode == "Single simulation") {
      return(
        base_plot +
          geom_point(data = df,
                     aes(x = diff, y = y_center, fill = decision),
                     shape = 21, size = 6, color = "black")
      )
    }
    
    # 1000 simulations stacked squares
    square_list <- df2 %>%
      mutate(ymin = y_index - 1,
             ymax = y_index)
    
    x_range <- x_max - x_min
    square_width <- x_range / max_y
    
    square_list <- square_list %>%
      mutate(xmin = diff_bin - square_width / 2,
             xmax = diff_bin + square_width / 2)
    
    base_plot +
      geom_rect(data = square_list,
                aes(xmin = xmin, xmax = xmax,
                    ymin = ymin, ymax = ymax,
                    fill = decision),
                color = "black")
  })
}

shinyApp(ui, server)
