library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Hypothesis Testing by Simulation"),
  
  tags$style(HTML("
    .flex-row { display: flex; align-items: stretch; }
    .flex-col { flex: 1; padding: 10px; min-height: 700px; }
    .plot-col { display: flex; flex-direction: column; min-height: 700px; }
    .plot-col .plot-output { flex: 1; }
  ")),
  
  div(class = "flex-row",
      
      div(class = "flex-col",
          
          div(style="background:#f0f0f0; padding:10px; border-radius:5px; margin-bottom:10px;",
              textInput("sd_val", "Population SD:", value = "370"),
              radioButtons("alpha", "Significance level (alpha):",
                           choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                           selected = 0.05)
          ),
          
          sliderInput("n", "Sample size per group (n):", 50, 500, 250, step = 50),
          sliderInput("true_diff", "True treatment difference:", 0, 300, 100, step = 25),
          
          radioButtons("sim_mode", "Simulation mode:",
                       c("Single simulation", "1000 simulations")),
          
          actionButton("resimulate", "Resimulate"),
          
          br(),
          htmlOutput("power_text"),
          br(),
          htmlOutput("cv_text")
      ),
      
      div(class = "flex-col plot-col",
          div(class = "plot-output",
              plotOutput("plot", height = "100%")
          )
      )
  )
)

server <- function(input, output, session) {
  
  sd_diff_reactive <- reactive({
    sd_val <- as.numeric(input$sd_val)
    if (is.na(sd_val) || sd_val <= 0) sd_val <- 370
    sqrt(2 * sd_val^2 / input$n)
  })
  
  critical_value <- reactive({
    qnorm(1 - as.numeric(input$alpha)/2, 0, sd_diff_reactive())
  })
  
  single_sim <- reactiveVal(NULL)
  sims_1000 <- reactiveVal(NULL)
  
  generate_sim <- function() {
    sd_diff <- sd_diff_reactive()
    cv <- critical_value()
    
    if (input$sim_mode == "Single simulation") {
      diff <- rnorm(1, input$true_diff, sd_diff)
      decision <- ifelse(diff > cv, "reject", "accept")
      tibble(diff = diff, decision = decision)
    } else {
      diffs <- rnorm(1000, input$true_diff, sd_diff)
      decision <- ifelse(diffs > cv, "reject", "accept")
      tibble(diff = diffs, decision = decision)
    }
  }
  
  auto_trigger <- reactive({
    list(input$n, input$true_diff, input$sd_val, input$alpha, input$sim_mode)
  })
  
  observeEvent(list(auto_trigger(), input$resimulate), {
    new_sim <- generate_sim()
    if (input$sim_mode == "Single simulation") {
      single_sim(new_sim)
    } else {
      sims_1000(new_sim)
    }
  }, ignoreInit = FALSE)
  
  sims <- reactive({
    if (input$sim_mode == "Single simulation") single_sim() else sims_1000()
  })
  
  output$power_text <- renderUI({
    if (input$sim_mode == "1000 simulations") {
      p <- round(mean(sims()$decision == "reject") * 100)
      HTML(paste0("<span style='color:darkblue;font-size:20px;'>Simulated power: ", p, "%</span>"))
    }
  })
  
  output$cv_text <- renderUI({
    HTML(paste0("<span style='color:darkblue;font-size:18px;'>Critical value: ",
                round(critical_value(), 2), "</span>"))
  })
  
  output$plot <- renderPlot({
    
    df <- sims()
    sd_diff <- sd_diff_reactive()
    cv <- critical_value()
    
    margin <- 5 * sd_diff
    x_min <- min(-margin, input$true_diff - margin)
    x_max <- max(margin, input$true_diff + margin)
    
    if (input$sim_mode == "Single simulation") {
      max_y <- 20
      y_center <- max_y / 2
    } else {
      bin_width <- sd_diff / 5
      df <- df %>%
        mutate(bin = round(diff / bin_width) * bin_width) %>%
        group_by(bin) %>%
        mutate(y = row_number()) %>%
        ungroup()
      max_y <- max(df$y, 20)
    }
    
    # density for null only
    xseq <- seq(x_min, x_max, length.out = 400)
    null_dens <- tibble(x = xseq, y = dnorm(xseq, 0, sd_diff))
    null_dens$y <- null_dens$y / max(null_dens$y) * max_y
    
    text_y <- max_y * 1.05
    shade_top <- max_y * 1.12
    
    base <- ggplot() +
      
      # non-rejection background
      geom_rect(aes(xmin=x_min, xmax=cv, ymin=0, ymax=shade_top),
                fill="#FCF7E4", alpha=0.50) +
      
      # rejection region
      geom_rect(aes(xmin=cv, xmax=x_max, ymin=0, ymax=shade_top),
                fill="#dd7e0e", alpha=0.25) +
      
      # CV dashed line
      geom_vline(xintercept=cv, linetype="dashed", color="black") +
      
      # reference line at zero
      geom_vline(xintercept=0, color="#d9d9d9", linewidth=1) +
      
      # null hypothesis density
      geom_line(data=null_dens, aes(x=x, y=y), color="#bdbdbd", linewidth=1) +
      
      # annotations
      annotate("text", x=(cv+x_max)/2, y=text_y,
               label="Reject the null hypothesis", color="darkblue", size=5) +
      annotate("text", x=(x_min+cv)/2, y=text_y,
               label="Accept null hypothesis", color="darkblue", size=5) +
      
      coord_cartesian(xlim=c(x_min,x_max), ylim=c(0,shade_top)) +
      
      scale_fill_manual(values=c(accept="#e31a1c", reject="#41ab5d")) +
      scale_alpha_manual(values=c(accept=0.25, reject=0.6)) +
      
      theme_minimal(base_size=16) +
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none") +
      xlab("Estimated treatment effect")
    
    if (input$sim_mode == "Single simulation") {
      base +
        geom_point(aes(x=df$diff, y=y_center, fill=df$decision),
                   shape=21, size=6)
    } else {
      square_w <- (x_max - x_min) / max_y
      
      df <- df %>%
        mutate(xmin = bin - square_w/2,
               xmax = bin + square_w/2,
               ymin = y - 1,
               ymax = y)
      
      base +
        geom_rect(data=df,
                  aes(xmin=xmin, xmax=xmax,
                      ymin=ymin, ymax=ymax,
                      fill=decision, alpha=decision),
                  color="black")
    }
  })
}

shinyApp(ui, server)
