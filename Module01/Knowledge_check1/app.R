library(shiny)

ui <- fluidPage(
  titlePanel(" "),
  
  wellPanel(
    tags$h4(style = "color: steelblue", "Population Treatment Effect"),
  radioButtons(
    "q1",
    "Which of the following statements is the best description of the Population Treatment Effect?",
    choices = c("The average value of the endpoint in the study, compared to a published reference.",
                "The theoretical treatment effect based on the whole target population.",
                "Evidence of how well the treatment works in a subgroup of interest.",
                "The treatment effect observed in the study.")
  )),
  wellPanel(
    tags$h4(style = "color: steelblue", "Standard Error"),
  radioButtons(
    "q2",
    "Choose the best definition of a standard error?",
    choices = c("A measure of between-patient variability of the data.",
                "An assessment of adherence to the protocol.",
                "A range of plausible values for the population treatment effect.",
                "How much a statistic would vary if we repeated the study many times.")
  )),
  wellPanel(
  radioButtons(
    "q3",
    "Which of the following can reduce the standard error in a study?",
    choices = c("Increasing the sample size",
                "Reducing the between-patient variability.",
                "Improving precision of the endpoint.",
                "All of the above.")
  )),
  
  wellPanel(
    tags$h4(style = "color: steelblue","Confidence Intervals"),
  radioButtons(
    "q4",
    "Choose the most appropriate definition of a 95% confidence interval",
    choices = c("An interval where the population effect lies with 95% probability.",
                "A range of values where the treatment works for 95% of patients.",
                "A set of values that captures 95% of individual patient outcomes.",
                "A range of plausible values for the population treatment effect, compatible with the trial data.")
  )),
  
  actionButton("submit", "Submit Answers")
)

server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    
    correct_answers <- c(
      q1 = "The theoretical treatment effect based on the whole target population.",
      q2 = "How much a statistic would vary if we repeated the study many times.",
      q3 = "All of the above.",
      q4 = "A range of plausible values for the population treatment effect, compatible with the trial data."
    )
    
    user_answers <- c(
      q1 = input$q1,
      q2 = input$q2,
      q3 = input$q3,
      q4 = input$q4
    )
    
    # Check unanswered questions
    if (any(is.null(user_answers))) {
      showNotification(
        "Please answer all questions before submitting.",
        type = "error"
      )
      return()
    }
    
    score <- sum(user_answers == correct_answers)
    
    showNotification(
      paste0(
        "You got ", score, " out of 4 correct.\n",
        if (score == 4) "Perfect score!" else "Try again!"
      ),
      type = if (score == 4) "message" else "warning",
      duration = 5
    )
  })
}

shinyApp(ui = ui, server = server)
