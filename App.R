# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)

source("Correction_Calculator.R")


INPUTS <- c("area", "mass")

ui <- fluidPage(
  titlePanel("HomeRange Correction"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Calculate a mass specific correction for a home range area estimated from a conventional estimator."),
      
      width = 5,
      
      numericInput("area","Home range (area)", 0, min = 0, max = Inf),
      
      numericInput("mass","Mass (kg)", 0, min = 0, max = Inf),
      
      selectInput("Estimator", 
                  label = "Original estimator (Not currently functional)",
                  choices = c("KDE - Gaussian reference function", 
                              "KDE - Silverman's rule of thumb",
                              "KDE - Least Squares Cross-Validation", 
                              "MCP"),
                  selected = "KDE - Gaussian reference function"),
      
      actionButton("calculate", "Calculate")
      
    ),
    
    mainPanel(width = 6,
              #headerPanel("Corrected area"),
              
              tableOutput("results")
              
    )
  )
  
)#Closes the ui



server <- function(input, output, session){
  
  
  
  #output$results <- renderTable(HR_Correction(Area =0, Mass = 0))
  
  
  #create a data frame of the results
  compileResults <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("RESULTS")) {
      RESULTS <<- rbind(RESULTS, data)
    } else {
      RESULTS <<- data
    }
  }
  
  
  # Whenever a field is filled, aggregate all form data
  #formData is a reactive function
  inputData <- reactive({
    data <- sapply(INPUTS, function(x) input[[x]])
    data
  })
  
  # When the Save button is clicked, save the form data
  observeEvent(input$calculate, {
    compileResults(inputData())
  })
  
  #Function for displaying the results if the exists
  #Or a blank table if nothing has been calculated
  displayResults <- function() {
    if (exists("RESULTS")) {
      RESULTS
    } else {
      
      HR_Correction(Area =0, Mass = 0)
    }
  }
  
  

  #When the user hits `calculate', generate the new correction
  observeEvent(input$calculate,{
    output$results <- renderTable(HR_Correction(Area = isolate(input$area),
                                                Mass = isolate(input$mass)))
  }
  )
  

  #update the table of results with the current correction when `calculate' is clicked
  output$results <- renderTable({
    input$calculate
    displayResults()
  })

  
  
}


shinyApp(ui = ui, server = server)
