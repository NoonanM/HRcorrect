# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)

source("Correction_Calculator.R")


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
      
      actionButton("calculate", "Calculate"),
      
      downloadButton("downloadData", "Save results")
      
    ),
    
    mainPanel(width = 7,
              #headerPanel("Corrected area"),
              
              tableOutput("results")
              
    )
  )
  
)#Closes the ui



server <- function(input, output, session){

  
  #A function that creates a data frame of the results
  compileResults <- function(data) {
    
    #Coerce to a dataframe
    data <- as.data.frame(data)
    
    #Ask if there are already results
    if (exists("RESULTS")) {
      RESULTS <<- rbind(RESULTS, data)
      
      rownames(RESULTS) <- 1:nrow(RESULTS)
    } else {
      RESULTS <<- data
      
      rownames(RESULTS) <- 1:nrow(RESULTS)
    }
  }
  
  
  #Whenever some of the inputs are edited, calculate a new correction
  #NB: correctEstimate is a reactive function
  correctEstimate <- reactive({
    
    correction <- HR_Correction(Area = input$area,
                                         Mass = input$mass)
    correction
  })
  
  
  
  # When the calculate button is clicked, compile the results
  observeEvent(input$calculate, {
    compileResults(correctEstimate())
  })
  
  
  #Function for displaying the results if they exist
  #Or a blank table if nothing has been calculated
  displayResults <- function() {
    if (exists("RESULTS")) {
      RESULTS
    } else {
      
      HR_Correction(Area =0, Mass = 0)
    }
  }
  
  

  #When the user hits `calculate', generate the new correction
  #observeEvent(input$calculate,{
  #  output$results <- renderTable(HR_Correction(Area = isolate(input$area),
  #                                              Mass = isolate(input$mass)))
  #}
  #)
  
  
  #observeEvent(input$calculate,{
  #  RESULTS <- renderTable(HR_Correction(Area = isolate(input$area),
  #                                              Mass = isolate(input$mass)))
  #}
  #)
  
  
  #output$RES <- renderPrint({RESULTS})

  #update the table of results with the current correction when `calculate' is clicked
  output$results <- renderTable({
    input$calculate
    displayResults()
    
  }, rownames = TRUE)

  
  
  
  # Downloadable csv of corrected areas ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "Corrected_Areas.csv", sep = "")
    },
    content = function(file) {
      write.csv(displayResults(), file, row.names = FALSE)
    }
  )
  
  
}


shinyApp(ui = ui, server = server)
