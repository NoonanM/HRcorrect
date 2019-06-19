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
      
      numericInput("area","Home range (area)", NULL, min = 0, max = Inf),
      
      numericInput("mass","Mass (kg)", NULL, min = 0, max = Inf),
      
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
              headerPanel("Corrected area"),
              
              tableOutput("results")
              
    )
  )
  
)#Closes the ui



server <- function(input, output){
  
  output$results <- renderTable(HR_Correction(Area =0, Mass = 0))
  
  
  
  observeEvent( input$calculate,{
    output$results <- renderTable(HR_Correction(Area = input$area, Mass = input$mass))
  }
  
  )
  
  
  
  #output$results <- renderTable({RES})
}


shinyApp(ui = ui, server = server)
