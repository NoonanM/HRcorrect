# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)

source("Correction_Calculator.R")

ui <- fluidPage(
  headerPanel("Calculate a mass specific home range area correction"),
  
  sidebarPanel(width = 4,
    
    numericInput("area","Home range area", NULL, min = 0, max = Inf),
    
    numericInput("mass","Mass", NULL, min = 0, max = Inf),
    
    selectInput("Estimator", 
                label = "Original estimator",
                choices = c("KDE - Gaussian reference function", 
                            "KDE - Silverman's rule of thumb",
                            "KDE - Least Squares Cross-Validation", 
                            "MCP"),
                selected = "KDE - Gaussian reference function"),
    
    actionButton("add", "Calculate")
    
  ),
  
  mainPanel(
    
    tableOutput("results")
    
  )
)

server <- function(input, output){
  
  output$results <- renderTable(HR_Correction(Area =0, Mass = 0))
  
  observeEvent( input$add,{
  output$results <- renderTable(HR_Correction(Area = input$area, Mass = input$mass))
  }
  
  )
  

  
  #output$results <- renderTable({RES})
}


shinyApp(ui = ui, server = server)
