## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "green",

  #titlePanel("HomeRange Correction"),


  #dashboardHeader(width = 10, title = "HomeRange Correction"),
  dashboardHeader(title = "HomeRangeCorrection"),

  dashboardSidebar(width = 350,

    sidebarMenu(

      #Input the HR area to be corrected
      numericInput("area","Home range (area)", 0, min = 0, max = Inf),

      #Input the mass to correct at
      numericInput("mass","Mass (kg)", 0, min = 0, max = Inf),

      #Input the original estimator (NOT FUNCTIONAL, NEED TO RUN THE FULL REGRESSIONS FIRST)
      selectInput("Estimator",
                  label = "Original estimator (Not currently functional)",
                  choices = c("KDE - Gaussian reference function",
                              "KDE - Silverman's rule of thumb",
                              "KDE - Least Squares Cross-Validation",
                              "MCP"),
                  selected = "KDE - Gaussian reference function"),

      actionButton("calculate", "Calculate", style = "margin-top: 10%;")
    ),


    sidebarMenu(

      actionButton("clear", "Clear all",
                   style =
                     "color:orange;background-color: #232d33;border: transparent;margin-left: 4%; margin-top: 20%")

    ),

    sidebarMenu(

      #tags$style(type="text/css", "#downloadData {background-color:black;color: cyan; font-family: Courier New}"),



      downloadButton("downloadData", "Save results",
                     style =
                       "color: #02c1ef;background-color: #232d33;border: transparent;margin-left: 4%; margin-top: 1%")

               )
    ),

  dashboardBody(

    fluidPage(

      box(width = 6000, tableOutput("results")

          )#Close the box
    )

  )
)

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


  # When the Clear All button is clicked, remove the results object
  observeEvent(input$clear, {

    #RESULTS <
    RESULTS <<- HR_Correction(Area =0, Mass = 0)

    output$results <- renderTable({
      input$calculate
      displayResults()

    }, rownames = TRUE)

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

shinyApp(ui, server)
