## app.R ##
library(shiny)
library(shinydashboard)


correct <- function() {
  
#First put together the dashboard based user interface
ui <- dashboardPage(skin = "green",
                    
                    #Define the header
                    dashboardHeader(title = "HRcorrect"),

                    #Define the sidebar
                    dashboardSidebar(width = 330,

                                     #Add elements to the sidebar
                                     sidebarMenu(

                                       #Input the HR area to be corrected
                                       numericInput("area","Home range (area)",
                                                    NULL, min = 0, max = Inf),

                                       #Input the mass to correct at
                                       numericInput("mass","Mass (kg)",
                                                    NULL, min = 0, max = Inf),

                                       #Input the original estimator (NOT FUNCTIONAL, NEED TO RUN THE FULL REGRESSIONS FIRST)
                                       selectInput("Estimator",
                                                   label = "Original estimator (Not currently functional)",
                                                   choices = c("KDE - Gaussian reference function",
                                                               "KDE - Silverman's rule of thumb",
                                                               "KDE - Least Squares Cross-Validation",
                                                               "MCP"),
                                                   selected = "KDE - Gaussian reference function"),

                                       
                                       #An acton button to generate the calculations
                                       actionButton("calculate",
                                                    "Calculate",
                                                    style = "margin-top: 12%")
                                                    #style =
                                                    #  "color: yellow; fsize: 40;
                                                    #  background-color: #232d33;
                                                    #  border: transparent;
                                                    #  margin-left: 2%;
                                                    #  margin-top: 10%")
                                     ),

                                     #Add a separate sidebar (makes the button layout cleaner)
                                     sidebarMenu(

                                       #Add an action button for clearing any previous corrections
                                       actionButton("clear", "Clear all",
                                                    style =
                                                      "color: orange;
                                                      background-color: #232d33;
                                                      border: transparent;
                                                      margin-left: 2%;
                                                      margin-top: 40%")
                                       
                                     ),

                                     sidebarMenu(
                                       #Add a button for saving any calculated corrections as a .csv
                                       downloadButton("downloadData", "Save results",
                                                      style =
                                                        "color: #02c1ef;
                                                        background-color: #232d33;
                                                        border: transparent;
                                                        margin-left: 2%;
                                                        margin-top: 1%")
                                       
                                     )
                    ),

                    dashboardBody(background = "grey",

                      #fluidRow(
                      
                      box(title = "Application description",
                          width = 6000,
                          solidHeader = TRUE,
                          status = "info",
                        #background = "light-blue",
                        HTML(paste("This application corrects 95% home range area estimates
                                   for the mass-specific bias in conventional estimators.",
                          
                          "The correction is based on the regressional analyses in
                          Noonan et al. (2019) Body size dependent underestimation of home range areas. In prep.",
                        sep = "<br/> <br/>")
                        )
                      ),

                        box(title = "Corrected area estimates",
                            solidHeader = TRUE,
                            status = "primary",
                            width = 6000, tableOutput("results")

                        )#Close the box
                      #)

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
    #RESULTS <<- HR_Correction(Area = 0, Mass = 0)

    RESULTS <<- NULL
    
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

      HR_Correction(Area = 0, Mass = 0)
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

  

} #Closes the server function

shinyApp(ui, server)


}
