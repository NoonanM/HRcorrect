\name{correct}
\alias{correct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
correct()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    ui <- dashboardPage(skin = "green", dashboardHeader(title = "HRcorrect"), 
        dashboardSidebar(width = 330, sidebarMenu(numericInput("area", 
            "Home range (area)", NULL, min = 0, max = Inf), numericInput("mass", 
            "Mass (kg)", NULL, min = 0, max = Inf), selectInput("Estimator", 
            label = "Original estimator (Not currently functional)", 
            choices = c("KDE - Gaussian reference function", 
                "KDE - Silverman's rule of thumb", "KDE - Least Squares Cross-Validation", 
                "MCP"), selected = "KDE - Gaussian reference function"), 
            actionButton("calculate", "Calculate", style = "margin-top: 12\%")), 
            sidebarMenu(actionButton("clear", "Clear all", style = "color: orange;\n                                                      background-color: #232d33;\n                                                      border: transparent;\n                                                      margin-left: 2\%;\n                                                      margin-top: 40\%")), 
            sidebarMenu(downloadButton("downloadData", "Save results", 
                style = "color: #02c1ef;\n                                                        background-color: #232d33;\n                                                        border: transparent;\n                                                        margin-left: 2\%;\n                                                        margin-top: 1\%"))), 
        dashboardBody(background = "grey", box(title = "Application description", 
            width = 6000, solidHeader = TRUE, status = "info", 
            HTML(paste("This application corrects 95\% home range area estimates\n                                   for the mass-specific bias in conventional estimators.", 
                "The correction is based on the regressional analyses in\n                          Noonan et al. (2019) Body size dependent underestimation of home range areas. In prep.", 
                sep = "<br/> <br/>"))), box(title = "Corrected area estimates", 
            solidHeader = TRUE, status = "primary", width = 6000, 
            tableOutput("results"))))
    server <- function(input, output, session) {
        compileResults <- function(data) {
            data <- as.data.frame(data)
            if (exists("RESULTS")) {
                RESULTS <<- rbind(RESULTS, data)
                rownames(RESULTS) <- 1:nrow(RESULTS)
            }
            else {
                RESULTS <<- data
                rownames(RESULTS) <- 1:nrow(RESULTS)
            }
        }
        correctEstimate <- reactive({
            correction <- HR_Correction(Area = input$area, Mass = input$mass)
            correction
        })
        observeEvent(input$calculate, {
            compileResults(correctEstimate())
        })
        observeEvent(input$clear, {
            RESULTS <<- HR_Correction(Area = 0, Mass = 0)
            output$results <- renderTable({
                input$calculate
                displayResults()
            }, rownames = TRUE)
        })
        displayResults <- function() {
            if (exists("RESULTS")) {
                RESULTS
            }
            else {
                HR_Correction(Area = 0, Mass = 0)
            }
        }
        output$results <- renderTable({
            input$calculate
            displayResults()
        }, rownames = TRUE)
        output$downloadData <- downloadHandler(filename = function() {
            paste(input$dataset, "Corrected_Areas.csv", sep = "")
        }, content = function(file) {
            write.csv(displayResults(), file, row.names = FALSE)
        })
    }
    shinyApp(ui, server)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
