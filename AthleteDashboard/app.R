library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardPage()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
