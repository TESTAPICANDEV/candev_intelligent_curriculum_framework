library(tidyverse)
library(shiny)
library(readxl)
library(openxlsx)

# Define UI for application
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fuzzy Logic"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      fileInput("bus_file", "BUS file (xlsx)"),
      numericInput("margin", "Margin of error for string matching",
                   2, 0, 100, 1),
      numericInput("sheet_num", "Sheet number",
                   1, 0, 999, 1), br(),
      actionButton("load_file", "Load file")
    ),
    
    mainPanel(
      downloadButton("download_data", "Download data with fuzzy matches")),
  )
))

#####
# Server
#####

server <- function(input, output) {
  
}


# Run the application 
shinyApp(ui = ui, server = server)
