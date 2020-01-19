library(tidyverse)
library(shiny)
library(readxl)
library(openxlsx)
library(stringi)

# Read in data
metadata <- read_csv(file.path(getwd(), "GcCampus metadata.csv"), skip = 1)
catalogue <- read_csv(file.path(getwd(), "catalogue.csv"))
translations <- read_csv(file.path(getwd(), "CST20160704.csv"))

# Convert to UTF-8 where needed
windows_1252_to_utf8 <- function(x) {
  iconv(x, "windows-1252", "UTF-8")
}
metadata[] <- lapply(metadata, windows_1252_to_utf8)
colnames(metadata) <- windows_1252_to_utf8(colnames(metadata))

translations[] <- lapply(translations, windows_1252_to_utf8)
colnames(translations) <- windows_1252_to_utf8(colnames(translations))

# Define UI for application
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Intelligent Curriculum Framework"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput("metadata_cols", "Select metadata column to gather topics on",
                  iconv(colnames(metadata), "UTF-8", "UTF-8", "")),
      selectInput("metadata_cols", "Select metadata column to gather topics on",
                  iconv(colnames(metadata), "UTF-8", "UTF-8", ""))
    ),
    
    mainPanel(
      downloadButton("download_data", "Download data")),
  )
))

#####
# Server
#####

server <- function(input, output) {
  
}


# Run the application 
shinyApp(ui = ui, server = server)
