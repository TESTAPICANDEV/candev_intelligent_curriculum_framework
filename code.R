library(tidyverse)
library(shiny)
library(readxl)
library(openxlsx)
library(stringi)

# Functions to convert to UTF-8
windows_1252_to_utf8 <- function(x) {
  iconv(x, "windows-1252", "UTF-8")
}

windows_1252_to_utf8_table <- function(tabl) {
  tabl_copy <- tabl
  tabl_copy[] <- lapply(tabl, windows_1252_to_utf8)
  colnames(tabl_copy) <- windows_1252_to_utf8(colnames(tabl_copy))
  tabl_copy
}

# Read in data
metadata <- read_csv(file.path(getwd(), "GcCampus metadata.csv"), skip = 1) %>%
  windows_1252_to_utf8_table()
catalogue <- read_csv(file.path(getwd(), "catalogue.csv")) %>%
  windows_1252_to_utf8_table()
translations <- read_csv(file.path(getwd(), "CST20160704.csv")) %>%
  windows_1252_to_utf8_table()

# Convert the time to HH:MM:SS
double_digit_num <- function(x) {
  ifelse(str_length(x) == 1, paste0('0', x), x)
}
hh_mm <- metadata$`Duration HH:MM` %>%
  ifelse(str_detect(., "sec") & (str_length(.) == 5),
         paste0("00:00:", substr(., 1, 2)), .) %>%
  ifelse(str_detect(., "min") & (str_length(.) == 5),
         paste0("00:", substr(., 1, 2), ":00"), .)
hh_mm[1018] <- "00:02:54"
hh <- ifelse(str_detect(hh_mm, "1h"), '01', hh_mm) %>%
  ifelse(str_length(.) == 5, str_sub(., 1, 2), .) %>%
  ifelse(str_length(.) == 4, paste0('0', str_sub(., 1, 1)), .) %>%
  ifelse(str_length(.) == 8, str_sub(., 1, 2), .) %>%
  ifelse(str_length(.) != 2, "00", .)
mm <- ifelse(str_length(hh_mm) == 5, str_sub(hh_mm, 4, 5), hh_mm) %>%
  ifelse(str_length(.) == 4, str_sub(., 3, 4), .) %>%
  ifelse(str_length(.) == 8, str_sub(., 4, 5), .) %>%
  str_remove_all(" ") %>%
  str_remove_all("1hr") %>%
  str_remove_all("1h") %>%
  str_remove_all("m.*") %>%
  double_digit_num()
ss <- ifelse(str_length(hh_mm) <= 5, "00", hh_mm) %>%
  ifelse(str_length(.) == 8, str_sub(., 7, 8), .) %>%
  str_remove_all(" ") %>%
  str_remove_all(".*n") %>%
  str_remove_all(".*m") %>%
  str_remove_all("sec") %>%
  double_digit_num()
metadata$`Duration HH:MM:SS` <- paste(hh, mm, ss, sep = ':') %>%
  ifelse(str_detect(., "NA"), "00:00:00", .)

# Get vector for course types in metadata
course_types <- unique(metadata$`Course type EN/Type de cours ang`)

# Get checkbox columns in metadata
checkbox_cols <- lapply(metadata, function(tabl_col) {
  all(unique(tabl_col) == "X", na.rm = TRUE) &&
    (length(unique(tabl_col)) == 2)
}) %>%
  unlist() %>%
  names(.)[.]

# Define UI for application
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Intelligent Curriculum Framework"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput("desc_len", "Description to gather topics on", c("Full", "Simple")),
      selectInput("course_type", "Course type", c("any", course_types)),
      numericInput("min_duration", "Minimum course duration (hours)", 1, 0),
      numericInput("max_duration", "Maximum course duration (hours)", 30, 1),
      checkboxInput("include_checks", "Only include courses with specific tags?"),
      conditionalPanel("input.include_checks",
                       checkboxGroupInput("include_checklist", NULL, checkbox_cols)),
      checkboxInput("exclude_checks", "Only include courses without specific tags?"),
      conditionalPanel("input.exclude_checks",
                       checkboxGroupInput("exclude_checklist", NULL, checkbox_cols)),
      actionButton("filter", "Filter data")
    ),
    
    mainPanel(
      downloadButton("download_data", "Download data"))
  )
))

#####
# Server
#####

server <- function(input, output) {
  filtered_data <- reactiveVal(metadata)
  
  observeEvent(input$filter, {
    course_type <- input$course_type
    if (course_type == "any") {
      course_type <- course_types
    }
    filtered_metadata <- metadata %>%
      filter(`Course type EN/Type de cours ang` %in% course_type) %>%
      filter(str_sub(`Duration HH:MM:SS`, 1, 2) >= double_digit_num(input$min_duration)) %>%
      filter(str_sub(`Duration HH:MM:SS`, 1, 2) <= double_digit_num(input$max_duration))
    if (input$include_checks & !is.null(input$include_checklist)) {
      not_in_checklist <- checkbox_cols[!(checkbox_cols %in% input$include_checklist)]
      filtered_metadata <- select(-not_in_checklist)
    }
    if (input$exclude_checks & !is.null(input$exclude_checklist)) {
      in_checklist <- checkbox_cols[checkbox_cols %in% input$exclude_checklist]
      filtered_metadata <- select(-in_checklist)
    }
    print(filtered_metadata)
    filtered_data(filtered_metadata)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
