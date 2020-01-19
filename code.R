library(tidyverse)
library(shiny)
library(readxl)
library(openxlsx)
library(stringi)
library(stopwords)
library(tidytext)
library(textmineR)
library(wordcloud)

load(file.path(getwd(), "demo.RData"))

#####
# Read in data
#####

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
translations <- read_csv(file.path(getwd(), "CST20160704.csv"),
                         col_names = c("term1", "category", "term2")) %>%
  windows_1252_to_utf8_table()

#####
# Format metadata
#####

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

# Get list of course names
course_names <- metadata$`title EN/Titre EN`[2:length(metadata$`title EN/Titre EN`)] %>%
  unique()

sample_metadata <- metadata[1:10, ]
sample_course_names <- course_names[1:10]

#####
# List interdependencies
#####

# 8 possible dependencies: Related Term, Subject Category, Use, Used For,
# Broader Term, Scope Note, Narrower Term, History note
# French is excluded

thesaurus <- filter(translations, category != "French")
term1 <- thesaurus$term1
term2 <- thesaurus$term2
category <- thesaurus$category
term_list <- unique(c(term1, term2))
interdependencies <- lapply(term_list, function(x) {
  c(term2[(term1 == x) & (category == "Related Term")],
    term1[(term2 == x) & (category == "Related Term")],
    term1[(term2 == x) & (category == "Subject Category")],
    term2[(term1 == x) & (category == "Use")],
    term1[(term2 == x) & (category == "Use")],
    term2[(term1 == x) & (category == "Used For")],
    term1[(term2 == x) & (category == "Used For")],
    term1[(term2 == x) & (category == "Broader Term")],
    term2[(term1 == x) & (category == "Scope Note")],
    term1[(term2 == x) & (category == "Scope Note")],
    term2[(term1 == x) & (category == "Narrower Term")]) %>%
    unique()
})
names(interdependencies) <- term_list

#####
# UI
#####

# Define UI for application
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Intelligent Curriculum Framework"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
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
      tabsetPanel(
        tabPanel("Sample metadata",
                 numericInput("init_row", "Initial row", 1),
                 tableOutput("sample_table")
        ),
        tabPanel("Modelling",
          selectInput("desc_len", "Description to gather topics on", c("Full", "Simple")),
          numericInput("k", "k value", 30, 1),
          actionButton("model", "Perform LDA modelling"),
          tableOutput("lda_model"),
          tableOutput("lda_model_thesaurus")
        )
      )
    )
  )
))

#####
# Server
#####

server <- function(input, output) {
  filtered_data <- reactiveVal(metadata)
  
  output$sample_table <- renderTable(filtered_data()[input$init_row:(input$init_row + 10),
                                                     c("title EN/Titre EN",
                                                       "simple description EN/description simple ang",
                                                       "Duration HH:MM:SS")])
  
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
    
    output$sample_table <- renderTable(filtered_metadata[input$init_row:(input$init_row + 10),
                                                         c("title EN/Titre EN",
                                                           "simple description EN/description simple ang",
                                                           "Duration HH:MM:SS")])
    
    filtered_data(filtered_metadata)
  })
  
  observeEvent(input$model, {
    #####
    # First run - no thesaurus
    #####
    desc <- ifelse(input$desc_len == "Full",
                   "description for GCcampus EN/description pour GCcampus ang",
                   "simple description EN/description simple ang")
    model_data <- select(sample_metadata, id = `title EN/Titre EN`,
                         desc = desc)[2:nrow(sample_metadata), ] %>%
      unnest_tokens(word, desc)
    model_data$word <- str_replace_all(model_data$word, "[[:digit:]]+", "") %>%
      str_replace_all("[[:punct:]]+", "")
    model_data <- filter(model_data, str_length(word) > 2) %>%
      anti_join(stop_words) %>%
      mutate(index = row_number()) %>%
      group_by(id) %>%
      mutate(index = row_number()) %>%
      spread(key = index, value = word)
    model_data[is.na(model_data)] <- ""
    model_data <- unite(model_data, desc, -id, sep = " ")
    model_data$desc <- str_trim(model_data$desc)
    
    # Credit to
    # https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25#https://www.kaggle.com/crowdflower/first-gop-debate-twitter-sentiment
    # for the following code
    dtm <- CreateDtm(model_data$desc, doc_names = model_data$id, 
                     ngram_window = c(1, 2))
    tf <- TermDocFreq(dtm = dtm)
    original_tf <- select(tf, term, term_freq, doc_freq)
    rownames(original_tf) <- 1:nrow(original_tf)
    vocabulary <- tf$term[tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2]
    
    k_list <- seq(1, input$k, by = 1)
    model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
    if (!dir.exists(model_dir)) dir.create(model_dir)
    model_list <- TmParallelApply(X = k_list, FUN = function(k){
      filename = file.path(model_dir, paste0(k, "_topics.rda"))
      
      if (!file.exists(filename)) {
        m <- FitLdaModel(dtm = dtm, k = k, iterations = 50)
        m$k <- k
        m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
        save(m, file = filename)
      } else {
        load(filename)
      }
      m
    }, export = c("dtm", "model_dir")) # export only needed for Windows machines
    # model tuning
    # choosing the best model
    coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                                coherence = sapply(model_list, function(x) mean(x$coherence)), 
                                stringsAsFactors = FALSE)
    model <- model_list[which.max(coherence_mat$coherence)][[1]]
    model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
    top20_wide <- as.data.frame(model$top_terms)
    
    #####
    # Second run - include thesaurus
    #####
    desc <- ifelse(input$desc_len == "Full",
                   "description for GCcampus EN/description pour GCcampus ang",
                   "simple description EN/description simple ang")
    model_data <- select(sample_metadata, id = `title EN/Titre EN`,
                         desc = desc)[2:nrow(sample_metadata), ]
    model_data$desc <- lapply(model_data$desc, function(x) {
      synonyms <- lapply(names(interdependencies), function(y) {
        if (str_detect(x, y)) {
          return(paste(interdependencies[[y]], collapse = ' '))
        }
        ''
      }) %>%
        paste(collapse = '')
      paste(x, synonyms)
    }) %>%
      unlist()
    model_data <- unnest_tokens(model_data, word, desc)
    model_data$word <- str_replace_all(model_data$word, "[[:digit:]]+", "") %>%
      str_replace_all("[[:punct:]]+", "")
    model_data <- filter(model_data, str_length(word) > 2) %>%
      anti_join(stop_words) %>%
      mutate(index = row_number()) %>%
      group_by(id) %>%
      mutate(index = row_number()) %>%
      spread(key = index, value = word)
    model_data[is.na(model_data)] <- ""
    model_data <- unite(model_data, desc, -id, sep = " ")
    model_data$desc <- str_trim(model_data$desc)
    
    # Credit to
    # https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25#https://www.kaggle.com/crowdflower/first-gop-debate-twitter-sentiment
    # for the following code
    dtm <- CreateDtm(model_data$desc, doc_names = model_data$id, 
                     ngram_window = c(1, 2))
    tf <- TermDocFreq(dtm = dtm)
    original_tf <- select(tf, term, term_freq, doc_freq)
    rownames(original_tf) <- 1:nrow(original_tf)
    vocabulary <- tf$term[tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2]
    
    k_list <- seq(1, input$k, by = 1)
    model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
    if (!dir.exists(model_dir)) dir.create(model_dir)
    model_list <- TmParallelApply(X = k_list, FUN = function(k){
      filename = file.path(model_dir, paste0(k, "_topics.rda"))
      
      if (!file.exists(filename)) {
        m <- FitLdaModel(dtm = dtm, k = k, iterations = 50)
        m$k <- k
        m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
        save(m, file = filename)
      } else {
        load(filename)
      }
      m
    }, export = c("dtm", "model_dir")) # export only needed for Windows machines
    # model tuning
    # choosing the best model
    coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                                coherence = sapply(model_list, function(x) mean(x$coherence)), 
                                stringsAsFactors = FALSE)
    model_thesaurus <- model_list[which.max(coherence_mat$coherence)][[1]]
    model_thesaurus$top_terms <- GetTopTerms(phi = model_thesaurus$phi, M = 20)
    top20_wide_thesaurus <- as.data.frame(model_thesaurus$top_terms)
    
    output$lda_model <- renderTable(top20_wide)
    output$lda_model_thesaurus <- renderTable(top20_wide_thesaurus)
    
    #visualising topics of words based on the max value of phi
    set.seed(1234)
    final_summary_words <- data.frame(top_terms = t(model$top_terms))
    final_summary_words$topic <- rownames(final_summary_words)
    rownames(final_summary_words) <- 1:nrow(final_summary_words)
    final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
    final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
    final_summary_words <- final_summary_words %>% group_by(topic,word)
    final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
      ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
    word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))
    
    for(i in 1:length(unique(final_summary_words$topic))) {
      pdf(file.path(getwd(), 'models', paste0("cluster", i, ".pdf")))
      temp <- filter(final_summary_words, topic == i)$word
      wordcloud(words = temp, freq = word_topic_freq$term_freq,
                min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))
      dev.off()
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
