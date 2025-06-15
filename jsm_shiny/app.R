#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(timevis))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(shinyWidgets))


empty_table =         data.frame(
  id = "1",
  start = "2025-08-01 10AM",
  end =  "2025-08-01 11AM",
  content = "No events"
)


DF <- read.csv("time_table.csv")
days <- DF %>% pull(day) %>% unique
types <- DF %>% pull(type) %>% unique

get_event_style <- function(event_type) {
  style_map <- list(
    "Invited Paper Session " = "background-color: #d62728; color: white; font-weight: bold;",
    "Contributed Papers " = "background-color: #1f77b4; color: white; font-weight: bold;",
    "JSM Hours " = "background-color: #c7c7c7; color: black;",
    "ASA Meetings and Events " = "background-color: #7f7f7f; color: white;",
    "Affiliate Meetings and Events " = "background-color: #bcbd22; color: black;",
    "Professional Development Course/CE " = "background-color: #8c564b; color: white;",
    "Lunchtime Speakers " = "background-color: #ff7f0e; color: white;",
    "Topic-Contributed Paper Session " = "background-color: #aec7e8; color: black;",
    "Invited Panel Session " = "background-color: #e377c2; color: white;",
    "Topic-Contributed Panel Session " = "background-color: #f7b6d2; color: black;",
    "Contributed Speed " = "background-color: #17becf; color: black;",
    "Introductory Overview Lectures " = "background-color: #9467bd; color: white;",
    "Contributed Posters " = "background-color: #9edae5; color: black;",
    "Invited Posters " = "background-color: #c49c94; color: black;",
    "Roundtables – Breakfast " = "background-color: #ffbb78; color: black;",
    "Roundtables – Lunch " = "background-color: #ff9896; color: black;",
    "Late-Breaking Session " = "background-color: #2ca02c; color: white;",
    "Professional Skills Development " = "background-color: #d62728; color: white;",
    "Professional Development Computer Technology Workshop (CTW) " = "background-color: #7f7f7f; color: white;"
  )
  unname(style_map[event_type] %||% "background-color: #dddddd; color: black;")
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  fluidPage(
    tags$head(
      tags$style(HTML("
      html, body {
        overflow-y: scroll !important;
        overflow-x: hidden !important;
        overscroll-behavior: contain;
        width: 95%
      }
    "))
    )),
    
    tags$style(HTML("
    .vis-timeline {
      font-size: 15px;
    }
  ")),

    # Application title
    # titlePanel("Old Faithful Geyser Data"),
    fluidRow(
      column(3, sliderInput("wrap_width", "Wrap Width:", min = 10, max = 100, value = 30)), 
      column(3, 
             pickerInput(
               inputId = "selected_day", 
               label = "Select a day", 
               choices = days, 
               multiple = TRUE,
               options = pickerOptions(
                 actionsBox = TRUE, liveSearch = TRUE
               )
               )
             ),
      column(3, 
             pickerInput(
               inputId = "selected_type",
               label = "Event Type",
               choices = types,
                choicesOpt = list(style = types %>% sapply(get_event_style) %>% sapply(unlist)
               ),
             multiple = TRUE,
             options = pickerOptions(
               actionsBox = TRUE, liveSearch = TRUE
             ))
      ),
      column(3, textInput("title_search_pattern", "Filter:", ""))
    ),

    timevisOutput("mytime"), # timevis output with ID "mytime"

    # Sidebar with a slider input for number of bins 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  wrap_width <- reactiveVal(30) # Initial wrap width
  # wrap_width = 30
  observeEvent(input$wrap_width, {
    wrap_width(input$wrap_width)
  })
  
  selected_day = reactiveVal( days[4])
  observeEvent(input$selected_day, {
    cat(input$selected_day)
    selected_day(input$selected_day)
  })
  

  selected_type = reactiveVal("Invited Paper Session ")
  observeEvent(input$selected_type, {
    cat(input$selected_type)
    selected_type(input$selected_type)
  })
  
  title_search_pattern = reactiveVal("fun")
  observeEvent(input$title_search_pattern, {
    cat(input$title_search_pattern)
    title_search_pattern(input$title_search_pattern)
  })
  
  
  tv <- reactive({
    data_ <- DF %>% 
      filter(day %in% selected_day()) %>% 
      filter(type %in% selected_type()) %>% 
      filter(grepl(tolower(title_search_pattern()), tolower(title)) )
    if(nrow(data_) == 0) {
      cat("Empty data table\n")
      return(empty_table)
    } else {
      data <- data_  %>% 
        separate_wider_delim(time, delim = " - ", names = c("start", "end")) %>% 
        transmute(id = 1:nrow(.), day, start, end, title, type) %>% 
        mutate(title  = gsub("\\n", "<br>", str_wrap(title, width = wrap_width() ))) # Adjust width as needed
      return(
        data.frame(
          id = data$id, 
          start = paste(data$day, ",", data$start), 
          end =  paste(data$day, data$end), content = data$title,
          style = data$type %>% sapply(get_event_style) %>% sapply(unlist)
        )
      )
    }
  })

  output$mytime = renderTimevis(timevis(tv()))
}

# Run the application 
shinyApp(ui = ui, server = server)
