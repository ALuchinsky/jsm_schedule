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
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(htmlwidgets))

empty_table =         data.frame(
  id = "1",
  start = "2025-08-01T10:00:00",
  end =  "2025-08-01T11:00:00",
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

get_event_style_string <- function(type) {
  paste0("<span style=\"", get_event_style(type)[[1]], "\">", type, "</span>")
}

parse_date_string <- function(raw_start) {
  # Step 1: Remove weekday name
  # This removes everything before the first comma and the comma itself
  cleaned <- str_trim(str_remove(raw_start, "^[^,]+,\\s*"))  
  # cleaned is now: "August 4, 2025 , 7:00 AM"
  
  # Step 2: Remove any extra commas
  cleaned <- str_replace_all(cleaned, ",", "")  
  # cleaned is now: "August 4 2025 7:00 AM"
  
  # Step 3: Parse using mdy_hm (month-day-year hour-minute) or mdy_hms if seconds included
  parsed_date <- mdy_hm(cleaned)
  return(parsed_date)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      html, body {
        overflow-y: scroll !important;
        overflow-x: hidden !important;
        overscroll-behavior: contain;
      }
    ")),
      
      tags$script(HTML("
      Shiny.addCustomMessageHandler('bindDoubleClick', function(id) {
        const el = document.getElementById(id);
        if (!el || !el.timeline) return;
        if (el._dblclickBound) return;
        el._dblclickBound = true;

        el.timeline.on('doubleClick', function (props) {
          Shiny.setInputValue(id + '_doubleclick', props, {priority: 'event'});
        });
      });
    ")),
      
      tags$style(HTML("
    .vis-timeline {
      font-size: 15px;
    }"
    ))
  ), # eng of tags$head

    # Application title
    # titlePanel("Old Faithful Geyser Data"),
    fluidRow(
      column(4, sliderInput("wrap_width", "Wrap Width:", min = 10, max = 100, value = 30)), 
      column(4, 
             pickerInput(
               inputId = "selected_day", 
               label = "Select a day", 
               choices = days, 
               selected = days[1], 
               multiple = TRUE,
               options = pickerOptions(
                 actionsBox = TRUE, liveSearch = TRUE
               )
               )
             ),
      column(4, textInput("title_search_pattern", "Filter:", ""))
    ), # end of first row,

    fluidRow(
      column(10, uiOutput("timeline_ui")), 
      column(1, checkboxGroupButtons(
        inputId = "event_select",
        label = "Select event types:",
        choiceNames = sapply(types, get_event_style_string) %>% unname,
        choiceValues = types,
        selected = c("Invited Paper Session ", "Contributed Papers "),
        direction = "vertical",
        justified = TRUE,
        width = "100%",
        checkIcon = list(yes = icon("check")),
        individual = TRUE
      ))
    ) # end of 2nd row

    # Sidebar with a slider input for number of bins 
) # end of ui fluid page

# Define server logic required to draw a histogram
server <- function(input, output, session) {
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
  
  selected_ids <- reactiveVal(c())  # Store clicked item IDs
  redraw_trigger <- reactiveVal(0)  # Force UI re-render
  

  event_select = reactiveVal(types)
  observeEvent(input$event_select,{
    cat(input$event_select,"\n")
    event_select(input$event_select)
  })
  
  title_search_pattern = reactiveVal("fun")
  observeEvent(input$title_search_pattern, {
    cat(input$title_search_pattern)
    title_search_pattern(input$title_search_pattern)
  })

  tv_data <- reactive({
    data_ <- DF %>% 
      filter(day %in% selected_day()) %>% 
      filter(type %in% event_select()) %>% 
      filter(grepl(tolower(title_search_pattern()), tolower(title)) )
    if(nrow(data_) == 0) {
      cat("Empty data table\n")
      return(empty_table)
    } else {
      data <- data_  %>% 
        separate_wider_delim(time, delim = " - ", names = c("start", "end")) %>% 
        mutate(popup = paste0(title, "|", type, "| section: ", id)) %>% 
        transmute(id = 1:nrow(.), day, start, end, title, type, popup) %>% 
        mutate(title  = gsub("\\n", "<br>", str_wrap(title, width = wrap_width() ))) # Adjust width as needed
      final_data <- data.frame(
        id = data$id, 
        start = format(as.POSIXct(paste(data$day, data$start), format = "%A, %B %d, %Y %I:%M %p"), "%Y-%m-%dT%H:%M:%S"),
        end   = format(as.POSIXct(paste(data$day, data$end), format = "%A, %B %d, %Y %I:%M %p"), "%Y-%m-%dT%H:%M:%S"),
        content = data$title,
        style = data$type %>% sapply(get_event_style) %>% sapply(unlist),
        title = data$popup
      )
      bad_rows <- is.na(final_data$start) | is.na(final_data$end)
      if (any(bad_rows)) {
        print("Bad rows found:")
        print(data[bad_rows, ])
      }
      return(final_data)
    }
  })
  
  

  output$timeline_ui <- renderUI({
    redraw_trigger()  # trigger timevis redraw when updated
    tagList(
      timevisOutput("timeline"),
      tags$script("setTimeout(function() {
        Shiny.setInputValue('bind_timevis', Math.random());
      }, 0);")
    )
  })
  
  output$timeline <- renderTimevis({
    timevis(tv_data()) %>% onRender("
      function(el, x) {
        document.getElementById(el.id).timeline = this.timeline;
        setTimeout(function() {
          Shiny.setInputValue('timeline_ready', el.id);
        }, 0);
      }
    ")
  })
  
  observeEvent(input$timeline_ready, {
    session$sendCustomMessage("bindDoubleClick", input$timeline_ready)
  })
  
  observeEvent(input$bind_timevis, {
    session$sendCustomMessage("bindDoubleClick", "timeline")
  })
  
  observeEvent(input$timeline_doubleclick, {
    clicked_id <- input$timeline_doubleclick$item
    cat("item ", clicked_id, " is clicked\n")
    if (!is.null(clicked_id)) {
      current <- selected_ids()
      if (!(clicked_id %in% current)) {
        selected_ids(c(current, clicked_id))  # Add to selection
        cat("Adding ", clicked_id, " selected_ids=", selected_ids(),"\n")
      } else {
        selected_ids(current[current != clicked_id])  # Add to selection
        cat("Removing ", clicked_id, " selected_ids=", selected_ids(),"\n")
      }
      redraw_trigger(redraw_trigger() + 1)  # Force update
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
