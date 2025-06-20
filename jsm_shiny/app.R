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
suppressPackageStartupMessages(library(stringr))

empty_table =         data.frame(
  id = "1",
  start = "2025-08-01T10:00:00",
  end =  "2025-08-01T11:00:00",
  content = "No events"
)


DF <- read.csv("time_table.csv")
days <- DF %>% pull(day) %>% unique
types <- DF %>% pull(type) %>% unique
shaded_events <- c()


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

to_POSIX_date <- function(day, time) as.POSIXct(paste(day, time), format = "%A, %B %d, %Y %I:%M %p") 

writeClipboard <- function(v) {
  clip <- pipe("pbcopy", "w")
  writeLines(paste(v), clip)
  close(clip)
}

update_shaded <- function(sel_sections, data_var) {
  shaded_events <<- c()
  # selected_ids <- data_var[data_var$section %in% selected_sections(), ]$id
  for(s_section in sel_sections) {
    sid_data <- data_var[data_var$section == s_section,]
    sid_start <- to_POSIX_date(sid_data$day, sid_data$start)
    sid_end <- to_POSIX_date(sid_data$day, sid_data$end)
    if (length(sid_start) == 0 || is.na(sid_start) ||
        length(sid_end)   == 0 || is.na(sid_end)) {
      next
    }
    for(ev_section in data_var$section) {
      if(ev_section == s_section) next;
      ev_data = data_var[data_var$section == ev_section,]
      ev_start <- to_POSIX_date(ev_data$day, ev_data$start)
      ev_end <- to_POSIX_date(ev_data$day, ev_data$end)
      if (length(ev_start) == 0 || is.na(ev_start) ||
          length(ev_end)   == 0 || is.na(ev_end)) {
        next
      }
      if( !( ev_end <= sid_start || ev_start >= sid_end)) {
        shaded_events <<- append(shaded_events, ev_section)
      }
    }
  }
  cat("shaded_events = ", shaded_events,"\n")
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
      column(1, actionBttn("reset_btn", "Reset")),
      column(1),
      column(1, actionBttn("info_btn", "Info")),
      column(1),
      column(1, actionBttn("redraw_btn", "Redraw")),
      column(1),
      column(1, downloadBttn("save_submit", "Download")),
      column(1),
      column(1, fileInput("upload_schedule", "Upload", accept = "txt"))
    ),
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
  
  selected_sections <- reactiveVal( c())
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

  # redraw function
  data <- reactive({
    data_ <<- DF %>% 
      filter(day %in% selected_day()) %>% 
      filter(type %in% event_select()) %>% 
      filter(grepl(tolower(title_search_pattern()), tolower(title)) )
    if(nrow(data_) == 0) {
      return( data_)
    }
    selected_ids <- data_[data_$id %in% selected_sections(),]$id
    cat("selected_ids = ", selected_ids, "\n")
    data_var <- data_  %>% 
      separate_wider_delim(time, delim = " - ", names = c("start", "end")) %>% 
      mutate(section = id, popup = paste0(title, "|", type, "| section: ", id)) %>% 
      transmute(id = 1:nrow(.), day, start, end, title, type, popup, section) %>% 
      mutate(title = ifelse(section %in% selected_ids, toupper(title), title)) %>% 
      mutate(title  = gsub("\\n", "<br>", str_wrap(title, width = wrap_width() ))) # Adjust width as needed
    
    print("data(): data_var")
    print(data_var)
    return(data_var)
  })
  
  tv_data <- reactive({
    data_var  <- data()
    print("tv_data: data_var")
    print(data_var)
    if(nrow(data_var) == 0) {
      cat("Empty data table\n")
      return(empty_table)
    } else {
      update_shaded( selected_sections(), data_var )
        shaded_ids <- data_var[data_var$section %in% shaded_events, ]$id
        final_data <- data.frame(
          id = data_var$id, 
          start = format(as.POSIXct(paste(data_var$day, data_var$start), format = "%A, %B %d, %Y %I:%M %p"), "%Y-%m-%dT%H:%M:%S"),
          end   = format(as.POSIXct(paste(data_var$day, data_var$end), format = "%A, %B %d, %Y %I:%M %p"), "%Y-%m-%dT%H:%M:%S"),
          content = data_var$title,
          style = data_var$type %>% sapply(get_event_style) %>% sapply(unlist),
          title = data_var$popup
      ) %>% mutate(
        style = ifelse(id %in% shaded_ids, "background-color: #f0f0f0; color: #888888;", style)
      )
        print("tv_data: shaded_events")
        print(shaded_events)
        print("End of tv_data")
        
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
    print("renderTimeviz: tv_data()")
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
    data_var <- data()
    cat("data_var$id[1] = ", data_var$id[1],"\n")
    if (!is.null(clicked_id)) {
      clicked_section <- data_var[data_var$id == clicked_id,]$section
      if(clicked_section %in% shaded_events) {
        return()
      }
      cat("clicked_section = ", clicked_section, "\n")
      if(is.null(clicked_section)) return();
      if (!(clicked_section %in% selected_sections())) {
        selected_sections( c(selected_sections(), clicked_section))
        cat("Adding section ", clicked_section, " selected_sections=", selected_sections(),"\n")
      } else {
        selected_sections( selected_sections()[selected_sections() != clicked_section])
        cat("Removing section ", clicked_section, " selected_sections=", selected_sections(),"\n")
      }
      # updating shaded
      # update_shaded( selected_sections(), data_var )
      redraw_trigger(redraw_trigger() + 1)  # Force update
    }
  })
  
  observeEvent(input$info_btn,{
    cat("INFO: \n")
    str(data())
    cat(" shaded_events = ", shaded_events, "\n")
    cat(" selected_sections=", selected_sections(), "\n")
    writeClipboard(selected_sections())
  })
  
  observeEvent(input$reset_btn, {
    cat("Reset button clicked\n")
    shaded_events <<- c()
    selected_sections(c())
    redraw_trigger(redraw_trigger() + 1)  # Force update
  })
  
  observeEvent(input$redraw_btn, {
    cat("Redraw #", redraw_trigger(), "\n")
    redraw_trigger(redraw_trigger() + 1)  # Force update
  })
 

  output$save_submit <- downloadHandler(
    filename = function() {
      "schedule.txt"
    },
    content = function(file) {
      text <- ""
      s_sections <- selected_sections()
      text <- paste(text, paste0(" You have ", length(s_sections), " selected sections"), sep = "\n")
      days <- DF[DF$id %in% s_sections,]$day %>% unique
      for(d in days) {
        df <- DF %>% filter(day == d) %>% filter(id %in% s_sections)
        text <- paste(text, "\n=========", d, "=========", sep = "\n")
        for(i in 1:nrow(df)) {
          line <- paste0(df[i,]$time, ": section ", df[i,]$id," \"", df[i,]$title,"\" /", df[i,]$type, "/")
          text <- paste(text, line, sep="\n")
        }
      }
      cat("text=\n", text, "\n")
      writeLines(text, file)
    }
  )
    

  
  observeEvent(input$load_btn, {
    cat("Load button pushed\n")
    showModal(modalDialog(
      title = "Load Data",
      tags$p("Paste comma-separates section numbers"),
      textInput("loaded_sections", "Sections:"),
      easyClose = TRUE,
      footer = actionBttn("load_submit", "OK")
    ))
  })
  

  observeEvent(input$upload_schedule, {
    req(input$upload_schedule)
    text <- paste(readLines(input$upload_schedule$datapath, warn = FALSE), collapse = "\n")
    cat("=== text ======\n")
    cat(text, "\n")
    numbers <- stringr::str_extract_all(text, "\\b\\d{4}\\b")[[1]]
    cat("-- numbers --\n")
    print(numbers)
    sects <- sapply(numbers, as.integer, USE.NAMES = FALSE)
    cat("Submitted: ", sects, "\n")
    selected_sections(sects)
    data_var <- data()
    # update_shaded( selected_sections(), data_var )
    redraw_trigger(redraw_trigger() + 1)  # Force update
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
