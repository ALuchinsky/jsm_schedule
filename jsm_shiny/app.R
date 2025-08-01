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
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(commonmark))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(rintrojs))

source("./scrap_event_info.R")

empty_table =         data.frame(
  id = "1",
  start = "2025-08-01T10:00:00",
  end =  "2025-08-01T11:00:00",
  content = "No events"
)

# global variable to turn on/off the debug console print
debug_print = TRUE

# Information about all JSM events
#   scrapped by ../JSM_program.Rmd code from JSM2025 site
#       id (int): section number
#       time (string): time of the event in "<start> - <end> format"
#       title (string): title of the event
#       type (string): type of the event
#       day  (string): day (like "Friday, August 1, 2025")
DF <- read.csv("time_table.csv")

# list of the conference days
days <- DF %>% pull(day) %>% unique

# list of the conference types
types <- DF %>% pull(type) %>% unique

# data.frame with detailed information about each of the events (sections)
#  for each event a list of talks is stores, one row for talk
#   is collected automatically by web scrapping of the JSM site on the right click of the corresponding event
#       section (int): section number (could repeat)
#       root (string): room (could repeat)
#       title (string): title of the talk
#       speakers (string): comma-separated list of the authors of the talk
DF_sections <- data.frame()

# data.frame with detailed information about one event
#   used to display event-details dialog
#   format is the same as DF_sections
df_section <- load_section_info(1057)

# list of all shaded events
#   updated automatically on each refresh
#   see update_shaded() function
shaded_events <- c()

# returns html style for each event type
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

# converts event style into suitable format
#   TODO: join these two functions
get_event_style_string <- function(type) {
  paste0("<span style=\"", get_event_style(type)[[1]], "\">", type, "</span>")
}

# converts day and time of the event into standard format
to_POSIX_date <- function(day, time) as.POSIXct(paste(day, time), format = "%A, %B %d, %Y %I:%M %p") 


update_shaded <- function(sel_sections, data_var) {
  shaded_events <<- c()
  # for each selected section
  for(s_section in sel_sections) {
    # extract data, start and end times
    sid_data <- data_var[data_var$section == s_section,]
    sid_start <- to_POSIX_date(sid_data$day, sid_data$start)
    sid_end <- to_POSIX_date(sid_data$day, sid_data$end)
    # skip if they are not defined
    if (length(sid_start) == 0 || is.na(sid_start) ||
        length(sid_end)   == 0 || is.na(sid_end)) {
      next
    }
    # for each event in the current view
    for(ev_section in data_var$section) {
      # do not hide selected event!!!
      if(ev_section == s_section) next;
      # extract data, start and end times
      ev_data = data_var[data_var$section == ev_section,]
      ev_start <- to_POSIX_date(ev_data$day, ev_data$start)
      ev_end <- to_POSIX_date(ev_data$day, ev_data$end)
      # skip if they are not defined
      if (length(ev_start) == 0 || is.na(ev_start) ||
          length(ev_end)   == 0 || is.na(ev_end)) {
        next
      }
      # add the event to shaded if intersects with the current
      if( !( ev_end <= sid_start || ev_start >= sid_end)) {
        shaded_events <<- append(shaded_events, ev_section)
      }
    }
  }
  if(debug_print) {
    cat("shaded_events = ", shaded_events,"\n")
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  introjsUI(),  # Loads required JS/CSS
  tags$head(
      tags$style(HTML("
      html, body {
        overflow-y: scroll !important;
        overflow-x: hidden !important;
        overscroll-behavior: contain;
      }
     .vis-item .vis-item-content {
        white-space: normal !important;
        word-wrap: break-word;
        max-width: 4000px;
      }
    ")),
      
      # JS functions to catch mouse events
      tags$script(HTML("
      Shiny.addCustomMessageHandler('bindDoubleClick', function(id) {
        const el = document.getElementById(id);
        if (!el || !el.timeline) return;
        if (el._dblclickBound) return;
        el._dblclickBound = true;

        // Bind double click
        el.timeline.on('doubleClick', function (props) {
            Shiny.setInputValue(id + '_doubleclick', props, {priority: 'event'});
        });

        // Bind right click (context menu)
        el.timeline.on('contextmenu', function (props) {
            Shiny.setInputValue(id + '_rightclick', props, {priority: 'event'});
            props.event.preventDefault(); // prevent default browser menu
        });
      });
      
      // JavaScript to trigger file input click when action button is pressed
      Shiny.addCustomMessageHandler('triggerUpload', function(message) {
        document.getElementById('file_hidden').click();
      });
    ")),
      
      # style
      tags$style(HTML("
    .vis-timeline {
      font-size: 15px;
    }
   .intro-orphan {
      position: absolute;
      top: 50%;
      left: 50%;
      height: 1px;
      width: 1px;
      overflow: hidden;
      z-index: -1;
    }
  "
    ))
  ), # eng of tags$head

    # Application title
    titlePanel("JSM 2025 Schedule"),
    tags$p(" Click ", 
           tags$a(href="https://ww3.aievolution.com/JSMAnnual2025/Events/pubSearchOptions?style=0", "this link"),
           " to open the site"),
    fluidRow(
      column(2, downloadBttn("save_submit", "Download", style = "simple", color = "primary") %>% tagAppendAttributes(
        `data-intro` = "Press download button to save file",
        `data-step` = 6
      )),
      column(2, 
             # fileInput("upload_schedule", "Upload", accept = "txt") %>% 
             actionBttn(inputId = "upload_btn", 
                        label = "Upload",
                        icon = icon("upload"),
                        style = "simple",
                        color = "primary") %>% 
               tagAppendAttributes(
        `data-intro` = "Press Upload button to load saved file and continue your work",
        `data-step` = 7
      )),
      column(1, actionBttn(
        inputId = "about_btn",
        label = "About",  # or "Options" if you want a label
        icon = icon("info-circle")	,  # Font Awesome 'cog' or 'gear' icon
        style = "simple",  # or "jelly", "simple", "gradient", etc.
        color = "primary"
      )),
      column(2, actionBttn(
        inputId = "tutorial_btn",
        label = "Start tutorial",
        style = "simple", color = "primary"
      ))
    ),
    fluidRow(
      column(4, 
             pickerInput(
               inputId = "selected_day", 
               label = "Select a day", 
               choices = days, 
               selected = days[3], 
               multiple = TRUE,
               options = pickerOptions(
                 actionsBox = TRUE, liveSearch = TRUE
               )
               )  %>% tagAppendAttributes(
                 `data-intro` = "Select a date",
                 `data-step` = 1
               )
             ),
      column(3, textInput("title_search_pattern", "Filter:", "") %>% tagAppendAttributes(
        `data-intro` = "type a text to filter title or section number",
        `data-step` = 2
      )),
      column(1, actionBttn(
        inputId = "options_btn",
        label = NULL,  # or "Options" if you want a label
        icon = icon("gear"),  # Font Awesome 'cog' or 'gear' icon
        style = "material-circle",  # or "jelly", "simple", "gradient", etc.
        color = "primary"
      ) %>% tagAppendAttributes(
        `data-intro` = "Use options to show/hide shaded, selected, unselected events",
        `data-step` = 4
      ))
    ), # end of first row,

    fluidRow(
      column(10, uiOutput("timeline_ui") %>% tagAppendAttributes(
        `data-intro` = "Right click an event to see details or double click to select/unselect it",
        `data-step` = 3
        )), 
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
      ) %>% tagAppendAttributes(
        `data-intro` = "Filter event types here",
        `data-step` = 5
      ))
    ), # end of 2nd row
  tags$div(
    id = "orphan_step",
    class = "intro-orphan",
    `data-intro` = "Thank you very much for watching this tutorial. You are welcome to return here when you need it.",
    `data-step` = 8
  ),
  tags$input(
    id = "file_hidden", type = "file", style = "display: none;"
  )
  ) # end of ui fluid page

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

  # list of the days to show
  #   controlled by the dropbox
  selected_day = reactiveVal( days[4])
  observeEvent(input$selected_day, {
    cat(input$selected_day)
    selected_day(input$selected_day)
  })
  
  # list of the selected event types
  #   controlled by the dropbox
  event_select = reactiveVal(types)
  observeEvent(input$event_select,{
    cat(input$event_select,"\n")
    event_select(input$event_select)
  })
  
  # listed of the selected sections
  #   is updated on the double click
  selected_sections <- reactiveVal( c())
  
  # string to filrer the sections' titles or numbers
  #   controlled by the input field
  title_search_pattern = reactiveVal("fun")
  observeEvent(input$title_search_pattern, {
    cat(input$title_search_pattern)
    title_search_pattern(input$title_search_pattern)
  })

  # active value to force redraw of the timetable vizual
  #   used throught the code
  redraw_trigger <- reactiveVal(0)  # Force UI re-render
  

  # reactive variable that store data.frame with filtered out rows and formatted fields
  data <- reactive({
    if(debug_print) {
      cat("DF_sections")
      str(DF_sections)
    }
    # Filter out day, event type, pattern string
    data_ <<- DF %>% 
      filter(day %in% selected_day()) %>% 
      filter(type %in% event_select()) %>% 
      filter(grepl(tolower(title_search_pattern()), tolower(title)) | grepl(title_search_pattern(), id))
    if(nrow(data_) == 0) {
      return( data_)
    }
    selected_ids <- data_[data_$id %in% selected_sections(),]$id
    if(debug_print) {
      cat("selected_ids = ", selected_ids, "\n")
    }
    # splitting time field and creating popup message
    data_var <- data_  %>% 
      separate_wider_delim(time, delim = " - ", names = c("start", "end")) %>% 
      mutate(section = id, popup = paste0(title, "|", type, "| section: ", id))
    # searching for number of presentors in the event
    data_var$n_presenters <- sapply(data_var$id, function(i) sum(DF_sections$section == i))
    # updating popup, capitalizing selected, wrapping the title, removing unnecessary fields
    data_var <- data_var %>% 
      mutate(popup = paste0(popup, " # = ", n_presenters)) %>% 
      transmute(id = 1:nrow(.), day, start, end, title, type, popup, section) %>% 
      mutate(title = ifelse(section %in% selected_ids, toupper(title), title))
    # %>% 
    #   mutate(title  = gsub("\\n", "<br>", str_wrap(title, width = wrap_width() ))) # Adjust width as needed
    return(data_var)
  })
  
  # reactive variable with final data.frame in TimeViz format
  tv_data <- reactive({
    # updating show flags
    show_shadowed <- is.null(input$show_options) || ("Shadowed" %in% input$show_options)
    show_selected <- is.null(input$show_options) || ("Selected" %in% input$show_options)
    show_nonselected <- is.null(input$show_options) || ("Not Selected" %in% input$show_options)
    # get current version of the data
    data_var  <- data()
    # check for empty
    if(nrow(data_var) == 0) {
      cat("Empty data table\n")
      return(empty_table)
    } else {
      update_shaded( selected_sections(), data_var )
      # do not show events that we do not want to see
      if(! show_shadowed) {
        data_var <- data_var[!(data_var$section %in% shaded_events), ]
      }        
      if(! show_selected) {
        data_var <- data_var[!(data_var$section %in% selected_sections()), ]
      }        
      if(! show_nonselected) {
        data_var <- data_var[data_var$section %in% selected_sections(), ]
      }
      # get IDs of shaded events from session numbers
      shaded_ids <- data_var[data_var$section %in% shaded_events, ]$id
      # transfer data to TimeViz format and shade shaded events
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
      if(debug_print) {
        print("tv_data: shaded_events")
        print(shaded_events)
        print("final_data")
        print(final_data)
        print("End of tv_data")
      }
      return(final_data)
    }
  })

  
  #=============================================
  #=== Updating on redraw trigger and bind back all mouse events
  #=============================================
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
    timevis(tv_data(), options = list(template = NULL)) %>% onRender("
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
  
  
  
  #=============================================
  #=== current event detaild for model dialog
  #=============================================
  modal_table <- reactiveVal(df_section)
  output$datatable_modal <- DT::renderDataTable({
    modal_table()
  })
  
  
  
  #=============================================
  #=== mouse rightclick handler
  #=============================================
  observeEvent(input$timeline_rightclick, {
    data_var <- data()
    if(debug_print) {
      cat("right click\n")
    }
    # getting the ID
    clicked_id <- input$timeline_rightclick$item
    if(debug_print) {
      cat("item ", clicked_id, " is clicked\n")
    }
    # if event is clicked
    if (!is.null(clicked_id)) {
      # get section number
      clicked_section <- data_var[data_var$id == clicked_id,]$section
      # download on scrao section details and update rective variable
      df_section = load_section_info(clicked_section)
      modal_table(df_section)
      # Show the event details dialig box
      if(nrow(df_section)>0) {
        showModal(modalDialog(
          title = "Event Info",
          tags$p(paste0("Section #: ", df_section$section[1])),
          tags$p(paste0("Room: ", df_section$room[1])),
          DT::dataTableOutput("datatable_modal"),
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      }
    }
  })
  
  # left click handler
  #   add or remove clicked event from the selected
  observeEvent(input$timeline_doubleclick, {
    clicked_id <- input$timeline_doubleclick$item
    data_var <- data()
    if(debug_print) {
      cat("item ", clicked_id, " is clicked\n")
      cat("data_var$id[1] = ", data_var$id[1],"\n")
    }
    if (!is.null(clicked_id)) {
      # get session number and ignire if shaded or null
      clicked_section <- data_var[data_var$id == clicked_id,]$section
      if(debug_print) {
        cat("clicked_section = ", clicked_section, "\n")
      }
      if(is.null(clicked_section)) return();
      if(clicked_section %in% shaded_events) {
        return()
      }
      
      # add to selected if not in
      if (!(clicked_section %in% selected_sections())) {
        selected_sections( c(selected_sections(), clicked_section))
        if(debug_print) {
          cat("Adding section ", clicked_section, " selected_sections=", selected_sections(),"\n")
        }
      # or remove from selected if is in
      } else {
        selected_sections( selected_sections()[selected_sections() != clicked_section])
        if(debug_print) {
          cat("Removing section ", clicked_section, " selected_sections=", selected_sections(),"\n")
        }
      }
      # need to redraw
      redraw_trigger(redraw_trigger() + 1)  # Force update
    }
  })
  
  # # print internal info on Info button
  # observeEvent(input$info_btn,{
  #   cat("INFO: \n")
  #   str(data())
  #   cat(" shaded_events = ", shaded_events, "\n")
  #   cat(" selected_sections=", selected_sections(), "\n")
  #   cat("Selected options:", input$show_options, "\n")
  #   print(input$show_options)
  # })
  
  # # resets all data on resets button
  # observeEvent(input$reset_btn, {
  #   cat("Reset button clicked\n")
  #   shaded_events <<- c()
  #   selected_sections(c())
  #   redraw_trigger(redraw_trigger() + 1)  # Force update
  # })
  
  # # redraws time viz on redraw button
  # observeEvent(input$redraw_btn, {
  #   cat("Redraw #", redraw_trigger(), "\n")
  #   redraw_trigger(redraw_trigger() + 1)  # Force update
  # })
 
  observeEvent(input$about_btn,{
    mdd <- readLines("./readme_text.md")
"# This is me
 
 * one
 * Two
"
    # Convert markdown to HTML
    about_text <- HTML(commonmark::markdown_html(mdd))
    
    showModal(modalDialog(
      title = "About",
      div(about_text),
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
  })

  # save shedule to file on Download button
  output$save_submit <- downloadHandler(
    filename = function() {
      "schedule.txt"
    },
    content = function(file) {
      text <- ""
      # for all selected sections
      s_sections <- selected_sections()
      # put total number
      text <- paste(text, paste0(" You have ", length(s_sections), " selected sections"), sep = "\n")
      # Scans through days
      days <- DF[DF$id %in% s_sections,]$day %>% unique
      # and for each day
      for(d in days) {
        # prints it as a subtitle
        text <- paste(text, "\n=========", d, "=========", sep = "\n")
        # and prints each event on separate line
        df <- DF %>% filter(day == d) %>% filter(id %in% s_sections)
        for(i in 1:nrow(df)) {
          line <- paste0(df[i,]$time, ": section ", df[i,]$id," \"", df[i,]$title,"\" /", df[i,]$type, "/")
          text <- paste(text, line, sep="\n")
        }
      }
      cat("text=\n", text, "\n")
      writeLines(text, file)
    }
  )
    

  # Uploads schedule on uplod button
  observeEvent(input$upload_btn, {
    session$sendCustomMessage("triggerUpload", list())
  })
  
  observe({
    input$file_hidden  # Reactively watch for file input
    
    if (!is.null(input$file_hidden)) {
      str(input$file_hidden)
      text <- paste(readLines(input$file_hidden$datapath, warn = FALSE), collapse = "\n")
      if(debug_print) {
        cat("=== text ======\n")
        cat(text, "\n")
      }
      # select only 4-digit numbers
      numbers <- stringr::str_extract_all(text, "\\b\\d{4}\\b")[[1]]
      if(debug_print) {
        cat("-- numbers --\n")
        print(numbers)
      }
      # these numbers are selected sections
      sects <- sapply(numbers, as.integer, USE.NAMES = FALSE)
      if(debug_print) {
        cat("Submitted: ", sects, "\n")
      }
      # store them, and redraw
      selected_sections(sects)
    }
  })

  # view options dialig box
  observeEvent(input$options_btn, {
    cat("options_btn pushed")
    showModal(modalDialog(
      title = "Display Options",
      checkboxGroupButtons(
        inputId = "show_options",
        label = "Show",
        choices = c("Shadowed", "Selected", "Not Selected"),
        selected = c("Shadowed", "Selected", "Not Selected"),
        justified = TRUE,
        checkIcon = list(yes = icon("check"))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  observeEvent(input$tutorial_btn, {
    cat("Tutorial is started")
    introjs(session, options = list(
      "nextLabel" = "Next",
      "prevLabel" = "Back",
      "doneLabel" = "Done"
    ))
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
