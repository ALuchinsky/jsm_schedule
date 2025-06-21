suppressPackageStartupMessages(library(rvest))

load_section_info <- function(id, prefix = "https://ww3.aievolution.com/JSMAnnual2025/Events/viewEv?ev=") {
  file_name <-  paste0("./events/", id, ".csv")
  if(file.exists(file_name)) {
    return( read.csv(file_name))
  }
  cat("webscraping info")
  html <- read_html(paste0(prefix, id))
  room <- html %>% html_elements("#eventPreview_roomAssignments") %>% html_text2()
  room <- trimws(strsplit(room, ":")[[1]][2])
  titles <- html %>% html_elements(".presentation-title")
  speakers <- html %>% html_elements(".presesntaionspeakers")
  char_speakers <- c()
  for(i in 1:length(speakers)) {
    char_speakers <- append(char_speakers, as.character(speakers[i] %>% html_element(xpath = "..")))
  }
  html_speakers <- lapply(unique(char_speakers), read_html)
  DF <- data.frame()
  for(i in 1:length(titles)) {
    title = gsub("\\r","", html_text2(titles[i]) )
    speakers <- html_speakers[[i]] %>% html_elements(".speakername") %>% html_text2() %>% unique %>% paste(collapse = ", ")
    df_ <- data.frame(
      section = id, 
      title = trimws(title),
      speakers = trimws(speakers),
      room = room
    )
    DF <- bind_rows(DF, df_)
  }
  write.csv(DF, file_name, row.names = FALSE)
  return(DF)
}
