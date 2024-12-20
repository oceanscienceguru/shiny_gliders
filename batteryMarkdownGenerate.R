library(tidyverse)
library(emayili)
library(yaml)
library(purrr)
library(dplyr)

# Read and process YAML data to create a dataframe of gliders where 'process' is TRUE
get_process_gliders <- function(config_path = "./app/config.yaml") {
  # Load YAML data
  yaml_data <- read_yaml(config_path)
  gliders_data <- yaml_data$gliders
  
  # Ensure gliders_data is a list
  if (!is.null(gliders_data) && is.list(gliders_data)) {
    # Filter gliders where 'process' is TRUE
    process_gliders <- gliders_data %>%
      keep(~ isTRUE(.x$process)) %>%
      imap_dfr(~ {
        # Combine the glider name with the rest of the data
        tibble(glider = .y, !!!.x)
      })
    
    return(process_gliders)
  } else {
    warning("No valid glider data found in the YAML file.")
    return(tibble())  # Return empty tibble instead of data.frame
  }
}

# Example usage
process_gliders_df <- get_process_gliders("/srv/shiny-server/thebrewery/config.yaml")
#process_gliders_df <- get_process_gliders("./app/config.yaml")

for (i in process_gliders_df$glider){

  #extract battery capacity as listed
  amps <- process_gliders_df %>%
    filter(glider == i)

  #pass glidername as parameter from i to each render block
  if (amps$ahr_capacity > 0){
    msg <- envelope() %>%
      emayili::render("./Documentation/batteryMarkdown.Rmd", params = list(gliderName = paste0(i))) %>%
      subject(paste0("Daily summary for ", as.character(i)))

    capture.output(print(msg, details = TRUE), file = paste0("/echos/", i, "/summary.html"))
  } else {
    msg <- envelope() %>%
      emayili::render("./Documentation/batteryMarkdownNoAmp.Rmd", params = list(gliderName = paste0(i))) %>%
      subject(paste0("Daily summary for ", as.character(i)))

    capture.output(print(msg, details = TRUE), file = paste0("/echos/", i, "/summary.html"))
  }

}
