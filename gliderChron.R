library(yaml)
library(purrr)
library(dplyr)

source("./gliderLive.R")

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

gliders_live <- list()
for (i in process_gliders_df$glider){
  df <- filter(process_gliders_df, glider == i)

  gliderLive(df$glider, df$ahr_capacity)
}
