ssv_to_df <- function(inputFile, headerSize = 14) {
  
  library(tidyverse)
  library(lubridate)
  
  # if(is.null(headerSize))
  #   headerSize <- 14
  
  # inputFile <- "/echos/science/usf-stella-2023-059-1-308.ssv"
  # inputFile <- "/echos/science/usf-stella-2023-059-1-309.ssv"
  
  top <- read.csv(inputFile,
                   sep="", #whitespace as delimiter
                   nrows=headerSize
                   )
  
  head <- read.csv(inputFile,
                  sep="", #whitespace as delimiter
                  skip=headerSize,
                  nrows=1,
                  header = TRUE)
  
  units <- read.csv(inputFile,
                    sep="", #whitespace as delimiter
                    skip=headerSize,
                    nrows=1,
                    header = TRUE)
  
  raw <- read.csv(inputFile,
                   sep="", #whitespace as delimiter
                   skip=headerSize,
                   header = TRUE)
  
  vars <- colnames(head)
  
  N <- 2
  raw <- tail(raw, -N)
  
  newGlider <- raw %>%
    mutate(across(everything(), ~ as.numeric(.)))
    
  if ("m_present_time" %in% vars){
    newGlider$m_present_time <- as_datetime(newGlider$m_present_time) #convert to POSIXct
    
    if (nrow(newGlider) > 0){
    newGlider$m_present_time <- as_datetime(floor(seconds(newGlider$m_present_time))) #round to seconds
    }
    
  } else if ("sci_m_present_time" %in% vars){ 
    newGlider$sci_m_present_time <- as_datetime(newGlider$sci_m_present_time) #convert to POSIXct
    
    if (nrow(newGlider) > 0){
    newGlider$sci_m_present_time <- as_datetime(floor(seconds(newGlider$sci_m_present_time))) #round to seconds
    }
    
  }

  
  return(newGlider)
  
}
