gliderGPS_to_dd <- function(gliderGPS) {
  
  library(tidyverse)
  
  df <- data.frame(gps = as.character(gliderGPS))
  
  dd <- df %>%
    separate(gps, paste0("gpss", c("d","m")), sep="\\.", remove = FALSE) %>% #have to double escape to sep by period
    mutate(gpsd = substr(gpssd, 1, nchar(gpssd)-2)) %>% #pull out degrees
    mutate(gpsm = paste0(str_sub(gpssd, start= -2),".", gpssm)) %>% #pull out minutes
    mutate(across(gpsd:gpsm, as.numeric)) %>% #coerce back to numeric
    # mutate(gpsd = format(gpsd, nsmall = 0),
    #        gpsm = format(gpsm, nsmall = 4)) %>% #coerce to character keeping zeroes
    mutate(gpsdd = ifelse(sign(gpsd) == -1, (abs(gpsd) + (gpsm/60))*-1, (abs(gpsd) + (gpsm/60)))) # check if neg and multiply by -1 if needed
    
  return(dd$gpsdd)
}