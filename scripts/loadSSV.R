ssv_to_rds <- function(inputFile, missionNumber) {

library(tidyverse)
library(lubridate)

missionNum <- sub(".ssv", "", missionNumber)

head <- read.csv(inputFile,
                 sep="", #whitespace as delimiter
                 nrows=1)

raw <- read.csv(inputFile,
                sep="", #whitespace as delimiter
                skip=2,
                header = FALSE)

colnames(raw) <- colnames(head)

raw <- raw %>%
  mutate(m_present_time = as_datetime(m_present_time)) #convert to POSIXct

gps <- raw %>%
  select(m_present_time, m_lat, m_lon)

library(zoo)

full.time <- with(gps,seq(m_present_time[1],tail(m_present_time,1),by=1)) #grab full list of timestamps
gps.zoo <- zoo(gps[2:3], gps$m_present_time) #convert to zoo
result <- na.approx(gps.zoo, xout = full.time) #interpolate

igps <- fortify.zoo(result) %>% #extract out as DF
  rename(i_lat = m_lat) %>%
  rename(i_lon = m_lon) %>%
  rename(m_present_time = Index) %>%
  mutate(m_present_time = as_datetime(m_present_time))
  
#force both time sets to match (i.e., round to 1sec)
igps$m_present_time <- as_datetime(floor(seconds(igps$m_present_time)))
raw$m_present_time <- as_datetime(floor(seconds(raw$m_present_time)))

newGlider <- raw %>%
  left_join(igps) %>%
  mutate(status = if_else(m_avg_depth_rate > 0, "dive", "climb")) %>%
  fill(status) %>%
  #convert from rad to degrees for some vars
  mutate(m_roll = m_roll * 180/pi) %>%
  mutate(m_heading = m_heading * 180/pi) %>%
  mutate(c_heading = c_heading * 180/pi) %>%
  mutate(m_fin = m_fin * 180/pi) %>%
  mutate(c_fin = c_fin * 180/pi) %>%
  mutate(m_pitch = m_pitch * 180/pi)

saveRDS(newGlider, file = paste0("./Data/",missionNum,".rds"))

return(newGlider)

}
