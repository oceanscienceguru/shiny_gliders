depthInt <- function(inGliderdf){

library(tidyverse)
library(lubridate)
library(zoo)
#library(echogram)

#test <- palette.echogram(Svthr = -75, Svmax = -35, col.sep = 1, scheme = "echov", visu = FALSE)

ef <- inGliderdf %>%
  select(c(m_present_time, m_depth))

#coerce as dataframe
ef <- as.data.frame(ef) %>%
  arrange(m_present_time) #ensure chronological order

#cutoff at seconds
ef$m_present_time <- as_datetime(floor(seconds(ef$m_present_time)))

#m_depth interpolation
full.time <- with(ef,seq(m_present_time[1],tail(m_present_time,1),by=1)) #grab full list of timestamps
depth.zoo <- zoo(ef$m_depth, ef$m_present_time) #convert to zoo
result <- na.approx(depth.zoo, xout = full.time) #interpolate

idepth <- fortify.zoo(result) %>% #extract out as DF
  rename(osg_i_depth = result) %>%
  rename(m_present_time = Index) %>%
  mutate(m_present_time = as_datetime(m_present_time))

#force both time sets to match (i.e., round to 1sec)
idepth$m_present_time <- as_datetime(floor(seconds(idepth$m_present_time)))

return(idepth)
}
