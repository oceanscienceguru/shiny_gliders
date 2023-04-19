pseudogram <- function(binSSV, depthSSV){

library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)
#library(echogram)

#test <- palette.echogram(Svthr = -75, Svmax = -35, col.sep = 1, scheme = "echov", visu = FALSE)

raw <- read.csv(binSSV,
                sep="", #whitespace as delimiter
                #skip=2,
                header = FALSE)

rawDepth <- read.csv(depthSSV,
                sep="", #whitespace as delimiter
                skip=17,
                header = FALSE)

#force as dataframe and rename
df <- as.data.frame(raw) %>%
  rename(m_present_time = V1)
#cutoff at seconds for clean merge
df$m_present_time <- as_datetime(floor(seconds(df$m_present_time)))

#tbd time correction
df <- df %>%
  mutate(m_present_time = m_present_time - 20)

#force as dataframe and rename
ef<- as.data.frame(rawDepth) %>%
  rename(m_present_time = V2) %>%
  rename(m_depth = V1)
#cutoff at seconds for clean merge
ef$m_present_time <- as_datetime(floor(seconds(ef$m_present_time)))

#m_depth interpolation (sbd sourced)
full.time <- with(ef,seq(m_present_time[1],tail(m_present_time,1),by=1)) #grab full list of timestamps
depth.zoo <- zoo(ef[1], ef$m_present_time) #convert to zoo
result <- na.approx(depth.zoo, xout = full.time) #interpolate

idepth <- fortify.zoo(result) %>% #extract out as DF
  rename(i_depth = m_depth) %>%
  rename(m_present_time = Index) %>%
  mutate(m_present_time = as_datetime(m_present_time))

#force both time sets to match (i.e., round to 1sec)
idepth$m_present_time <- as_datetime(floor(seconds(idepth$m_present_time)))

# SSV output for testing
# output <- idepth
# output$m_present_time <- as.numeric(output$m_present_time)
# write.table(output,
#             file =  "test.ssv",
#             quote = FALSE,
#             row.names = FALSE,
#             sep = " ")

#merge interpolated depth
bigLong <- pivot_longer(df, !c(m_present_time)) %>%
  left_join(idepth, by = "m_present_time")

#build ping depth matrix
binList <- unique(bigLong$name)
binOffset <- data.frame(name = c(binList),
                   offset = c(seq(0, (length(binList)-1)*2.5, 2.5)))

#merge in ping depths and compute
bigLong <- as.data.frame(bigLong %>%
  left_join(binOffset, by = "name") %>%
  mutate(p_depth = i_depth + offset)) %>%
  mutate(q_depth = round(p_depth, 1))

return(bigLong)
}
