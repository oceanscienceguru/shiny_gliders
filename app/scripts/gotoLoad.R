gotoLoad <- function(inFile){

#### parse goto_l*.ma 

source("./scripts/gliderGPS_to_dd.R")
library(tidyverse)

rawMa <- read.delim(inFile, sep = "\n")
rawMa <- as.data.frame(rawMa)
colnames(rawMa)[1] = "raw"

#get row indices for waypoint list block
first <- which(rawMa == "<start:waypoints>")
last  <- which(rawMa == "<end:waypoints>")

#find radius
radInd <- which(str_detect(rawMa$raw, "b_arg: list_when_wpt_dist"))
radBarg <- rawMa[radInd,1]
  rads <- ifelse(str_detect(radBarg, ".*(?=#)"),str_extract(radBarg, ".*(?=#)"), radBarg) %>% #strip off any comments at end
    str_trim() %>%
    parse_number()
  
wptsRaw <- rawMa %>%
  slice((first+1):(last-1)) %>% #pull out all the wpts
  filter(!str_starts(raw,"#")) %>% #remove any commented lines
  mutate(nums = ifelse(str_detect(raw, ".*(?=#)"),str_extract(raw, ".*(?=#)"), raw)) %>% #strip off any comments at end
  mutate(comment = str_trim(ifelse(str_detect(raw, "(?=#).*"),str_extract(raw, "(?=#).*"), ""))) %>% #extract any comments from end
  mutate(cleanNums = str_trim(nums)) %>% #clean up both sides
  separate_wider_delim(cleanNums, delim = " ", names = c("rawlong", "rawlat"), too_many = "merge") %>% #break into 2
#convert to decimal degrees using osg function
  mutate(lat = gliderGPS_to_dd(rawlat),
         long = gliderGPS_to_dd(rawlong)) %>%
  mutate(rad = ifelse(!is_empty(rads), rads, 10)) %>% # pull in parsed radius if present. otherwise masterdata default
  mutate(order = rownames(.)) #get order of wpts as listed

  
return(wptsRaw)

}
