gotoLoad <- function(inFile){

#### parse goto_l*.ma 

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
  mutate(cleanNums = str_trim(nums)) %>% #clean up both sides
  separate(cleanNums, into = c("rawlong", "rawlat"), sep = "\\s") %>% #break into 2
  # mutate(latt = format(rawlat, nsmall = 4),
  #        longg = format(rawlong, nsmall = 4)) %>% #coerce to character keeping zeroes out to 4 decimals
  separate(rawlat, paste0("latt",c("d","m")), sep="\\.", remove = FALSE) %>% #have to double escape to sep by period
  separate(rawlong, paste0("longg",c("d","m")), sep="\\.", remove = FALSE) %>%
  mutate(latd = substr(lattd, 1, nchar(lattd)-2), #pull out degrees
         longd = substr(longgd, 1, nchar(longgd)-2)) %>%
  mutate(latm = paste0(str_sub(lattd, start= -2),".",lattm), #pull out minutes
         longm = paste0(str_sub(longgd, start= -2),".",longgm)) %>%
  mutate(across(latd:longm, as.numeric)) %>% #coerce back to numeric
  mutate(lat = latd + (latm/60),
         long = (abs(longd) + (longm/60))*-1) %>% #*-1 for western hemisphere
  mutate(rad = ifelse(!is_empty(rads), rads, 10)) %>% # pull in parsed radius if present. otherwise masterdata default
  mutate(order = rownames(.)) #get order of wpts as listed
  
return(wptsRaw)

}
