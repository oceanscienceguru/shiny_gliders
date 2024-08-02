library(tidyverse)
library(emayili)

#get deployed gliders
deployedGliders <- read.csv("/echos/processGliders.txt", 
                            sep = "",
                            header = FALSE)
colnames(deployedGliders)[1] = "Name"
colnames(deployedGliders)[2] = "ahrCap"

#only process "real" ones
deployedGliders <- deployedGliders %>%
  filter(!str_starts(Name,"#")) #remove any commented lines

for (i in deployedGliders$Name){
  
  #extract battery capacity as listed
  amps <- deployedGliders %>%
    filter(Name == i)
  
  #pass glidername as parameter from i to each render block
  if (amps$ahrCap > 0){
    msg <- envelope() %>%
      emayili::render("/echos/batteryMarkdown.Rmd", params = list(gliderName = paste0(i))) %>%
      subject(paste0("Daily summary for ", as.character(i)))
    
    capture.output(print(msg, details = TRUE), file = paste0("/echos/", i, "/summary.html"))
  } else {
    msg <- envelope() %>%
      emayili::render("/echos/batteryMarkdownNoAmp.Rmd", params = list(gliderName = paste0(i))) %>%
      subject(paste0("Daily summary for ", as.character(i)))
    
    capture.output(print(msg, details = TRUE), file = paste0("/echos/", i, "/summary.html"))
  }
  
}