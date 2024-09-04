library(tidyverse)
library(ggplot2)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(ggtext)
library(sf)
#library(PlotSvalbard) #devtools::install_github("MikkoVihtakari/PlotSvalbard", upgrade = "never")
#library(patchwork)
#library(cowplot)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux?
library(scales)
library(seacarb)
library(shinycssloaders)
library(shinydashboard)
library(slickR)
library(svglite)
library(lubridate)
library(egg)
library(shinyWidgets)
library(shinyglide)
library(ggiraph)
library(cmocean)
library(osgUtils)
library(plotly)
library(rjson)
library(shinyjs)
library(quarto)

#source all modules and all scripts within the app structure
file.sources = list.files(c("./modules", "./scripts"), 
                          pattern="*.R$", full.names=TRUE, 
                          ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)

#cache testing
shinyOptions(cache = cachem::cache_disk("/echos/temp", max_size = 500e6))

#maximum file upload size of 3Gb
options(shiny.maxRequestSize = 3000*1024^2)

#make sure serve is UTC
Sys.setenv(TZ="UTC")

#pull sensors and units
sensor_defs <- fromJSON(file = "https://github.com/kerfoot/gncutils/raw/master/resources/sensor-def-masters/slocum-sensor_defs.json"
)

#read server directory structure
server_metadata <- read.csv("./server_metadata.txt",
                            sep = "",
                            header = FALSE)

# EXAMPLE SERVER_METADATA.TXT FILE
#
# servDir "/echos"
# liveDir "/echos"
# rawDir  "/gliders"
# fullDir "/echos/brewdata"
#
# servDir is where to find various metadata files (deployed and glider fleet)
# liveDir is where to find the structure for incoming live data
# rawDir  is where to look for the rsync'd copy of all raw data including flight files
# fullDir is where to find the structure for full datasets post mission

servDir <- server_metadata[which(server_metadata == "servDir"),2]
liveDir <- server_metadata[which(server_metadata == "liveDir"),2]
rawDir <- server_metadata[which(server_metadata == "rawDir"),2]
fullDir <- server_metadata[which(server_metadata == "fullDir"),2]
app_name <- server_metadata[which(server_metadata == "app_name"),2]

#read in which gliders to display as live data
deployedGliders <- read.csv(paste0(servDir, "/deployedGliders.txt"),
                            sep = "",
                            header = FALSE)

colnames(deployedGliders)[1] = "Name"
colnames(deployedGliders)[2] = "ahrCap"

deployedGliders <- deployedGliders %>%
  filter(!str_starts(Name,"#")) #remove any commented lines

#all possible gliders in the fleet
fleetGliders <- read.csv(paste0(servDir, "/fleetGliders.txt"),
                         sep = "",
                         header = FALSE) %>%
  arrange(V1)

#if routes directory exists with example goto files, load them
routesList_files <- file.info(list.files(path = paste0(servDir, "/routes"),
                                         pattern = "*.ma"))

routesList_files$names <- rownames(routesList_files)

#build route list
routesList <- list()
for (i in routesList_files$names) {
  routesList[[i]] <- gotoLoad(paste0(servDir, "/routes/", i))
}

#make some icons for maps
icon.start <- makeAwesomeIcon(
  icon = "flag", markerColor = "green",
  library = "fa",
  iconColor = "black"
)

icon.end <- makeAwesomeIcon(
  icon = "flag", markerColor = "red",
  library = "fa",
  iconColor = "black"
)

icon.latest <- makeAwesomeIcon(
  icon = "flag", markerColor = "purple",
  library = "fa",
  iconColor = "black"
)

#https://rdrr.io/github/AustralianAntarcticDivision/ZooScatR/src/R/soundvelocity.R
c_Coppens1981 <- function(D,S,T){
  t <- T/10
  D = D/1000
  c0 <- 1449.05 + 45.7*t - 5.21*(t^2)  + 0.23*(t^3)  + (1.333 - 0.126*t + 0.009*(t^2)) * (S - 35)
  c <- c0 + (16.23 + 0.253*t)*D + (0.213-0.1*t)*(D^2)  + (0.016 + 0.0002*(S-35))*(S- 35)*t*D
  return(c)
}

#https://rdrr.io/cran/wql/man/ec2pss.html
ec2pss <-
  function (ec, t, p = 0) {
    # Define conductivity ratio
    R <- ec/42.914

    # Estimate temperature correction (valid for -2 < t < 35)
    c <- c(0.6766097, 0.0200564, 0.0001104259, -6.9698e-07, 1.0031e-09)
    rt <- c[1] + c[2] * t + c[3] * t^2 + c[4] * t^3 + c[5] * t^4

    # Estimate pressure correction (validity range varies with t and S)
    d <- c(0.03426, 0.0004464, 0.4215, -0.003107)
    e <- c(2.07e-05, -6.37e-10, 3.989e-15)
    Rp <- 1 + p * (e[1] + e[2] * p + e[3] * p^2)/(1 + d[1] * t +
                                                    d[2] * t^2 + (d[3] + d[4] * t) * R)

    # Estimate salinity (valid for 2 < S < 42 and -2 < t < 35).
    Rt <- R/(Rp * rt)
    a <- c(0.008, -0.1692, 25.3851, 14.0941, -7.0261, 2.7081)
    b <- c(5e-04, -0.0056, -0.0066, -0.0375, 0.0636, -0.0144)
    ft <- (t - 15)/(1 + 0.0162 * (t - 15))
    S <- a[1] + a[2] * Rt^0.5 + a[3] * Rt + a[4] * Rt^1.5 + a[5] *
      Rt^2 + a[6] * Rt^2.5 + ft * (b[1] + b[2] * Rt^0.5 + b[3] *
                                     Rt + b[4] * Rt^1.5 + b[5] * Rt^2 + b[6] * Rt^2.5)

    # Estimate salinity correction for S < 2
    x <- 400 * Rt
    y <- 100 * Rt
    ifelse(S >= 2, S, S - a[1]/(1 + 1.5 * x + x^2) - b[1] * ft/(1 +
                                                                  y^0.5 + y + y^1.5))
  }
