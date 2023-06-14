ssv_to_rds <- function(inputFile, missionNumber, gliderName) {

library(tidyverse)
library(lubridate)
  library(seacarb)

  source("./scripts/gliderGPS_to_dd.R")
  
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
  select(m_present_time, m_gps_lat, m_gps_lon) %>%
  mutate(lat = gliderGPS_to_dd(m_gps_lat),
         long = gliderGPS_to_dd(m_gps_lon)) %>%
  select(m_present_time, lat, long)

library(zoo)

full.time <- with(gps,seq(m_present_time[1],tail(m_present_time,1),by=1)) #grab full list of timestamps
gps.zoo <- zoo(gps[2:3], gps$m_present_time) #convert to zoo
result <- na.approx(gps.zoo, xout = full.time) #interpolate

igps <- fortify.zoo(result) %>% #extract out as DF
  rename(i_lat = lat) %>%
  rename(i_lon = long) %>%
  rename(m_present_time = Index) %>%
  mutate(m_present_time = as_datetime(m_present_time))

#force both time sets to match (i.e., round to 1sec)
igps$m_present_time <- as_datetime(floor(seconds(igps$m_present_time)))
raw$m_present_time <- as_datetime(floor(seconds(raw$m_present_time)))

gliderdf <- raw %>%
  left_join(igps) %>%
  mutate(status = if_else(m_avg_depth_rate > 0, "dive", "climb")) %>%
  fill(status) %>%
  #convert from rad to degrees for some vars
  mutate(m_roll = m_roll * 180/pi) %>%
  mutate(m_heading = m_heading * 180/pi) %>%
  mutate(c_heading = c_heading * 180/pi) %>%
  mutate(m_fin = m_fin * 180/pi) %>%
  mutate(c_fin = c_fin * 180/pi) %>%
  mutate(m_pitch = m_pitch * 180/pi) %>%
  mutate(osg_salinity = ec2pss(sci_water_cond*10, sci_water_temp, sci_water_pressure*10)) %>%
  mutate(osg_theta = theta(osg_salinity, sci_water_temp, sci_water_pressure)) %>%
  mutate(osg_rho = rho(osg_salinity, osg_theta, sci_water_pressure)) %>%
  mutate(osg_depth = p2d(sci_water_pressure*10, lat=30)) %>%
  mutate(osg_soundvel1 = c_Coppens1981(osg_depth,
                                       osg_salinity,
                                       sci_water_temp))

save(gliderdf, gliderName, file = paste0("./Data/",gliderName,"_",missionNum,".RData"))

}
