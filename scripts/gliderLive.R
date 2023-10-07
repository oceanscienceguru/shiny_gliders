gliderLive <- function(gliderName, ahrCap){

library(tidyverse)
library(seacarb)
library(svglite)
library(egg)
library(lubridate)
library(ggplot2)
library(scales)
library(osgUtils)
    
  load("/echos/G1AlkalineCurve.RData")

# source("/srv/shiny-server/thebrewery/scripts/ssv_to_df.R")
# source("/srv/shiny-server/thebrewery/scripts/pseudogram.R")
# source("/srv/shiny-server/thebrewery/scripts/depthInt.R")
# source("/srv/shiny-server/thebrewery/scripts/gliderGPS_to_dd.R")
# source("/srv/shiny-server/thebrewery/scripts/identify_casts.R")
# source("/srv/shiny-server/thebrewery/scripts/identify_casts_smooth.R")
# source("/srv/shiny-server/thebrewery/scripts/add_yo_id.R")
  
message(paste0(gliderName, ", ", ahrCap, "ahr capacity"))

  
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

scienceList_liveInfo <- file.info(list.files(path = paste0("/echos/gliders/", gliderName, "/science/"),
                                             full.names = TRUE)) %>%
  filter(size > 0)

scienceList_live <- rownames(scienceList_liveInfo) %>%
  basename()

flightList_liveInfo <- file.info(list.files(path = paste0("/echos/gliders/", gliderName, "/flight/"),
                                            full.names = TRUE)) %>%
  filter(size > 0)

flightList_live <- rownames(flightList_liveInfo) %>%
  basename()

flightTotal <- length(flightList_live)
#if flightList changed ... then ... do df creation

message("Checking if the data have changed")

#write new file if the list doesn't exist at all and set flag to FALSE
if (!file.exists(paste0("/echos/", gliderName, "/scienceList.csv"))){
  sciCheck <- FALSE
} else {
  #if it does exist, read it and compare it, setting check value TRUE or FALSE
  sciencePickle <- readLines(paste0("/echos/", gliderName, "/scienceList.csv"))
  sciCheck <- length(intersect(sciencePickle, scienceList_live)) == length(scienceList_live)
}

#same for flight
if (!file.exists(paste0("/echos/", gliderName, "/flightList.csv"))){
  fliCheck <- FALSE
} else {
  flightPickle <- readLines(paste0("/echos/", gliderName, "/flightList.csv"))
  fliCheck <- length(intersect(flightPickle, flightList_live)) == length(flightList_live)
}

#use check values to proceed
if (!isTRUE(sciCheck) | !isTRUE(fliCheck)){
  message("Data have changed, building new RData file")
  writeLines(scienceList_live, 
             paste0("/echos/", gliderName, "/scienceList.csv"))
  writeLines(flightList_live, 
             paste0("/echos/", gliderName, "/flightList.csv"))

message("Assembling raw dataframe")
flist <- list()
slist <- list()
for (i in flightList_live) {
  flist[[i]] <- ssv_to_df(paste0("/echos/gliders/", gliderName, "/flight/", i))
}
for (j in scienceList_live) {
  slist[[j]] <- ssv_to_df(paste0("/echos/gliders/", gliderName, "/science/", j))
}

fdf <- bind_rows(flist, .id = "segment")

sdf <- bind_rows(slist, .id = "segment") %>%
  mutate(m_present_time = sci_m_present_time) #consider sci time same as flight time for ease of merging

message("osg water calculations")
gliderdfraw <- fdf %>%
  select(!c(segment)) %>% #temporarily remove segment
  full_join(sdf, relationship = "many-to-many") %>% #merge in sci data and get segment back
  arrange(m_present_time) %>% #ensure chronological order
  fill(segment, .direction = "downup") %>% #fill out segment ID
  distinct(m_present_time, .keep_all = TRUE) %>% #remove duplicate m_present_time rows (usually bad sci file)
  #calculate several osg* variables for plotting
  mutate(osg_salinity = ec2pss(sci_water_cond*10, sci_water_temp, sci_water_pressure*10)) %>%
  mutate(osg_theta = theta(osg_salinity, sci_water_temp, sci_water_pressure)) %>%
  mutate(osg_rho = rho(osg_salinity, osg_theta, sci_water_pressure)) %>%
  mutate(osg_depth = p2d(sci_water_pressure*10, lat=30)) %>%
  mutate(osg_soundvel1 = c_Coppens1981(osg_depth,
                                       osg_salinity,
                                       sci_water_temp))

message("Depth interpolation")
#interpolate across depth
idepth <- depthInt(gliderdfraw, CTD = TRUE)

message("GPS interpolation")
#interpolate across gps
#lots of GPS massaging
gps <- gliderdfraw %>%
  select(m_present_time, m_gps_lat, m_gps_lon) %>%
  filter(!is.na(m_gps_lat)) %>% #clean up input for conversion
  mutate(latt = format(m_gps_lat, nsmall = 4),
         longg = format(m_gps_lon, nsmall = 4)) %>% #coerce to character keeping zeroes out to 4 decimals
  mutate(lat = gliderGPS_to_dd(latt),
         long = gliderGPS_to_dd(longg)) %>%
  # mutate(lat = gliderGPS_to_dd(m_gps_lat),
  #        long = gliderGPS_to_dd(m_gps_lon)) %>%
  select(m_present_time, lat, long) %>%
  filter(lat >= -90 & lat <= 90) %>% #remove illegal values
  filter(long >= -180 & long <= 180)

library(zoo)

full.time <- with(gps,seq(m_present_time[1],tail(m_present_time,1),by=1)) #grab full list of timestamps
gps.zoo <- zoo(gps[2:3], gps$m_present_time) #convert to zoo
result <- na.approx(gps.zoo, xout = full.time) #interpolate

igps <- fortify.zoo(result) %>% #extract out as DF
  rename(i_lat = lat) %>%
  rename(i_lon = long) %>%
  rename(m_present_time = Index) %>%
  mutate(m_present_time = as_datetime(m_present_time))

#join the interpolations back in
gliderdfInt <- gliderdfraw %>%
  left_join(idepth) %>%
  left_join(igps)

message("Glider state algorithms")
#glider state algorithms
gliderState <- gliderdfInt %>%
  #filter(m_present_time %within% time) %>%
  #identify_casts(surface_threshold = 1) %>%
  identify_casts_smooth(surface_threshold = 1, rolling_window_size = 4) %>% #first cast identification pass with "surface" threshold
  filter(cast != "Surface" & cast != "Unknown") %>% #strip out surface/unknown for yo ID
  add_yo_id() %>%
  full_join(gliderdfInt) %>% #rejoin with full set to get surface/unknown sections back
  arrange(m_present_time) %>% #ensure chronological order
  identify_casts_smooth(surface_threshold = 1, rolling_window_size = 4) %>% 
  #identify_casts(surface_threshold = 1) %>% #label cast state again
  select(c(m_present_time, cast, yo_id)) #clean up

#join in gliderState
gliderdf <- gliderdfInt %>%
  left_join(gliderState)

message("Extracting variables")
#pull out science variables
scivarsLive <- sdf %>%
  select(!(c(segment, m_present_time))) %>%
  colnames()

#pull out flight variables
flightvarsLive <- fdf %>%
  select(!(c(segment))) %>%
  #select(!starts_with("sci")) %>%
  colnames()

#get oldest timestamp in dataset for use in calculations
endDateLive <- max(gliderdf$m_present_time)

##### dashboard calculations #####
message("dashboard calculations")
gliderdfChunk <- gliderdf %>%
  filter(m_present_time >= endDateLive-14400)

if (ahrCap > 0){
ahrUsed <- max(gliderdf$m_coulomb_amphr_total, na.rm = TRUE)
ahrLeft <- as.numeric(ahrCap-ahrUsed)
pwr3day <- gliderdf %>%
  select(m_present_time, m_coulomb_amphr_total) %>%
  filter(m_present_time >= endDateLive - 259200,
         m_coulomb_amphr_total > 0)
pwr1day <- pwr3day %>%
  filter(m_present_time >= endDateLive - 86400)

ahr3day <- (max(pwr3day$m_coulomb_amphr_total)-min(pwr3day$m_coulomb_amphr_total))/(as.numeric(max(pwr3day$m_present_time))-as.numeric(min(pwr3day$m_present_time)))*86400
ahr1day <- (max(pwr1day$m_coulomb_amphr_total)-min(pwr1day$m_coulomb_amphr_total))/(as.numeric(max(pwr1day$m_present_time))-as.numeric(min(pwr1day$m_present_time)))*86400
ahrAllday <- (max(gliderdf$m_coulomb_amphr_total, na.rm = TRUE)-min(gliderdf$m_coulomb_amphr_total, na.rm = TRUE))/(as.numeric(max(gliderdf$m_present_time, na.rm = TRUE))-as.numeric(min(gliderdf$m_present_time, na.rm = TRUE)))*86400

battLeft <- as.numeric((ahrLeft/ahrCap)*100)

#coulombs daily use
coulombs <- gliderdf %>%
  select(c(m_present_time, m_coulomb_amphr_total)) %>%
  filter(m_coulomb_amphr_total > 0) %>%
  mutate(day = floor_date(m_present_time,
                          unit = "days")) %>%
  group_by(day) %>%
  mutate(dailyAhr = (max(m_coulomb_amphr_total)-min(m_coulomb_amphr_total))/(as.numeric(max(m_present_time))-as.numeric(min(m_present_time)))*86400) %>%
  #select(c(day, meanBatt)) %>%
  distinct(day, dailyAhr)

couLive <- ggplot(
  data = 
    coulombs,
  aes(x=day,
      y=dailyAhr,
  )) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(title = "Daily Power Usage",
       y = "Ahrs",
       x = "Date") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))
} else {
  
  #calculate mean battery voltage daily
  meanVolt <- gliderdf %>%
    select(c(m_present_time, m_battery)) %>%
    filter(m_battery > 0) %>%
    mutate(days = yday(m_present_time)) %>% #may break if deployment crosses to new year
    mutate(shiftDay = (days - min(days)) + 1) %>%
    group_by(shiftDay) %>%
    mutate(battAvg = mean(m_battery)) %>%
    select(shiftDay, battAvg) %>%
    distinct()
  
  #find "deployment day" based on reference curve
  refDay <- reference %>%
    ungroup() %>%
    slice(which.min(abs(reference$avg - max(meanVolt$battAvg))))
  
  #shift for plotting
  battShift <- meanVolt %>%
    mutate(depDay = (shiftDay + refDay$shiftDay)-1)
  
  #plot shifted days against ref curve
  couLive <- ggplot() +
    geom_point(data = battShift, 
               size = 2, aes(x = depDay, y = battAvg), color = "red") +
    geom_line(data = reference, aes(x = shiftDay, y = avg), color = "blue") +
    labs(title = "Daily Voltage vs. Curve",
         caption = "Reference curve calculated from usf-bass M86 and M109",
         x = "Days Deployed", 
         y = "Battery (V)") +
    theme_bw() +
    theme(plot.title = element_text(size = 32),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 16))
  
  #find day on curve that matches lowest daily voltage average
  battLeft <- as.numeric(reference %>%
                           ungroup() %>%
                           slice(which.min(abs(reference$avg - min(meanVolt$battAvg)))) %>%
                           select(daysLeft))
}

###### to glider file list ########
toGliderListInfo <- file.info(list.files(path = paste0("/gliders/gliders/", gliderName, "/archive/"),
                                     full.names = TRUE)) %>%
  filter(size > 0)


toGliderList <- basename(rownames(toGliderListInfo)) %>%
  as.data.frame() %>%
  rename(fileName = 1)

#### plots for carousel ####
#get daily battery average
bats <- gliderdf %>%
  select(c(m_present_time, m_battery)) %>%
  filter(m_battery > 0) %>%
  mutate(day = floor_date(m_present_time,
                          unit = "days")) %>%
  group_by(day) %>%
  mutate(meanBatt = mean(m_battery)) %>%
  #select(c(day, meanBatt)) %>%
  distinct(day, meanBatt)

battLive <- ggplot(
  data = 
    bats,
  aes(x=day,
      y=meanBatt,
  )) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(title = "Daily Voltage Average",
       y = "Battery (V)",
       x = "Date") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))

#label all LD vars
LDvars <- c("m_leakdetect_voltage", "m_leakdetect_voltage_forward", "m_leakdetect_voltage_science")

leaks <- gliderdf %>%
  select(c(m_present_time, any_of(LDvars))) %>%
  filter(m_leakdetect_voltage > 0) %>%
  filter(m_present_time >= endDateLive-14400) %>%
  pivot_longer(cols = any_of(LDvars))

LDmin <- min(leaks$value, na.rm = TRUE)

leakLive <- ggplot(
  data = 
    leaks,
  aes(x=m_present_time,
      y=value,
      color = name
  )) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  theme_bw() +
  labs(title = "LD last 4hrs",
       y = "Voltage",
       x = "Date") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.position   =  "bottom")

#get daily roll averages
roll <- gliderdf %>%
  select(c(m_present_time, m_roll)) %>%
  filter(!is.na(m_roll)) %>%
  mutate(day = floor_date(m_present_time,
                          unit = "days")) %>%
  group_by(day) %>%
  mutate(meanRoll = mean(m_roll)) %>%
  #select(c(day, meanBatt)) %>%
  distinct(day, meanRoll)

rollLive <- ggplot(
  data = 
    roll,
  aes(x=day,
      y=meanRoll*180/pi,
  )) +
  geom_point(
    size = 2,
    na.rm = TRUE
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(limits = symmetric_range) +
  theme_bw() +
  labs(title = "Daily Roll Average",
       y = "Roll (deg)",
       x = "Date") +
  theme(plot.title = element_text(size = 32),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16))



if(gliderName == "usf-stella"){

###### pseudogram calculations #####
message("pseudogram calculations")
#initial file list setup ... ensure files have data before allowing
velInfo <- file.info(list.files(path = "/echos/layers/",
                                full.names = TRUE)) %>%
  filter(size > 0)

velList <- rownames(velInfo) %>%
  basename()

depthInfo <- file.info(list.files(path = "/echos/depths/",
                                  full.names = TRUE))

depthList <- rownames(depthInfo) %>%
  basename()

echoListraw <- intersect(velList, depthList) %>% #keep only files that are matched depths/layers sets
  str_remove(pattern = ".ssv") %>%
  enframe() %>%
  mutate(fileID = str_remove(value, paste0(gliderName, "-"))) %>% #strip off glider name
  separate(fileID, into = c("Year", "Day", "mNum", "fileNum"), sep = "-") %>% #separate filename components
  mutate(fileNum = str_pad(fileNum, 4, side = "left", pad = "0")) %>% #pad the file number for easy sort
  arrange(Year, Day, mNum, fileNum) %>% #sort in order
  select(!c(name)) #drop column

# process into long format for plotting
ehunk <- pseudogram(paste0("/echos/layers/", tail(echoListraw$value, 1), ".ssv"),
                      paste0("/echos/depths/", tail(echoListraw$value, 1), ".ssv"))


#color palette source:
#https://rdrr.io/github/hvillalo/echogram/src/R/palette.echogram.R

#plotitup
ggEcho <-
    ggplot(data = 
             ehunk,
           aes(x=m_present_time,
               y=p_depth,
               z=value)) +
    geom_point(
      aes(color = value),
      size = 6,
      pch = 15,
      na.rm = TRUE
    ) +
  scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
                                     "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
                         limits = c(-75, -30)) +
    scale_y_reverse() +
    theme_bw() +
    labs(title = "Latest Pseudogram",
         y = "Depth (m)",
         x = "Date/Time (UTC)",
         colour = "dB") +
    theme(plot.title = element_text(size = 32),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.key = element_blank()) +
    guides(size="none") +
    scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))

#### pseudotimegram setup ####
elist <- list()
for (i in echoListraw$value) {
  elist[[i]] <- pseudogram(paste0("/echos/layers/", i, ".ssv"),
                           paste0("/echos/depths/", i, ".ssv"))
}

# main assembled dataframe
fullehunk <- bind_rows(elist, .id = "segment") %>%
  mutate(r_depth = round(q_depth, 0)) %>%
  mutate(day = day(m_present_time)) %>%
  mutate(hour = hour(m_present_time))

}
##### save everything ########

# dashboardValues <- list(
#   "ahrCap" = as.numeric(ahrCap),
#   "ahrUsed" = ahrUsed,
#   "ahrLeft" = ahrLeft,
#   "pwr3day" = pwr3day,
#   "pwr1day" = pwr1day,
#   "ahr3day" = ahr3day,
#   "ahr1day" = ahr1day,
#   "ahrAllday" = ahrAllday,
#   "LDmin" = LDmin,
#   "battLeft" = battLeft
# )


#assemble ggplots into list for export
if(gliderName == "usf-stella"){
livePlots <- list(
  #carousel plots
  leakLive, battLive, rollLive, couLive, ggEcho)
} else if (ahrCap > 0) {
  livePlots <- list(
    #carousel plots
    leakLive, battLive, rollLive, couLive)
} else {
  livePlots <- list(
    #carousel plots
    leakLive, battLive, rollLive, couLive)
}

# glider_live <- list(
#   gliderdf, scivarsLive, flightvarsLive, toGliderList,
#   ahrCap,
#   ahrUsed,
#   ahrLeft,
#   pwr3day,
#   pwr1day,
#   ahr3day,
#   ahr1day,
#   ahrAllday,
#   LDmin,
#   battLeft,
#   add_if_valid(echoListraw),
#   add_if_valid(fullehunk),
#   livePlots)

# 
# print("exporting everything")
# glider_live <-
#   list("gliderdf" = as.data.frame(gliderdf),
#        "scivarsLive" = scivarsLive,
#        "flightvarsLive" = flightvarsLive,
#        "toGliderList" = as.data.frame(toGliderList),
#        "dashboardValues" = dashboardValues,
#        "livePlots" = livePlots)
# 
# if(gliderName == "usf-stella"){
#   glider_live <- c(glider_live,
#                    as.data.frame(echoListraw),
#                    as.data.frame(fullehunk))
#   
#   # "echoListraw" = as.data.frame(echoListraw),
#   # "fullehunk" = as.data.frame(fullehunk))
# }
# 
# return(glider_live)

message("saving everything")
if(gliderName == "usf-stella"){
save(gliderdf, scivarsLive, flightvarsLive, toGliderList,
     ahrCap,
     ahrUsed,
     ahrLeft,
     pwr3day,
     pwr1day,
     ahr3day,
     ahr1day,
     ahrAllday,
     LDmin,
     battLeft,
     echoListraw,
     fullehunk,
     livePlots,
     file = paste0("/echos/", gliderName, "/glider_live.RData"))
} else if (ahrCap > 0) {
  save(gliderdf, scivarsLive, flightvarsLive, toGliderList,
       ahrCap,
       ahrUsed,
       ahrLeft,
       pwr3day,
       pwr1day,
       ahr3day,
       ahr1day,
       ahrAllday,
       LDmin,
       battLeft,
       #echoListraw,
       #fullehunk,
       livePlots,
       file = paste0("/echos/", gliderName, "/glider_live.RData"))
} else {
  save(gliderdf, scivarsLive, flightvarsLive, toGliderList,
       ahrCap,
       LDmin,
       battLeft,
       #echoListraw,
       #fullehunk,
       livePlots,
       file = paste0("/echos/", gliderName, "/glider_live.RData"))
}
     
} else {
  message("No data change, stopping")
}
}