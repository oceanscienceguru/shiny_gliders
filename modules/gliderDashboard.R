##### Main glider dashboard module #########
library(shiny)

gliderDashboard_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
            fluidRow(
            box(
              leafletOutput(outputId = ns("missionmapLive"))
            ),
            box(
              slickROutput(ns("img"))
            ),
            # box(
            #   plotOutput(
            #     outputId = "battplot",
            #   ) %>% withSpinner(color="#0dc5c1")
            # ),
          ),
          fluidRow(
            valueBoxOutput(
              ns("recoBox"),
              width = 2),
            valueBoxOutput(
              ns("LDBox"),
              width = 2),
            valueBoxOutput(
              ns("battBox"),
              width = 2),
            valueBoxOutput(
              ns("powerBox1"),
              width = 2),
            valueBoxOutput(
              ns("powerBox3"),
              width = 2),
            valueBoxOutput(
              ns("powerBoxall"),
              width = 2),
          ),
          # fluidRow(
          #   box(
          #     plotOutput(
          #       outputId = "battplot",
          #     ),
          #   ),
          # ),
  )
}

gliderDashboard_server <- function(id, gliderName) {
  
  moduleServer(id, function(input, output, session) {
    # print(gliderName())
    load(paste0("/echos/", gliderName, "/glider_live.RData"))



    output$LDBox <- renderValueBox({
      if(LDmin >= 2.3){
        Mycolor = "green"
      } else if(LDmin >= 2 & LDmin < 2.3) {
        Mycolor = "yellow"
      } else {
        Mycolor = "red"
      }
      valueBox(
        round(LDmin, 3), "LD min", icon = icon("tint", lib = "glyphicon"),
        color = Mycolor
      )
    })

    output$recoBox <- renderValueBox({
      recoDays <- ((ahrCap*.9)-ahrUsed)/ahr3day

      if(recoDays >= 14){
        Mycolor = "green"
      } else if(recoDays >= 7 & recoDays < 14) {
        Mycolor = "yellow"
      } else {
        Mycolor = "red"
      }
      valueBox(
        round(recoDays, 1), "Days til 10% charge abort", icon = icon("time", lib = "glyphicon"),
        color = Mycolor
      )
    })

    output$battBox <- renderValueBox({
      if(battLeft >= 25){
        Mycolor = "green"
      } else if(battLeft >= 10 & battLeft < 25) {
        Mycolor = "yellow"
      } else {
        Mycolor = "red"
      }
      valueBox(
        round(battLeft, 3), "Batt Percent", icon = icon("off", lib = "glyphicon"),
        color = Mycolor
      )
    })

    output$powerBox3 <- renderValueBox({
      if(ahr3day <= 7){
        Mycolor = "green"
      } else if(ahr3day >= 7 & ahr3day < 10) {
        Mycolor = "yellow"
      } else {
        Mycolor = "red"
      }
      valueBox(
        round(ahr3day, 3), "3 Day Power Usage", icon = icon("off", lib = "glyphicon"),
        color = Mycolor
      )
    })

    output$powerBox1 <- renderValueBox({
      if(ahr1day <= 7){
        Mycolor = "green"
      } else if(ahr1day >= 7 & ahr1day < 10) {
        Mycolor = "yellow"
      } else {
        Mycolor = "red"
      }
      valueBox(
        round(ahr1day, 3), "1 Day Power Usage", icon = icon("off", lib = "glyphicon"),
        color = Mycolor
      )
    })

    output$powerBoxall <- renderValueBox({
      if(ahrAllday <= 7){
        Mycolor = "green"
      } else if(ahrAllday >= 7 & ahrAllday < 10) {
        Mycolor = "yellow"
      } else {
        Mycolor = "red"
      }
      valueBox(
        round(ahrAllday, 3), "Full Mission Power Usage", icon = icon("off", lib = "glyphicon"),
        color = Mycolor
      )
    })

    output$img <- renderSlickR({
      
      ## NOTE: requires non-CRAN for switching plots
      ## remotes::install_github("yonicd/slickR@1ab229e4c400e54187a406130610852b0300986c")
      plotItUp <- list()
      for (i in 1:length(livePlots)){
        plotItUp[[i]] <- xmlSVG({show(livePlots[[i]])},standalone=TRUE, width = 9.5)
      }

      x <- slickR(obj = plotItUp,
             height = 400,
             slideId = gliderName
      ) + settings(dots = TRUE, autoplay = TRUE, fade = TRUE, infinite = TRUE, autoplaySpeed = 7500)

      x
    })

    #live mission map
    #massage gps data a lot
    map_sf <- gliderdf %>%
      select(m_present_time, m_gps_lon, m_gps_lat) %>%
      filter(!is.na(m_gps_lat)) %>%
      mutate(latt = format(m_gps_lat, nsmall = 4),
             longg = format(m_gps_lon, nsmall = 4)) %>% #coerce to character keeping zeroes out to 4 decimals
      separate(latt, paste0("latt",c("d","m")), sep="\\.", remove = FALSE) %>% #have to double escape to sep by period
      separate(longg, paste0("longg",c("d","m")), sep="\\.", remove = FALSE) %>%
      mutate(latd = substr(lattd, 1, nchar(lattd)-2), #pull out degrees
             longd = substr(longgd, 1, nchar(longgd)-2)) %>%
      mutate(latm = paste0(str_sub(lattd, start= -2),".",lattm), #pull out minutes
             longm = paste0(str_sub(longgd, start= -2),".",longgm)) %>%
      mutate_if(is.character, as.numeric) %>% #coerce back to numeric
      mutate(lat = latd + (latm/60),
             long = (abs(longd) + (longm/60))*-1) #*-1 for western hemisphere

    gotoFiles <- toGliderList %>%
      filter(str_ends(fileName, "goto_l10.ma")) %>%
      arrange(fileName)

    #get commanded wpt
    cwpt <- gliderdf %>%
      select(m_present_time, c_wpt_lat, c_wpt_lon) %>%
      filter(!is.na(c_wpt_lat)) %>%
      select(!c(m_present_time)) %>%
      format(., nsmall = 4) %>% #coerce to character keeping zeroes out to 4 decimals
      tail(1)  %>% 
      separate(c_wpt_lat, paste0("latt",c("d","m")), sep="\\.", remove = FALSE) %>% #have to double escape to sep by period
      separate(c_wpt_lon, paste0("longg",c("d","m")), sep="\\.", remove = FALSE) %>%
      mutate(latd = substr(lattd, 1, nchar(lattd)-2), #pull out degrees
             longd = substr(longgd, 1, nchar(longgd)-2)) %>%
      mutate(latm = paste0(str_sub(lattd, start= -2),".",lattm), #pull out minutes
             longm = paste0(str_sub(longgd, start= -2),".",longgm)) %>%
      mutate_if(is.character, as.numeric) %>% #coerce back to numeric
      mutate(lat = latd + (latm/60),
             long = (abs(longd) + (longm/60))*-1) #*-1 for western hemisphere
    
    gotoN <- as.integer(nrow(gotoFiles))
    
    #build goto history
    gotoHistory <- list()
    for (i in 1:gotoN) {
      gotoHistory[[i]] <- gotoLoad(paste0("/gliders/gliders/", gliderName, "/archive/", gotoFiles[i,]))
    }
    
    #get most recent goto file
    goto <- as.data.frame(tail(gotoHistory, 1))
    
    liveMissionMap <- leaflet() %>%
      #base provider layers
      addProviderTiles(providers$Esri.OceanBasemap,
                       group = "Ocean Basemap") %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "World Imagery") %>%
      addLayersControl(baseGroups = c('Ocean Basemap', 'World Imagery')) %>%
      addPolylines(
        lat = map_sf$lat,
        lng = map_sf$long,
        color = "grey",
        weight = 3,
        opacity = 1,
      ) %>%
      #timestamps for surfacings
      addCircles(data = map_sf,
                 lat = map_sf$lat,
                 lng = map_sf$long,
                 color = "gold",
                 popup = map_sf$m_present_time,
                 weight = 3
      ) %>%
      #start marker
      addAwesomeMarkers(
        lat = map_sf[1, "lat"],
        lng = map_sf[1, "long"],
        label = "Initial position",
        icon = icon.start
      ) %>%
      #end marker
      addAwesomeMarkers(
        lat = map_sf[nrow(map_sf), "lat"],
        lng = map_sf[nrow(map_sf), "long"],
        label = "Latest position",
        icon = icon.latest
      ) %>%
      #waypoints, list first, then add current commanded
      addCircles(lat = goto$lat,
                 lng = goto$long,
                 radius = goto$rad,
                 label = goto$comment) %>%
      addArrowhead(lat = goto$lat,
                   lng = goto$long, color="blue",
                   options = arrowheadOptions(
                     #yawn = 60,
                     size = '10%',
                     frequency = 'allvertices',
                     fill = TRUE,
                     opacity=0.5, stroke=TRUE, fillOpacity=0.4,
                     proportionalToTotal = TRUE,
                     offsets = NULL,
                     perArrowheadOptions = NULL)) %>%
      addMarkers(lat = cwpt$lat,
                 lng = cwpt$long,
                 label = "Commanded wpt") %>%
      addMeasure(primaryLengthUnit = "kilometers",
                 secondaryLengthUnit = "miles")
    #setView(lat = 27.75, lng = -83, zoom = 6)

    output$missionmapLive <- renderLeaflet({
      liveMissionMap})
    
    
    })
}
