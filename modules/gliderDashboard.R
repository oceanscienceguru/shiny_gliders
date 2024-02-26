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
    
    if (length(gliderName) > 0) {
    load(paste0(liveDir, "/", gliderName, "/glider_live.RData"))



    output$LDBox <- renderValueBox({
      if(is.null(LDmin)){
        Mycolor = "black"
      } else if(LDmin >= 2.3){
        Mycolor = "green"
      } else if(LDmin >= 2 & LDmin < 2.3) {
        Mycolor = "yellow"
      } else if(LDmin >= 0 & LDmin < 2) {
        Mycolor = "red"
      }
      valueBox(
        ifelse(!is.null(LDmin), round(LDmin, 3), "N/A"), "LD min", icon = icon("tint", lib = "glyphicon"),
        color = Mycolor
      )
    })
    
    if (ahrCap > 0) {
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
    
    } else {
      output$battBox <- renderValueBox({
        if(battLeft >= 14){
          Mycolor = "green"
        } else if(battLeft >= 7 & battLeft < 14) {
          Mycolor = "yellow"
        } else {
          Mycolor = "red"
        }
        valueBox(
          round(battLeft, 0), "Approx. Days Left", icon = icon("off", lib = "glyphicon"),
          color = Mycolor
        )
      })
    }

    output$img <- renderSlickR({
      
      ## NOTE: requires non-CRAN for switching plots
      ## remotes::install_github("yonicd/slickR@1ab229e4c400e54187a406130610852b0300986c")
      plotItUp <- list()
      for (i in 1:length(livePlots)){
        plotItUp[[i]] <- xmlSVG({methods::show(livePlots[[i]])},standalone=TRUE, width = 9.5)
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
      mutate(lat = gliderGPS_to_dd(latt),
             long = gliderGPS_to_dd(longg)) %>%
      filter(lat >= -90 & lat <= 90) %>% #remove illegal values
      filter(long >= -180 & long <= 180)

   # if (nrow(toGliderList) > 0){
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
      mutate(lat = gliderGPS_to_dd(c_wpt_lat),
             long = gliderGPS_to_dd(c_wpt_lon))
    
    gotoN <- as.integer(nrow(gotoFiles))
    
    if (gotoN > 0){
    #build goto history
    gotoHistory <- list()
    for (i in 1:gotoN) {
      gotoHistory[[i]] <- gotoLoad(paste0(rawDir, "/gliders/", gliderName, "/archive/", gotoFiles[i,]))
    }
    
    #get most recent goto file
    goto <- as.data.frame(tail(gotoHistory, 1))
    }
    
    liveMissionMap <- leaflet() %>%
      #base provider layers
      addWMSTiles("https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}.png",
                  layers = "World_Ocean_Base",
                  group = "Ocean Basemap",
                  options = WMSTileOptions(format = "image/png", transparent = F)) %>%
      addWMSTiles("https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/{z}/{y}/{x}.png",
                  layers = "World_Ocean_Reference",
                  group = "Ocean Reference",
                  options = WMSTileOptions(format = "image/png", transparent = T)) %>%
      addWMSTiles("https://www.gebco.net/data_and_products/gebco_web_services/web_map_service/mapserv?",
                  layers = "GEBCO_LATEST",
                  group = "GEBCO",
                  options = WMSTileOptions(format = "image/png", transparent = F)) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "World Imagery") %>%
      addLayersControl(baseGroups = c('Ocean Basemap', 'GEBCO', 'World Imagery'),
                       overlayGroups = c('Ocean Reference')) %>%
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
      addMeasure(primaryLengthUnit = "kilometers",
                 secondaryLengthUnit = "miles") %>%
      addSimpleGraticule(interval = 2.5) %>%
      addFullscreenControl()
    
    if (nrow(cwpt > 0)) {
      liveMissionMap <- liveMissionMap %>%
        addMarkers(lat = cwpt$lat,
                   lng = cwpt$long,
                   label = "Commanded wpt")
    }
    
    if (gotoN > 0) {
      liveMissionMap <- liveMissionMap %>%
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
                       perArrowheadOptions = NULL))
      }
    #setView(lat = 27.75, lng = -83, zoom = 6)

    output$missionmapLive <- renderLeaflet({
      liveMissionMap})
    
    } 
    })
  
}
