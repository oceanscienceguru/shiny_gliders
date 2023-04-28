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

gliderDashboard_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    #observeEvent(input$gliderSelect, {
      
    load(paste0("/echos/", tail(deployedGliders$Name,1), "/glider_live.RData"))
    
    startDateLive <- min(gliderdf$m_present_time)
    endDateLive <- max(gliderdf$m_present_time)
    
    #get start/end days and update data filters
    updateDateInput(session, "date1Live", NULL, min = min(gliderdf$m_present_time), max = max(gliderdf$m_present_time), value = startDateLive)
    updateDateInput(session, "date2Live", NULL, min = min(gliderdf$m_present_time), max = max(gliderdf$m_present_time), value = endDateLive)
    
    updateSelectInput(session, "display_varLive", NULL, choices = c(scivarsLive), selected = tail(scivarsLive, 1))
    updateSelectizeInput(session, "flight_varLive", NULL, choices = c(flightvarsLive), selected = "m_roll")
    
    updateSelectInput(session, "derivedTypeLive", NULL, choices = c("Salinity", "Density", "SV Plot", "TS Plot"), selected = "Salinity")
    
    
    
    gliderChunk_live <- reactive({
      df <- gliderdf %>%
        filter(m_present_time >= input$date1Live & m_present_time <= input$date2Live)
      #filter(status %in% c(input$status)) %>%
      #filter(!(is.na(input$display_var) | is.na(m_depth))) %>%
      #filter(m_depth >= input$min_depth & m_depth <= input$max_depth)
      
      df
    })
    
    scienceChunk_live <- reactive({
      req(input$display_varLive)
      
      qf <- gliderChunk_live() %>%
        select(c(m_present_time, osg_depth, any_of(input$display_varLive))) %>%
        filter(!is.na(across(!c(m_present_time:osg_depth))))
      
      qf
      
    })
    
    flightChunk_live <- reactive({
      req(input$date1Live)
      
      df <- gliderChunk_live() %>%
        dplyr::select(c(m_present_time, all_of(input$flight_varLive))) %>%
        filter(m_present_time >= input$date1Live & m_present_time <= input$date2Live) %>%
        pivot_longer(
          cols = !m_present_time,
          names_to = "variable",
          values_to = "count") %>%
        filter(!is.na(count))
      
      df
      
    })
    
    output$LDBox <- renderValueBox({
      ns <- session$ns
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
      ns <- session$ns
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
      ns <- session$ns
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
      ns <- session$ns
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
      ns <- session$ns
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
      ns <- session$ns
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
      ns <- session$ns
      plotItUp <- list()
      for (i in 1:length(livePlots)){
        plotItUp[[i]] <- xmlSVG({show(livePlots[[i]])},standalone=TRUE, width = 9.5) 
      }
      
      slickR(obj = plotItUp,
             height = 400
      ) + settings(dots = TRUE, autoplay = TRUE, fade = TRUE, infinite = TRUE, autoplaySpeed = 7500)
      
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
    
    wpt <- gotoLoad(paste0("/gliders/gliders/", tail(deployedGliders$Name,1), "/archive/", tail(gotoFiles$fileName,1)))
    
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
      #waypoint
      addMarkers(lat = wpt$lat,
                 lng = wpt$long,
                 label = wpt$order) %>%
      addCircles(lat = wpt$lat,
                 lng = wpt$long,
                 radius = wpt$rad,
                 label = "Waypoint")
    #setView(lat = 27.75, lng = -83, zoom = 6)
    
    output$missionmapLive <- renderLeaflet({
      ns <- session$ns
      liveMissionMap })
    })
}
