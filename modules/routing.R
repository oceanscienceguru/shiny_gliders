##### Main glider dashboard module #########
library(shiny)

routing_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    h2("Route Planning"),
    p("Red circles denote points in selected goto file. Radius of circle is considered as achieved."),
    box(width=12,
      leafletOutput(outputId = ns("routingMap")) %>% withSpinner(color="#0dc5c1")
    ),
    fluidRow(
    selectInput(inputId = ns("gotoFile"),
                label = "Which goto file to load?",
                choices = c("user upload", routesList_files$names),
                selected = tail(routesList_files$names, 1)),
    fileInput(ns("gotoTest"), "Load goto file",
              multiple = FALSE,
              accept = c(".ma"))
    )
    )
}

routing_server <- function(id, gliderName) {
  
  moduleServer(id, function(input, output, session) {
    
    if (length(gliderName) > 0) {
    load(paste0("/echos/", gliderName, "/glider_live.RData"))
    
    #massage gps data a lot
    map_sf <- gliderdf %>%
      select(m_present_time, m_gps_lon, m_gps_lat) %>%
      filter(!is.na(m_gps_lat)) %>%
      mutate(latt = format(m_gps_lat, nsmall = 4),
             longg = format(m_gps_lon, nsmall = 4)) %>% #coerce to character keeping zeroes out to 4 decimals
      mutate(lat = gliderGPS_to_dd(latt),
             long = gliderGPS_to_dd(longg))
    
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
    
    #build goto history
    gotoHistory <- list()
    for (i in 1:gotoN) {
      gotoHistory[[i]] <- gotoLoad(paste0("/gliders/gliders/", gliderName, "/archive/", gotoFiles[i,]))
    }
    
    #get most recent goto file
    goto <- as.data.frame(tail(gotoHistory, 1))
    
    routingMap <- reactive({
      
      if(input$gotoFile == "user upload"){
        
        file <- input$gotoTest
        req(file)
        # print(file)
        # print(file$datapath)
        
        userGoto <- gotoLoad(file$datapath)
        
        #print(userGoto)
        #base provider layers
        p <- leaflet() %>%
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
          #waypoints, list first, then add current commanded
          addCircles(lat = goto$lat,
                     lng = goto$long,
                     radius = goto$rad,
                     label = goto$comment) %>%
          addMarkers(lat = cwpt$lat,
                     lng = cwpt$long,
                     label = "Commanded wpt") %>%
          addMeasure(primaryLengthUnit = "kilometers",
                     secondaryLengthUnit = "miles") %>%
          addCircles(lat = userGoto$lat,
                     lng = userGoto$long,
                     radius = userGoto$rad,
                     label = userGoto$comment,
                     color = "red") %>%
          addArrowhead(lat = userGoto$lat,
                       lng = userGoto$long, color="red",
                       options = arrowheadOptions(
                         #yawn = 60,
                         size = '10%',
                         frequency = 'allvertices',
                         fill = TRUE,
                         opacity=0.5, stroke=TRUE, fillOpacity=0.4,
                         proportionalToTotal = TRUE,
                         offsets = NULL,
                         perArrowheadOptions = NULL))
        
        p
      } else {
      
    p <- leaflet() %>%
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
      #waypoints, list first, then add current commanded
      addCircles(lat = goto$lat,
                 lng = goto$long,
                 radius = goto$rad,
                 label = goto$comment) %>%
      addMarkers(lat = cwpt$lat,
                 lng = cwpt$long,
                 label = "Commanded wpt") %>%
      addMeasure(primaryLengthUnit = "kilometers",
                 secondaryLengthUnit = "miles") %>%
      addCircles(lat = routesList[[input$gotoFile]]$lat,
                 lng = routesList[[input$gotoFile]]$long,
                 radius = routesList[[input$gotoFile]]$rad,
                 label = routesList[[input$gotoFile]]$comment,
                 color = "red") %>%
      addArrowhead(lat = routesList[[input$gotoFile]]$lat,
                   lng = routesList[[input$gotoFile]]$long, color="red",
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
    })
    
    output$routingMap <- renderLeaflet({
      routingMap()})
    
    } 
})
}
  
  
    

