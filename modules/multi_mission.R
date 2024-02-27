##### Multi mission module #########
library(shiny)

multi_mission_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    h2("Multi mission stuff"),
    p("Pick missions"),
    # box(width=12,
    #     leafletOutput(outputId = ns("routingMap")) %>% withSpinner(color="#0dc5c1")
    # ),
    box(
      actionButton(
      inputId = ns("load"),
      label = "Load Mission Data",
      icon("plane"),
      style =
        "color: #fff; background-color: #963ab7; border-color: #2e6da4"
    ),
    br(),
      selectizeInput(
        inputId = ns("mission"),
        label = "Which mission data to display",
        choices = NULL,
        selected =  NULL,
        multiple = TRUE,
        options = list(plugins = list('remove_button'))
      ),
    selectizeInput(
      inputId = ns("isobath"),
      label = "Which isobath",
      choices = c(30, 50, 100),
      selected =  NULL,
      multiple = TRUE,
      options = list(plugins = list('remove_button'))
     )),
    # box(
    #   verbatimTextOutput(
    #     outputId = ns("test"), placeholder = TRUE
    #   )
    # ),
    box(
      leafletOutput(outputId = ns("georef_map")),
      br(),
      # Button
      downloadButton(
        outputId = ns("georef_down"), 
        label = "Download plotted data"),
      br()
    ),
  )
}

multi_mission_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    fileList_archive <- list.files(path = paste0(fullDir, "Data/"),
                                   pattern = "*.RData")
    
    missionList_archive <- str_remove(fileList_archive, pattern = ".RData")
    
    updateSelectizeInput(session, "mission", NULL, choices = c(missionList_archive), selected = head(missionList_archive, 1))
    
    mission_list <- reactiveValues(id = NULL)
    depth_list   <- reactiveValues(isobath = NULL)
    
    glider_map  <- reactiveValues()
    export_data <- reactiveValues()

    observe({
    mission_list$id    <- input$mission
    depth_list$isobath <- input$isobath
    })
    
    output$test <- renderText({ -1*as.numeric(depth_list$isobath) })

    # bounding box setup
    latitude <- as.numeric(c("25.5", "31", "31", "25.5"))
    longitude <- as.numeric(c("-81.5", "-81.5", "-88", "-88"))
    rhombus <- data.frame(latitude, longitude)
    eGOM <- rhombus %>%
      st_as_sf(coords = c("longitude", "latitude"),
               crs = 4008) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")

    #load in full isobaths and select
    eyeso <- st_read("./isobaths/w98e78n31s18_isobath_5-100m.shp")

    iso30 <- eyeso %>%
      filter(CONTOUR == -30) %>%
      st_intersection(eGOM)

    iso50 <- eyeso %>%
      filter(CONTOUR == -50) %>%
      st_intersection(eGOM)

    iso100 <- eyeso %>%
      filter(CONTOUR == -100) %>%
      st_intersection(eGOM)
    
    odvVars <- c("sci_water_temp",
                 "osg_rho",
                 "sci_flbbcd_chlor_units",
                 "sci_flbbcd_bb_units")

    observeEvent(input$load, {
    depth <- c(-1*as.numeric(depth_list$isobath))
    output <- list()
    for (i in mission_list$id){
      print(i) #watch progress
      holding <- new.env() #clear environment each time

      load(paste0("./Data/", i, ".RData"), envir = holding) #load in

      #process
      depthDF <- holding$gliderdf %>%
        mutate(ddate = date(m_present_time)) %>%
        group_by(yo_id) %>%
        mutate(gDepth = max(osg_i_depth, na.rm = TRUE),
               wDepth = max(m_water_depth, na.rm = TRUE)) %>%
        #filter(m_present_time %within% interval(ymd("2023-01-01"), ymd("2023-12-31"))) %>%
        #ungroup() %>%
        arrange(m_present_time, .by_group = FALSE)

      gliderTrack <- depthDF %>%
        filter(!is.na(i_lon)) %>%
        filter(!is.na(yo_id)) %>%
        arrange(m_present_time) %>%
        group_by(yo_id) %>%
        slice(c(1,n())) %>%
        st_as_sf(coords = c("i_lon", "i_lat"), crs = 4008) %>%
        group_by(yo_id) %>%
        dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
        st_cast("LINESTRING")

      yoDepth <- depthDF %>%
        distinct(yo_id, gDepth, wDepth) %>%
        filter(yo_id > 0)

      #find deepest dive
      deepestYo <- yoDepth$yo_id[which(abs(yoDepth$gDepth - max(yoDepth$gDepth, na.rm = TRUE)) == min(abs(yoDepth$gDepth - max(yoDepth$gDepth, na.rm = TRUE))))]

      yos <- list()
      for (j in seq_along(depth)){
        iso <- eyeso %>%
          filter(CONTOUR == depth[j]) %>%
          st_intersection(eGOM)

        #add check for nearest?
        targets <- gliderTrack[lengths(st_intersects(gliderTrack, iso)) > 0,] %>%
          distinct(yo_id) %>%
          #get range of yos to check
          mutate(yoMin = yo_id-3,
                 yoMax = yo_id+3)

        #only sequence if there are yos to sequence
        if(length(targets$yo_id) > 0){
          targets <- targets %>%
            rowwise() %>%
            #format for vectorize
            mutate(yoRan = list(seq.int(yoMin, yoMax)))

          #vectorize
          yoCandidates <- unlist(targets$yoRan, use.names = FALSE)

        } else {
          yoCandidates <- NULL
        }

        #score the yoCandidates and pick top score for each transect type
        scoredYos <- depthDF %>%
          filter(yo_id %in% yoCandidates) %>%
          group_by(yo_id) %>%
          mutate(depthDelta = abs(gDepth - (-1*depth[j]))) %>%
          distinct(depthDelta) %>%
          mutate(transect = ifelse(yo_id < deepestYo, 0, 1)) %>% #0 headed off, 1 headed in
          ungroup() %>%
          group_by(transect) %>%
          arrange(depthDelta, .by_group = TRUE) %>%
          slice(1)

        if(length(scoredYos$yo_id[scoredYos$transect == 0]) > 0){
          outYo <- depthDF %>%
            filter(yo_id == scoredYos$yo_id[scoredYos$transect == 0]) %>%
            filter(cast == "Downcast") %>%
            #drop the data if the glider didn't get within 15m of the target
            #filter(between(gDepth, depth[j]-15, depth[j]+15)) %>%
            mutate(isobath = -1*depth[j]) %>%
            mutate(transect = "out")
        } else {
          outYo <- data.frame()
        }

        if(length(scoredYos$yo_id[scoredYos$transect == 1]) > 0){
          inYo <- depthDF %>%
            filter(yo_id == scoredYos$yo_id[scoredYos$transect == 1]) %>%
            filter(cast == "Downcast") %>%
            #drop the data if the glider didn't get within 15m of the target
            #filter(between(gDepth, depth[j]-15, depth[j]+15)) %>%
            mutate(isobath = -1*depth[j]) %>%
            mutate(transect = "in")
        } else {
          inYo <- data.frame()
        }

        yos[[j]] <- bind_rows(outYo, inYo)
        rm(targets, outYo, inYo, iso, yoCandidates, scoredYos)
      }

      #export
      output[[i]] <- bind_rows(yos)
      rm(depthDF, yoDepth, deepestYo, yos, gliderTrack)
    }

    isobathDF <- bind_rows(output, .id = "missionID") %>%
      separate(missionID,
               c("missionNum","gliderName"),
               sep = "_",
               remove = FALSE) %>%
      ungroup() %>%
      unite("isoMission",
            c(missionNum, isobath, transect),
            sep = "_",
            remove = FALSE) %>%
      mutate(isobath = as.factor(isobath)) %>%
      group_by(missionNum, yo_id) %>%
      mutate(yo_lat = mean(i_lat, na.rm = TRUE),
             yo_lon = mean(i_lon, na.rm = TRUE))
    
    export_data$full <- isobathDF %>%
      select(missionNum, isoMission, yo_id, isobath, transect, m_present_time, yo_lat, yo_lon, osg_i_depth, any_of(odvVars))
    
    zz <- distinct(isobathDF, isoMission, yo_lat, yo_lon)
    
    glider_map$map <- leaflet() %>%
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
      #timestamps for surfacings
      addPolylines(data = iso30) %>%
      addPolylines(data = iso50) %>%
      addPolylines(data = iso100) %>%
      # addPolylines(data = gliderTrack) %>%
      addCircles(data = zz,
                 lat = zz$yo_lat,
                 lng = zz$yo_lon,
                 color = "black",
                 popup = zz$isoMission
      ) %>%
      setView(lng = mean(zz$yo_lon),
              lat = mean(zz$yo_lat),
              zoom = 7) %>%
      addFullscreenControl()
    
    })
    
    output$georef_map <- renderLeaflet({
      glider_map$map})
    
    output$georef_down <- downloadHandler(
      filename = function() {
        paste0(length(mission_list$id), "_missions_isobaths", ".csv")
      },
      content = function(file) {
        write.csv(export_data$full, file, row.names = FALSE)
      }
    )
    
  })
}


