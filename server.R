server <- function(input, output, session) {
  # glider = reactive({
  #   #req(input$mission)
  #   readRDS(paste0("./Data/", input$mission, ".rds"))
  # })
  
  
  #### live mission plotting #####
  load("/echos/usf-stella/glider_live.RData")
          #gliderdf, scivarsLive, flightvarsLive
  
  #load in .RData with latest plots from Cron job

  #mission date range variables
  startDateLive <- min(gliderdf$m_present_time)
  endDateLive <- max(gliderdf$m_present_time)
  
  #get start/end days and update data filters
  updateDateInput(session, "date1Live", NULL, min = min(gliderdf$m_present_time), max = max(gliderdf$m_present_time), value = startDateLive)
  updateDateInput(session, "date2Live", NULL, min = min(gliderdf$m_present_time), max = max(gliderdf$m_present_time), value = endDateLive)
  
  updateSelectInput(session, "display_varLive", NULL, choices = c(scivarsLive), selected = tail(scivarsLive, 1))
  updateSelectizeInput(session, "flight_varLive", NULL, choices = c(flightvarsLive), selected = "m_roll")
  
  updateSelectInput(session, "derivedTypeLive", NULL, choices = c("Salinity", "Density", "SV Plot", "TS Plot"), selected = "Salinity")
  
  #glider_live <- list(science = sdf, flight = fdf)
  
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
  
  battLive <- reactive({
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
    
    battLive
    
  })
  
  output$battplot <- renderPlot({battLive()})
  
  leakLive <- reactive({
    vars <- c("m_leakdetect_voltage", "m_leakdetect_voltage_forward", "m_leakdetect_voltage_science")
    
    leaks <- gliderdf %>%
      select(c(m_present_time, any_of(vars))) %>%
      filter(m_leakdetect_voltage > 0) %>%
      filter(m_present_time >= endDateLive-14400) %>%
      pivot_longer(cols = any_of(vars))
    
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
    
    leakLive
    
  })
  
  output$leakplot <- renderPlot({leakLive()})
  
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
    
    test <- list(xmlSVG({show(battLive())},standalone=TRUE, width = 9.5),
                 xmlSVG({show(leakLive())},standalone=TRUE, width = 9.5))
    
    slickR(obj = test,
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
  
  wpt <- gliderdf %>%
    select(m_present_time, c_wpt_lat, c_wpt_lon) %>%
    filter(!is.na(c_wpt_lat)) %>%
    select(!c(m_present_time)) %>%
    format(., nsmall = 4) %>%
    tail(1)  %>% #coerce to character keeping zeroes out to 4 decimals
    separate(c_wpt_lat, paste0("latt",c("d","m")), sep="\\.", remove = FALSE) %>% #have to double escape to sep by period
    separate(c_wpt_lon, paste0("longg",c("d","m")), sep="\\.", remove = FALSE) %>%
    mutate(latd = substr(lattd, 1, nchar(lattd)-2), #pull out degrees
           longd = substr(longgd, 1, nchar(longgd)-2)) %>%
    mutate(latm = paste0(str_sub(lattd, start= -2),".",lattm), #pull out minutes
           longm = paste0(str_sub(longgd, start= -2),".",longgm)) %>%
    mutate_if(is.character, as.numeric) %>% #coerce back to numeric
    mutate(lat = latd + (latm/60),
           long = (abs(longd) + (longm/60))*-1) #*-1 for western hemisphere
  
  
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
               label = "Waypoint") %>%
    setView(lat = 27.75, lng = -83, zoom = 6)
  
  output$missionmapLive <- renderLeaflet({liveMissionMap})
  
  ######### current mission data ########
  
  observeEvent(input$tabs, {
    
    if(input$tabs == "currMissData"){
      #Code for tab 2
      print("Tab currMissData code is run")

  
  gg1Live <- reactive({
    sciLive <- ggplot(
      data = 
        scienceChunk_live(),#dynamically filter the sci variable of interest
      aes(x=m_present_time,
          y=osg_depth,
          #z=.data[[input$display_varLive]],
          colour = .data[[input$display_varLive]],
      )) +
      geom_point(
        # size = 2,
        na.rm = TRUE
      ) +
      # coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
      #geom_hline(yintercept = 0) +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(input$minLive, input$maxLive)) +
      geom_point(data = filter(gliderChunk_live(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.3,
                 color = "black",
                 na.rm = TRUE
      ) +
      theme_bw() +
      labs(#title = paste0(missionNum, " Science Data"),
        y = "Depth (m)",
        x = "Date") +
      theme(plot.title = element_text(size = 32)) +
      theme(axis.title = element_text(size = 16)) +
      theme(axis.text = element_text(size = 12))
    
    sciLive
    
  })
  
  output$sciPlotLive <- renderPlot({gg1Live()})
  
  #flight plot
  gg2Live <- reactive({
    # if (input$flight_var == "m_roll") {
    #   flightxlabel <- "roll"
    # } else if (input$flight_var == "m_heading") {
    #   flightxlabel <- "heading"
    # }
    #req(input$load)
    fliLive <- ggplot(
      data =
        flightChunk_live(),
      aes(x = m_present_time,
          y = count,
          color = variable,
          shape = variable)) +
      geom_point() +
      #coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
      theme_bw() +
      labs(#title = paste0(missionNum, " Flight Data"),
        x = "Date") +
      theme(plot.title = element_text(size = 32)) +
      theme(axis.title = element_text(size = 16)) +
      theme(axis.text = element_text(size = 12))
    
    fliLive 
    
  })
  
  output$fliPlotLive <- renderPlot({gg2Live()})
  
  ##### derived Live plots #########
  gg3Live <- reactive({
    
    if (input$derivedTypeLive == "TS Plot"){
    df <- filter(gliderChunk_live(), osg_salinity > 0)
    wf <- filter(gliderChunk_live(), m_water_depth > 0)
    
    plot <- 
      ggplot(
      data = df,
      aes(x = osg_salinity,
          y = osg_theta,
          #color = segment,
          #shape = variable
          )) +
      geom_point(size = 3,
                 pch = 1) +
      # scale_color_gradient(
      #   low = "red",
      #   high = "blue",
      # ) +
      #coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
      theme_bw() +
      labs(title = "TS Plot",
        x = "Salinity",
        y = "Potential Temperature",
        #color = "Time",
        #caption = "Red = older ... Blue = more recent"
        ) +
      theme(plot.title = element_text(size = 32),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12),
            plot.caption = element_text(size = 16),
            legend.position ="none",
            ) 
    }
    
    if (input$derivedTypeLive == "SV Plot"){
      df <- filter(gliderChunk_live(), osg_soundvel1 > 0)
      wf <- filter(gliderChunk_live(), m_water_depth > 0)
      
      plot <- 
        ggplot(data = df,
               aes(x=m_present_time,
                   y=osg_depth,
                   #z=osg_soundvel1
                   )) +
        geom_point(
          aes(color = osg_soundvel1)
        ) +
        #geom_hline(yintercept = 0) +
        scale_y_reverse() +
        scale_colour_viridis_c() +
        geom_point(data = wf,
                   aes(y = m_water_depth),
                   size = 0.3,
                   color = "black",
                   na.rm = TRUE
        ) +
        theme_bw() +
        labs(title = "Sound Velocity",
             caption = "Calculated using Coppens <i>et al.</i> (1981)",
             y = "Depth (m)",
             x = "Date") +
        theme(plot.title = element_text(size = 32)) +
        theme(axis.title = element_text(size = 16)) +
        theme(axis.text = element_text(size = 12)) +
        theme(plot.caption = element_markdown())
    }
    
    if (input$derivedTypeLive == "Density"){
      df <- filter(gliderChunk_live(), osg_rho > 0)
      wf <- filter(gliderChunk_live(), m_water_depth > 0)
      
      plot <- 
        ggplot(
        data = 
          df,
        aes(x=m_present_time,
            y=osg_depth,
            #z=.data[[input$display_varLive]],
            colour = osg_rho,
        )) +
        geom_point(
          # size = 2,
          # na.rm = TRUE
        ) +
        # coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
        #geom_hline(yintercept = 0) +
        scale_y_reverse() +
        scale_colour_viridis_c() +
        geom_point(data = wf,
                   aes(y = m_water_depth),
                   size = 0.3,
                   color = "black",
                   na.rm = TRUE
        ) +
        theme_bw() +
        labs(title = "Density at Depth",
          y = "Depth (m)",
          x = "Date") +
        theme(plot.title = element_text(size = 32)) +
        theme(axis.title = element_text(size = 16)) +
        theme(axis.text = element_text(size = 12))
    }
    
    if (input$derivedTypeLive == "Salinity"){
      df <- filter(gliderChunk_live(), osg_salinity > 0)
      wf <- filter(gliderChunk_live(), m_water_depth > 0)
      
      plot <- 
        ggplot(
          data = 
            df,
          aes(x=m_present_time,
              y=osg_depth,
              #z=.data[[input$display_varLive]],
              colour = osg_salinity,
          )) +
        geom_point(
          # size = 2,
          # na.rm = TRUE
        ) +
        # coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
        #geom_hline(yintercept = 0) +
        scale_y_reverse() +
        scale_colour_viridis_c() +
        geom_point(data = wf,
                   aes(y = m_water_depth),
                   size = 0.3,
                   color = "black",
                   na.rm = TRUE
        ) +
        theme_bw() +
        labs(title = "Salinity at Depth",
             y = "Depth (m)",
             x = "Date") +
        theme(plot.title = element_text(size = 32)) +
        theme(axis.title = element_text(size = 16)) +
        theme(axis.text = element_text(size = 12))
    }
    
    plot
    
  })
  
  output$tsPlotLive <- renderPlot({gg3Live()})
  
  #### psuedograms ########
  
  #color palette source:
  #https://rdrr.io/github/hvillalo/echogram/src/R/palette.echogram.R
  velInfo <- file.info(list.files(path = "/echos/layers/",
                                  full.names = TRUE)) %>%
    filter(size > 0)
  
  velList <- rownames(velInfo) %>%
    basename()
  
  depthInfo <- file.info(list.files(path = "/echos/depths/",
                                    full.names = TRUE))
  
  depthList <- rownames(depthInfo) %>%
    basename()
  
  echoListraw <- intersect(velList, depthList) %>%
    str_remove(pattern = ".ssv") %>%
    enframe() %>%
    mutate(ID = str_extract(value, "(?<=-)[0-9]*$")) %>%
    mutate(ID = as.numeric(ID)) %>%
    arrange(ID)
  
  echoList <- echoListraw$value
  
  #reactive pseudogram plot identifier for scrolling
  selectPgram <- reactiveValues(seg = NULL, id = NULL)
  
  updateSelectInput(session, "echo", NULL, choices = c(echoListraw$value), selected = tail(echoListraw$value))
  
  #attach IDs to psuedogram plot reactives
  observeEvent(input$echo, {
    selectPgram$seg <- input$echo
    selectPgram$id <- match(input$echo, echoListraw$value)
  })
  
  #process the requested pseudogram
  ehunk <- reactive({
    req(input$echo)
    
    ehunk <- pseudogram(paste0("/echos/layers/", selectPgram$seg, ".ssv"),
                        paste0("/echos/depths/", selectPgram$seg, ".ssv"))
  })
  
  ##### main pseudogram plot ####
  gg4 <- reactive({
    #plot
    ggEcho <-
      ggplot(data = 
               ehunk(),
             aes(x=m_present_time,
                 y=p_depth,
                 z=value)) +
      # geom_tile(aes(
      #   color = value,
      #   size = 10
      #   )
      # ) +
      # coord_fixed(ratio = 3.6) +
      geom_point(
        aes(color = value),
        size = 6,
        pch = 15,
        na.rm = TRUE
      ) +
      #coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
      # scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
      #                                    "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
      #                        limits = c(-75, -35)) +
      scale_y_reverse() +
      theme_bw() +
      labs(title = paste0(selectPgram$seg, " Pseudogram"),
           y = "Depth (m)",
           x = "Date/Time (UTC)",
           colour = "dB") +
      theme(plot.title = element_text(size = 32),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12),
            legend.key = element_blank(),
            plot.caption = element_markdown()) +
      guides(size="none") +
      scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))
    
    if (input$echoColor == "EK") {
      ggEcho +
        scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
                                                    "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
                                                    limits = c(-75, -30))
    } else if (input$echoColor == "magma") {
      ggEcho +
        scale_colour_viridis_c(limits = c(-75, -30),
                               option = "C"
        )
    } else {
      ggEcho +
        scale_colour_viridis_c(limits = c(-75, -30),
                               option = "D"
        )
    }
    
    
  })
  
  output$echoPlot <- renderPlot({gg4()})
  
  #### Buttons to scroll through pseudograms ####
  observeEvent(input$oldestPgram, {
    selectPgram$seg <- head(echoListraw$value, 1)
    updateSelectInput(session, "echo", NULL, choices = c(echoListraw$value), selected = head(echoListraw$value, 1))
  })
  observeEvent(input$prevPgram, {
    if (selectPgram$id > 1) {
      selectPgram$id <- selectPgram$id - 1
      updateSelectInput(session, "echo", NULL, choices = c(echoListraw$value), selected = echoListraw$value[selectPgram$id])
    }
  })
  observeEvent(input$nextPgram, {
    if (selectPgram$id < nrow(echoListraw)) {
      selectPgram$id <- selectPgram$id + 1
      updateSelectInput(session, "echo", NULL, choices = c(echoListraw$value), selected = echoListraw$value[selectPgram$id])
    }
  })
  observeEvent(input$latestPgram, {
    selectPgram$seg <- tail(echoListraw$value, 1)
    updateSelectInput(session, "echo", NULL, choices = c(echoListraw$value), selected = tail(echoListraw$value, 1))
  })
  
  
  #### pseudotimegram main setup ####
  fullehunk <- reactive({
    req(input$fullecho | input$fullecho2)
    
    elist <- list()
    for (i in echoList) {
      elist[[i]] <- pseudogram(paste0("/echos/layers/", i, ".ssv"),
                               paste0("/echos/depths/", i, ".ssv"))
    }
    ef <- bind_rows(elist, .id = "segment") %>%
      mutate(r_depth = round(q_depth, 0)) %>%
      mutate(day = day(m_present_time)) %>%
      mutate(hour = hour(m_present_time))
    
    ef
    
  })
  
  
  #### updater for pseudotimegram inputs ####
  observeEvent(input$fullecho | input$fullecho2, {
    
    updateDateRangeInput(session, "echohistrange", label = NULL, 
                         start = (max(fullehunk()$m_present_time)-259200),
                         end = max(fullehunk()$m_present_time), 
                         min = min(fullehunk()$m_present_time), 
                         max = max(fullehunk()$m_present_time))
    
    updateDateRangeInput(session, "echohistrange2", label = NULL, 
                         start = min(fullehunk()$m_present_time),
                         end = max(fullehunk()$m_present_time), 
                         min = min(fullehunk()$m_present_time), 
                         max = max(fullehunk()$m_present_time))
  })
  
  plotehunk <- reactive({
    req(input$echohistrange)
    
    pf <- filter(fullehunk(), m_present_time >= input$echohistrange[1] & m_present_time <= input$echohistrange[2]) %>%
      filter(hour >= input$echohour[1] & hour <= input$echohour[2]) %>%
      group_by(segment, r_depth) %>%
      mutate(avgDb = mean(value)) %>%
      ungroup() %>%
      group_by(segment) %>%
      mutate(seg_time = mean(m_present_time)) %>%
      ungroup() %>%
      mutate(seg_hour = hour(seg_time)) %>%
      mutate(cycle = case_when(seg_hour %in% c(11:23) ~ 'day',
                               seg_hour %in% c(1:10, 24) ~ 'night')) # add day/night filter
    
    pf
  })
  
  plotethunk <- reactive({
    req(input$echohistrange2)
    
    pf <- filter(fullehunk(), m_present_time >= input$echohistrange2[1] & m_present_time <= input$echohistrange2[2]) %>%
      filter(hour >= input$echohour2[1] & hour <= input$echohour2[2]) %>%
      group_by(segment, r_depth) %>%
      mutate(avgDb = mean(value)) %>%
      ungroup() %>%
      group_by(segment) %>%
      mutate(seg_time = mean(m_present_time)) %>%
      ungroup() %>%
      mutate(seg_hour = hour(seg_time)) %>%
      mutate(cycle = case_when(seg_hour %in% c(11:23) ~ 'day',
                               seg_hour %in% c(1:10, 24) ~ 'night')) # add day/night filter
    
    pf
  })
  
  #### frequency polygon ####
  gg5 <- reactive({
    req(input$echohistrange)
    
    ggHist <- ggplot(data = plotehunk(),
                     aes(y = r_depth,
                     )) +
      geom_freqpoly(aes(colour = as.factor(cycle)),
                    binwidth = input$depthbin
      ) +
      scale_y_reverse() +
      facet_wrap(as.factor(plotehunk()$value),
                 scales = "free_x",
                 ncol = 4) + 
      theme_bw() +
      labs(title = "Counts of Return Strength at Depth by Period",
           y = "Depth (m)",
           x = "Counts",
           color = "Period",
           caption = "Day = 1100-2300 UTC") +
      theme(plot.title = element_text(size = 24),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12),
            legend.key = element_blank(),
            plot.caption = element_markdown(),
      ) +
      guides(size="none")
    
    # ggHist <-
    #   ggplot(data = plotehunk(),
    #          aes(x = as.factor(value),
    #              y = r_depth,
    #              fill = hour,
    #          )) +
    #   geom_tile() +
    #   #coord_equal() +
    #   scale_fill_viridis_c() +
    #   scale_y_reverse() +
    #   theme_bw() +
    #   labs(title = paste0("Frequency of Returns at Depth from ", input$echohistrange[1], " to ", input$echohistrange[2]),
    #        y = "Depth (m)",
    #        #x = "Date/Time (UTC)",
    #        x = "dB",
    #        fill = "Hour (UTC)") +
    #   theme(plot.title = element_text(size = 32),
    #         axis.title = element_text(size = 16),
    #         axis.text = element_text(size = 12),
    #         legend.key = element_blank(),
    #         plot.caption = element_markdown()) +
    #   guides(size="none")
    #scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))
    
    ggHist
    
  })
  
  output$echoHist <- renderPlot({gg5()})
  
  
  #### pseudotimegram ####
  gg6 <- reactive({
    
    ggEchoTime <- 
      ggplot(data = plotethunk(),
             aes(x = seg_time,
                 y = r_depth,
                 colour = avgDb,
             )) +
      geom_point(size = 4,
                 pch = 15
      ) +
      #coord_equal() +
      #scale_color_viridis_c() +
      scale_y_reverse() +
      theme_bw() +
      labs(title = paste0("Avg dB returns (per meter) at depth from ", input$echohistrange2[1], " to ", input$echohistrange2[2]),
           y = "Depth (m)",
           #x = "Date/Time (UTC)",
           x = "Date",
           colour = "average dB") +
      theme(plot.title = element_text(size = 32),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12),
            legend.key = element_blank(),
            plot.caption = element_markdown()) +
      guides(size="none")
    
    if (input$echoColor2 == "EK") {
      ggEchoTime +
        scale_colour_gradientn(colours = c("#9F9F9F", "#5F5F5F", "#0000FF", "#00007F", "#00BF00", "#007F00",
                                                    "#FF1900", "#FF7F00","#FF00BF", "#FF0000", "#A65300", "#783C28"),
                                                    limits = c(-75, -30)
        )
    } else if (input$echoColor2 == "magma") {
      ggEchoTime +
        scale_colour_viridis_c(limits = c(-75, -30),
                               option = "C"
        )
    } else {
      ggEchoTime +
        scale_colour_viridis_c(limits = c(-75, -30),
                               option = "D")
    }
    
  })
  
  output$echoTime <- renderPlot({gg6()})
  
  
    }
    
  })
  
  
  
  ####### archived flight data ########
  
  fileList_archive <- list.files(path = "./Data/",
                                 pattern = "*.rds")
  
  missionList_archive <- str_remove(fileList_archive, pattern = ".rds")
  
  updateSelectInput(session, "mission", NULL, choices = c(missionList_archive), selected = tail(missionList_archive, 1))
  
  #mission map 
  output$missionmap <- renderLeaflet({

    #grab .kml per mission number
    raw_sf <- st_read(paste0("./KML/", input$mission, ".kml"),
                      layer = "Surfacings")
    
    #pull out only relevant portion
    KML_sf <- raw_sf %>%
      select(Name) #timestamps
    
    #get map from sf
    map_sf <- KML_sf[2:(nrow(KML_sf) - 1),]
    
    #convert to long form for start/end markers later
    mapUp <- KML_sf %>%
      mutate(long = st_coordinates(.)[,1],
             lat = st_coordinates(.)[,2]) %>%
      st_drop_geometry()
    
    leaflet() %>%
      #base provider layers
      addProviderTiles("Esri.OceanBasemap", 
                       group = "Ocean Basemap") %>%
      addProviderTiles("Esri.WorldImagery", 
                       group = "World Imagery") %>%
      addLayersControl(baseGroups = c('Ocean Basemap', 'World Imagery')) %>%
      #timestamps for surfacings
      addCircles(data = map_sf,
                 color = "gold",
                 popup = map_sf$Name
      ) %>%
      #start marker
      addAwesomeMarkers(
        lat = mapUp[1, 3],
        lng = mapUp[1, 2],
        label = "Starting point",
        icon = icon.start
      ) %>%
      #end marker
      addAwesomeMarkers(
        lat = mapUp[nrow(mapUp), 3],
        lng = mapUp[nrow(mapUp), 2],
        label = "Ending point",
        icon = icon.end
      )
  })
  
  missionNum <- reactiveValues()
  
  glider <- reactiveValues()
  
observeEvent(input$load, {
   #req(input$load)
    #on load add salinty + SV
    df <- readRDS(paste0("./Data/", isolate(input$mission), ".rds")) %>%
      mutate(osg_salinity = ec2pss(sci_water_cond*10, sci_water_temp, sci_water_pressure*10)) %>%
      mutate(osg_theta = theta(osg_salinity, sci_water_temp, sci_water_pressure)) %>%
      mutate(osg_rho = rho(osg_salinity, osg_theta, sci_water_pressure)) %>%
      mutate(soundvel1 = c_Coppens1981(m_depth,
                                       osg_salinity,
                                       sci_water_temp))
    #possible add ... from masterdata
    #mutate(new_water_depth = m_water_depth * (1500/soundvel1))
    
    #pull out science variables
    scivars <- df %>%
      select(starts_with(c("sci","osg"))) %>%
      colnames()
    
    #pull out flight variables
    flightvars <- df %>%
      select(!starts_with("sci")) %>%
      colnames()
    
    #commit mission number to reactive val at load
    missionNum$id <- isolate(input$mission)
    
    #mission date range variables
    startDate <- min(df$m_present_time)
    endDate <- max(df$m_present_time)
    
    #get start/end days and update data filters
    updateDateInput(session, "date1", NULL, min = min(df$m_present_time), max = max(df$m_present_time), value = startDate)
    updateDateInput(session, "date2", NULL, min = min(df$m_present_time), max = max(df$m_present_time), value = endDate)
    updateSelectInput(session, "display_var", NULL, choices = c(scivars), selected = "sci_water_temp")
    updateSelectizeInput(session, "flight_var", NULL, choices = c(flightvars), selected = "m_roll")
    
    showNotification("Data loaded", type = "message")
    
    print(paste0(missionNum$id, " data loaded"))
    
    glider$full <- df
    
  })
  
  #dynamically filter for plotting
  chunk <- reactive({
    df <- glider$full %>%
      filter(m_present_time >= input$date1 & m_present_time <= input$date2) %>%
      #filter(status %in% c(input$status)) %>%
      #filter(!(is.na(input$display_var) | is.na(m_depth))) %>%
      filter(m_depth >= input$min_depth & m_depth <= input$max_depth)
    
    df

  })
  
  #ranges for plot zooms
  rangefli <- reactiveValues(x = NULL, y = NULL)
  rangesci <- reactiveValues(x = NULL, y = NULL)
  
  ########## science plot #########
  
  scienceChunk <- reactive({
    #req(input$display_var)
    
    select(chunk(), m_present_time, m_depth, input$display_var) %>%
      filter(!is.na(across(!c(m_present_time:m_depth))))
  })
  
  gg1 <- reactive({
    ggplot(data = 
             scienceChunk(),#dynamically filter the sci variable of interest
           aes(x=m_present_time,
               y=m_depth,
               z=.data[[input$display_var]])) +
      geom_point(
        aes(color = .data[[input$display_var]]),
        na.rm = TRUE
      ) +
      coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
      #geom_hline(yintercept = 0) +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(input$min, input$max)) +
      geom_point(data = filter(chunk(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.1,
                 na.rm = TRUE
      ) +
      theme_bw() +
      labs(title = paste0(missionNum$id, " Science Data"),
           y = "Depth (m)",
           x = "Date") +
      theme(plot.title = element_text(size = 32)) +
      theme(axis.title = element_text(size = 16)) +
      theme(axis.text = element_text(size = 12))
  })
  
  output$sciPlot <- renderPlot({gg1()})
  
  #science plot zoom/click
  observeEvent(input$sciPlot_dblclick, {
    brush <- input$sciPlot_brush
    if (!is.null(brush)) {
      rangesci$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      #REVERSED RANGE DUE TO REVERSED Y see: https://github.com/tidyverse/ggplot2/issues/4021
      rangesci$y <- c(brush$ymax, brush$ymin)
      
    } else {
      rangesci$x <- NULL
      rangesci$y <- NULL
    }
  })
  
  ##### flight plot #####
  
  flightChunk <- reactive({
    req(input$load)
    select(chunk(), m_present_time, all_of(input$flight_var)) %>%
      pivot_longer(
        cols = !m_present_time,
        names_to = "variable",
        values_to = "count") %>%
      filter(!is.na(count))
  })
  
  # output$summary <- renderPrint({
  #   head(flightChunk())
  # })
  
  #flight plot
  gg2 <- reactive({
    # if (input$flight_var == "m_roll") {
    #   flightxlabel <- "roll"
    # } else if (input$flight_var == "m_heading") {
    #   flightxlabel <- "heading"
    # }
    req(input$load)
    ggplot(
      data =
        flightChunk(),
      aes(x = m_present_time,
          y = count,
          color = variable,
          shape = variable)) +
      geom_point() +
      coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
      theme_grey() +
      labs(title = paste0(missionNum$id, " Flight Data"),
           x = "Date") +
      theme(plot.title = element_text(size = 32)) +
      theme(axis.title = element_text(size = 16)) +
      theme(axis.text = element_text(size = 12))
    
    # plotup <- list()
    # for (i in input$flight_var){
    #   plotup[[i]] = ggplot(data = select(chunk(), m_present_time, all_of(i)) %>%
    #     pivot_longer(
    #       cols = !m_present_time,
    #       names_to = "variable",
    #       values_to = "count") %>%
    #     filter(!is.na(count)),
    #     aes(x = m_present_time,
    #         y = count,
    #         color = variable,
    #         shape = variable)) +
    #     geom_point() +
    #     coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
    #     theme_minimal()
    # }
    # wrap_plots(plotup, ncol = 1)
  })
  
  output$fliPlot <- renderPlot({gg2()})
  
  #flight plot zoom/click
  observeEvent(input$fliPlot_dblclick, {
    brush <- input$fliPlot_brush
    if (!is.null(brush)) {
      rangefli$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      rangefli$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangefli$x <- NULL
      rangefli$y <- NULL
    }
  })
  
  ######### sound velocity plot ##########
  
  gg3 <- reactive({
    req(input$load)
    # create plot
    ggplot(data = filter(chunk(), !is.nan(soundvel1)),
           aes(x=m_present_time,
               y=m_depth,
               z=soundvel1)) +
      geom_point(
        aes(color = soundvel1)
      ) +
      geom_point(data = filter(chunk(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.1,
                 na.rm = TRUE
      ) +
      #geom_hline(yintercept = 0) +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(limits = c(input$soundmin, input$soundmax))) +
      theme_bw() +
      labs(title = paste0(missionNum$id, " Sound Velocity"),
           caption = "Calculated using Coppens <i>et al.</i> (1981)",
           y = "Depth (m)",
           x = "Date") +
      theme(plot.title = element_text(size = 32)) +
      theme(axis.title = element_text(size = 16)) +
      theme(axis.text = element_text(size = 12)) +
      theme(plot.caption = element_markdown())
    
  })
  
  output$souPlot <- renderPlot({gg3()})
  
  
  ###### download handlers #########
  
  output$downloadSciPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_sci.png")},
    content = function(file){
      ggsave(file,
             gg1(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadFliPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_fli.png")},
    content = function(file){
      ggsave(file,
             gg2(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadSouPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_SV.png")},
    content = function(file){
      ggsave(file,
             gg3(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadEchoPlot <- downloadHandler(
    filename = function(){paste(input$echo, "_pseudo.png")},
    content = function(file){
      ggsave(file,
             gg4(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadEchoHist <- downloadHandler(
    filename = function(){paste(input$echo, "_pseudo.png")},
    content = function(file){
      ggsave(file,
             gg5(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadEchoHist2 <- downloadHandler(
    filename = function(){paste(input$echo, "_pseudo.png")},
    content = function(file){
      ggsave(file,
             gg6(),
             width = 16,
             height = 9)
    }
  )
  
  ####### File Upload/Processing #########
  observeEvent(input$upload, {
    #get file extension
    ext <- tools::file_ext(input$upload$name)
    
    #if SSV
    if (ext == "ssv") {
      print("SSV!")
      newGlider <- ssv_to_rds(inputFile = input$upload$datapath,
                              missionNum = input$upload$name)
      session$reload()
      #if kml
    } else if (ext == "kml"){
      print("KML!")
      file.copy(input$upload$datapath, "./KML")
      session$reload()
      #file.rename(f)
    } else if (ext == "kmz"){
      showModal(modalDialog(
        title = "Warning",
        ".kmz is not accepted, .kml ONLY",
        easyClose = TRUE
      ))
      #otherwise, error
    } else {
      showModal(modalDialog(
        title = "Warning",
        "Please upload .ssv or .kml only",
        easyClose = TRUE
      ))
    }
    
    #topGlider <- head(newGlider)
    
    #showNotification(paste0(outputName, " saved"))
  })
  # output$uploadTable <- renderTable({
  #   req(input$upload)
  #   
  #   topGlider
  # })
  
  
  
}
