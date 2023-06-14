##### Main glider dashboard module #########
library(shiny)

fullData_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidPage(
      wellPanel(  actionButton(
        inputId = ns("load"),
        label = "Load Mission Data",
        icon("plane"),
        style =
          "color: #fff; background-color: #963ab7; border-color: #2e6da4"
      ),
      selectInput(
        inputId = ns("mission"),
        label = "Which mission data to display",
        choices = NULL,
        selected =  NULL
      ),
      br(),
      airDatepickerInput(
        inputId = ns("date1"),
        label = "Start Date:",
        value = NULL,
        range = FALSE,
        minDate = NULL,
        maxDate = NULL,
        update_on = "close",
        timepicker = TRUE,
        clearButton = TRUE
      ),
      airDatepickerInput(
        inputId = ns("date2"),
        label = "End Date:",
        value = NULL,
        range = FALSE,
        minDate = NULL,
        maxDate = NULL,
        update_on = "close",
        timepicker = TRUE,
        clearButton = TRUE
      ),
      numericInput(
        inputId = ns("min_depth"),
        label = "Depth Minimum",
        value = 3,
        min = 0,
        max = 1000
      ),
      numericInput(
        inputId = ns("max_depth"),
        label = "Depth Maximum",
        value = 1000,
        min = 0,
        max = 1000
      ),),
      column(12,
             #mainPanel(#science variable settings
             tabsetPanel(id = ns("tabs"),
               #width = 12,
               tabPanel(title = "Map",
                        leafletOutput(outputId = ns("missionmap"),
                                      height = "600px")),
               tabPanel(title = "Science Data",
                        column(3,
                               wellPanel(
                                 selectInput(
                                   inputId = ns("display_var"),
                                   label = "Which science variable to display",
                                   choices = NULL
                                 ),
                                 #title = "Color Scale Override",
                                 numericInput(inputId = ns("min"),
                                              label = "Sci Axis Minimum",
                                              NULL),
                                 numericInput(inputId = ns("max"),
                                              label = "Sci Axis Maximum",
                                              NULL),
                                 downloadButton('downloadSciPlot')
                               )),
                        column(
                          9,
                          #"Brush and double-click to zoom (double-click again to reset)",
                          plotOutput(
                            outputId = ns("sciPlot"),
                            dblclick = "sciPlot_dblclick",
                            brush = brushOpts(id = ns("sciPlot_brush"),
                                              resetOnNew = TRUE),
                            height = "600px"
                          ) %>% withSpinner(color="#0dc5c1")
                        )
               ),
               #flight variable settings
               tabPanel(title = "Flight Data",
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   #   selectInput(
                                   #     "flight_var",
                                   #     "Which flight variable(s) to display",
                                   #     choices = c(flightvars),
                                   #     selected = c("m_roll")
                                   #   )
                                   # )),
                                   selectizeInput(
                                     inputId = ns("flight_var"),
                                     label = "Which flight variable(s) to display",
                                     choices = NULL,
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button'))
                                   ),
                                   downloadButton('downloadFliPlot'),
                                   verbatimTextOutput("summary")
                                 )),
                          column(
                            9,
                            #h4("Brush and double-click to zoom (double-click again to reset)"),
                            plotOutput(
                              outputId = ns("fliPlot"),
                              dblclick = "fliPlot_dblclick",
                              brush = brushOpts(id = ns("fliPlot_brush"),
                                                resetOnNew = TRUE),
                              height = "600px"
                            ) %>% withSpinner(color="#0dc5c1")
                          )
                        )),
               #sound velocity tab
               tabPanel(title = "Sound Velocity",
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   numericInput(inputId = ns("soundmin"),
                                                label = "Sound Axis Minimum",
                                                NULL),
                                   numericInput(inputId = ns("soundmax"),
                                                label = "Sound Axis Maximum",
                                                NULL),
                                   downloadButton('downloadSouPlot')
                                 )),
                          column(9,
                                 plotOutput(outputId = ns("souPlot"),
                                            height = "600px"
                                 ) %>% withSpinner(color="#0dc5c1")
                          )
                        ),),
               selected = "Map"
             )
      )
    )
  )
}

fullData_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    fileList_archive <- list.files(path = "./Data/",
                                   pattern = "*.RData")
    
    missionList_archive <- str_remove(fileList_archive, pattern = ".RData")
    
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
        addPolylines(
          lat = mapUp$lat,
          lng = mapUp$long,
          color = "grey",
          weight = 3,
          opacity = 1,
        ) %>%
        #timestamps for surfacings
        addCircles(data = map_sf,
                   color = "gold",
                   popup = map_sf$Name,
                   weight = 3
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
    
    gliderReactor <- reactiveValues()
    
    observeEvent(input$load, {
      #req(input$load)
      #on load add salinty + SV
      load(paste0("./Data/", isolate(input$mission), ".RData"))
      
      gliderReactor <- gliderName
      
      df <- gliderdf 
      
      # df <- readRDS(paste0("./Data/", isolate(input$mission), ".RData")) %>%
      #   mutate(osg_salinity = ec2pss(sci_water_cond*10, sci_water_temp, sci_water_pressure*10)) %>%
      #   mutate(osg_theta = theta(osg_salinity, sci_water_temp, sci_water_pressure)) %>%
      #   mutate(osg_rho = rho(osg_salinity, osg_theta, sci_water_pressure)) %>%
      #   mutate(soundvel1 = c_Coppens1981(m_depth,
      #                                    osg_salinity,
      #                                    sci_water_temp))
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
      startDate <- as_datetime(min(df$m_present_time))
      endDate <- as_datetime(max(df$m_present_time))
      
      #get start/end days and update data filters
      updateAirDateInput(session, "date1", NULL, value = startDate, 
                         options = list(minDate = startDate, maxDate = endDate))
      updateAirDateInput(session, "date2", NULL, value = endDate, 
                         options = list(minDate = startDate, maxDate = endDate))
      updateSelectInput(session, "display_var", NULL, choices = c(scivars), selected = "sci_water_temp")
      updateSelectizeInput(session, "flight_var", NULL, choices = c(flightvars), selected = "m_roll")
      
      showNotification("Data loaded", type = "message")
      
      print(paste0(missionNum$id, " data loaded"))
      
      glider$full <- df
      
      if (gliderName == "usf-stella"){
        appendTab(inputId = "tabs",
                  tabPanel(title = "Pseudograms",
                           column(2,
                                  wellPanel(
                                    selectInput(
                                      inputId = ns("echo"),
                                      label = "Which pseudogram to display",
                                      choices = NULL,
                                      selected =  NULL
                                    ),
                                    selectInput(
                                      inputId = ns("echoColor"),
                                      label = "Color scheme",
                                      choices = c("EK", "magma", "viridis"),
                                      selected =  "EK"
                                    ),
                                    #downloadButton('downloadEchoPlot')
                                  )),
                           column(10,
                                  plotOutput(
                                    outputId = ns("echoPlot"),
                                    #dblclick = "fliPlot_dblclick",
                                    #brush = brushOpts(id = "fliPlot_brush",
                                    #                  resetOnNew = TRUE),
                                    #height = "600px"
                                  ),
                                  hr(),
                                  column(3,
                                         actionButton(
                                           inputId = ns("oldestPgram"),
                                           label = "Oldest",
                                           #icon("boat"),
                                           style =
                                             "color: #fff; background-color: #000000; border-color: #2e6da4"
                                         ), align = "center"),
                                  column(3,
                                         actionButton(
                                           inputId = ns("prevPgram"),
                                           label = "Previous",
                                           #icon("boat"),
                                           style =
                                             "color: #fff; background-color: #000000; border-color: #2e6da4"
                                         ), align = "center"),
                                  column(3,
                                         actionButton(
                                           inputId = ns("nextPgram"),
                                           label = "Next",
                                           #icon("boat"),
                                           style =
                                             "color: #fff; background-color: #000000; border-color: #2e6da4"
                                         ), align = "center"),
                                  column(3,
                                         actionButton(
                                           inputId = ns("latestPgram"),
                                           label = "Latest",
                                           #icon("boat"),
                                           style =
                                             "color: #fff; background-color: #000000; border-color: #2e6da4"
                                         ), align = "center"),
                           )),
                  tabPanel(title = "Pseudotimegram",
                           column(2,
                                  wellPanel(
                                    actionButton(
                                      inputId = ns("fullecho2"),
                                      label = "Plot!",
                                      #icon("boat"),
                                      style =
                                        "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                                    ),
                                    dateRangeInput(ns("echohistrange2"), "Date range:",
                                                   start  = NULL,
                                                   end    = NULL,
                                                   min    = NULL,
                                                   max    = NULL,
                                                   format = "mm/dd/yy",
                                                   separator = " - "),
                                    sliderInput(ns("echohour2"),
                                                "Hour:",
                                                min = 0,  max = 24, value = c(0, 24)),
                                    selectInput(
                                      inputId = ns("echoColor2"),
                                      label = "Color scheme",
                                      choices = c("EK", "magma", "viridis"),
                                      selected =  "EK"
                                    ),
                                    checkboxGroupInput(
                                      inputId = ns("todTgram"),
                                      label = "Time of day",
                                      choices = c("day", "night"),
                                      selected = c("day", "night")
                                    ),
                                    #downloadButton('downloadEchoHist2')
                                  )),
                           column(10,
                                  plotOutput(
                                    outputId = ns("echoTime"),
                                    #dblclick = "fliPlot_dblclick",
                                    #brush = brushOpts(id = "fliPlot_brush",
                                    #                  resetOnNew = TRUE),
                                    #height = "600px"
                                  ) %>% withSpinner(color="#0dc5c1")
                           )),
                  tabPanel(title = "Frequency Polygon",
                           column(2,
                                  wellPanel(
                                    actionButton(
                                      inputId = ns("fullecho"),
                                      label = "Plot!",
                                      #icon("boat"),
                                      style =
                                        "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                                    ),
                                    dateRangeInput(ns("echohistrange"), "Date range:",
                                                   start  = NULL,
                                                   end    = NULL,
                                                   min    = NULL,
                                                   max    = NULL,
                                                   format = "mm/dd/yy",
                                                   separator = " - "),
                                    numericInput(
                                      inputId = ns("depthbin"),
                                      label = "Depth Bin Size",
                                      value = 3,
                                      min = 1,
                                      max = 1000
                                    ),
                                    sliderInput(ns("echohour"),
                                                "Hour:",
                                                min = 0,  max = 24, value = c(0, 24)),
                                    #downloadButton('downloadEchoHist')
                                  )),
                           column(10,
                                  plotOutput(
                                    outputId = ns("echoHist"),
                                    #dblclick = "fliPlot_dblclick",
                                    #brush = brushOpts(id = "fliPlot_brush",
                                    #                  resetOnNew = TRUE),
                                    #height = "600px"
                                  ) %>% withSpinner(color="#0dc5c1")
                           ))
        )
      }
      
    })
    
    #dynamically filter for plotting
    chunk <- reactive({
      soFar <- interval(input$date1, input$date2)
      
      df <- glider$full %>%
        filter(m_present_time %within% soFar) %>%
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
    
    
  })
}
