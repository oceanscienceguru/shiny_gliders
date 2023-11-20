##### Main glider dashboard module #########
library(shiny)

fullData_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidPage(
      box(  actionButton(
        inputId = ns("load"),
        label = "Load Mission Data",
        icon("plane"),
        style =
          "color: #fff; background-color: #963ab7; border-color: #2e6da4"
      ),
      br(),
      selectInput(
        inputId = ns("mission"),
        label = "Which mission data to display",
        choices = NULL,
        selected =  NULL
      ),
      ),
      box(
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
        value = 1,
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
                                 checkboxInput(
                                   inputId = ns("zeroFilter"),
                                   label = "Filter data > 0?",
                                   value = TRUE
                                 ),
                                 h4("Color Scale Override"),
                                 numericInput(inputId = ns("min"),
                                              label = "Sci Axis Minimum",
                                              NULL),
                                 numericInput(inputId = ns("max"),
                                              label = "Sci Axis Maximum",
                                              NULL),
                                 #downloadButton('downloadSciPlot')
                               )),
                        column(
                          9,
                          #"Brush and double-click to zoom (double-click again to reset)",
                          plotOutput(
                            outputId = ns("sciPlot")
                            # dblclick = "sciPlot_dblclick",
                            # brush = brushOpts(id = ns("sciPlot_brush"),
                            #                   resetOnNew = TRUE),
                            #height = "600px"
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
                                   #downloadButton('downloadFliPlot'),
                                   verbatimTextOutput("summary")
                                 )),
                          column(
                            9,
                            #h4("Brush and double-click to zoom (double-click again to reset)"),
                            plotlyOutput(
                              outputId = ns("fliPlot")
                              # dblclick = "fliPlot_dblclick",
                              # brush = brushOpts(id = ns("fliPlot_brush"),
                              #                   resetOnNew = TRUE),
                              # height = "600px"
                            ) %>% withSpinner(color="#0dc5c1")
                          )
                        )),
               #sound velocity tab
               tabPanel(title = "Derived Data",
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   selectInput(inputId = ns("derivedType"),
                                               label = "Which type of plot?",
                                               choices = c("Salinity", "Density", "SV Plot", "TS Plot"), selected = "Salinity"),
                                   # numericInput(inputId = "derivedmaxLive",
                                   #              label = "Axis Maximum",
                                   #              NULL),
                                   #downloadButton('downloadSouPlot')
                                 )),
                          column(9,
                                 plotlyOutput(outputId = ns("tsPlot"),
                                            #height = "600px"
                                 ) %>% withSpinner(color="#0dc5c1")
                          )
                        ),),
               #yo by yo
               tabPanel(title = "Yo by Yo",
                        column(2,
                               wellPanel(
                                 h4("Which yo to display?"),
                                 airDatepickerInput(
                                   inputId = ns("yoDate"),
                                   label = "Time of interest:",
                                   value = NULL,
                                   range = FALSE,
                                   minDate = NULL,
                                   maxDate = NULL,
                                   update_on = "close",
                                   timepicker = TRUE,
                                   clearButton = TRUE
                                 ),
                                 numericInput(
                                   inputId = ns("yo"),
                                   label = "Which yo to display",
                                   value = NA
                                   #selected =  NULL
                                 ),
                                 br(),
                                 # Button
                                 downloadButton(
                                   outputId = ns("yoDown"), 
                                   label = "Download plotted data"),
                                 br(),
                                 checkboxGroupInput(
                                   inputId = ns("cast"),
                                   label = "Downcast or Upcast?",
                                   choices = c("Downcast", "Upcast"),
                                   selected =  "Downcast"
                                 ),
                                 checkboxInput(
                                   inputId = ns("zeroFilterYo"),
                                   label = "Filter data > 0?",
                                   value = TRUE
                                 ),
                                 # selectInput(
                                 #   inputId = ns("yo_var"),
                                 #   label = "Which variable to display",
                                 #   choices = NULL
                                 # ),
                                 selectizeInput(
                                   inputId = ns("yo_var"),
                                   label = "Which variable to display",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = list(plugins = list('remove_button'))
                                 ),
                               )),
                        column(10,
                               plotlyOutput(
                                 outputId = ns("yoPlot"),
                                 #dblclick = "fliPlot_dblclick",
                                 #brush = brushOpts(id = "fliPlot_brush",
                                 #                  resetOnNew = TRUE),
                                 #height = "600px"
                               ),
                               hr(),
                               column(3,
                                      actionButton(
                                        inputId = ns("oldestYo"),
                                        label = "Oldest",
                                        #icon("boat"),
                                        style =
                                          "color: #fff; background-color: #000000; border-color: #2e6da4"
                                      ), align = "center"),
                               column(3,
                                      actionButton(
                                        inputId = ns("prevYo"),
                                        label = "Previous",
                                        #icon("boat"),
                                        style =
                                          "color: #fff; background-color: #000000; border-color: #2e6da4"
                                      ), align = "center"),
                               column(3,
                                      actionButton(
                                        inputId = ns("nextYo"),
                                        label = "Next",
                                        #icon("boat"),
                                        style =
                                          "color: #fff; background-color: #000000; border-color: #2e6da4"
                                      ), align = "center"),
                               column(3,
                                      actionButton(
                                        inputId = ns("latestYo"),
                                        label = "Latest",
                                        #icon("boat"),
                                        style =
                                          "color: #fff; background-color: #000000; border-color: #2e6da4"
                                      ), align = "center"),
                        ),
               ),
               #data explorer tab
               tabPanel(title = "Data Explorer",
                        fluidRow(
                          column(3,
                                 wellPanel(
                                   selectInput(inputId = ns("exploreVar1"),
                                               label = "x-axis variable",
                                               choices = NULL,
                                               selected = NULL),
                                   selectInput(inputId = ns("exploreVar2"),
                                               label = "y-axis variable",
                                               choices = NULL, 
                                               selected = NULL),
                                   # numericInput(inputId = ns("exploreMin"),
                                   #              label = "x-axis minimum",
                                   #              NULL),
                                   # numericInput(inputId = ns("exploreMax"),
                                   #              label = "x-axis maximum",
                                   #              NULL),
                                   checkboxInput(inputId = ns("exploreRevAxis"),
                                                 label = "Invert y-axis"),
                                   checkboxInput(inputId = ns("exploreSmooth"),
                                                 label = "Smooth")
                                   # numericInput(inputId = "derivedmaxLive",
                                   #              label = "Axis Maximum",
                                   #              NULL),
                                   #downloadButton('downloadSouPlot')
                                 )),
                          column(9,
                                 plotOutput(outputId = ns("explorePlot"),
                                            #height = "600px"
                                 ) %>% withSpinner(color="#0dc5c1")
                          )
                        )
                        ),
               #sound velocity tab
               # tabPanel(title = "Sound Velocity",
               #          fluidRow(
               #            column(3,
               #                   wellPanel(
               #                     numericInput(inputId = ns("soundmin"),
               #                                  label = "Sound Axis Minimum",
               #                                  NULL),
               #                     numericInput(inputId = ns("soundmax"),
               #                                  label = "Sound Axis Maximum",
               #                                  NULL),
               #                     downloadButton('downloadSouPlot')
               #                   )),
               #            column(9,
               #                   plotOutput(outputId = ns("souPlot"),
               #                              height = "600px"
               #                   ) %>% withSpinner(color="#0dc5c1")
               #            )
               #          ),),
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
                        )),
               selected = "Map"
             )
      )
    )
  )
}

fullData_server <- function(id, clientTZ) {
  
  moduleServer(id, function(input, output, session) {
    
    fileList_archive <- list.files(path = "./Data/",
                                   pattern = "*.RData")
    
    missionList_archive <- str_remove(fileList_archive, pattern = ".RData")
    
    updateSelectInput(session, "mission", NULL, choices = c(missionList_archive), selected = tail(missionList_archive, 1))
    
    #mission map 
    output$missionmap <- renderLeaflet({
      req(input$mission)
      
      if(file.exists(paste0("./KML/", input$mission, ".kml"))){
      #grab .kml per mission number
      raw_sf <- st_read(paste0("./KML/", input$mission, ".kml"),
                        layer = "Surfacings")
      
      # raw_sf <- st_read(paste0("./thebrewery/KML/", "M112", ".kml"),
      #                   layer = "Surfacings")
      
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
      } else {
        mapUp <- read.csv(paste0("./KML/", input$mission, ".csv")) %>%
          select(m_present_time, long, lat)
        #mapUp2 <- read.csv(paste0("./thebrewery/KML/", "M103_usf-bass", ".csv"))
        
        map_sf <- mapUp %>%
          mutate(Name = m_present_time)
      }
      
      leaflet() %>%
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
    
    gliderReactor <- reactiveValues(name = NULL)
    
    selectPgram <- reactiveValues(seg = NULL, id = NULL)
    
    observeEvent(input$load, {
      load(paste0("./Data/", isolate(input$mission), ".RData"))
      
      gliderReactor$name <- gliderName
      
      df <- gliderdf
      
      #yoList$ids <- sort(na.omit(unique(df$yo_id)))

      #possible add ... from masterdata
      #mutate(new_water_depth = m_water_depth * (1500/soundvel1))
      
      allvars <- colnames(df)
      
      yoNums <- sort(na.omit(unique(df$yo_id)))

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
      
      updateSelectInput(session, "exploreVar1", NULL, choices = c(allvars), selected = "m_present_time")
      updateSelectInput(session, "exploreVar2", NULL, choices = c(allvars), selected = head(allvars, 1))
      
      updateAirDateInput(session, "yoDate", NULL, value = endDate, 
                         options = list(minDate = startDate, maxDate = endDate,
                                        timeFormat = "HH:mm"))
      updateNumericInput(session, "yo", NULL, min = 0, max = max(yoNums), value = max(yoNums))
      updateSelectInput(session, "yo_var", NULL, choices = c(scivars), selected = tail(scivars, 1))
      
      showNotification("Data loaded", type = "message")
      
      message(paste0(missionNum$id, " data loaded"))
      
      glider$full <- df
      
      if(gliderName == "usf-stella"){
        updateSelectInput(session, "echo", NULL, choices = c(echoListraw$value), selected = tail(echoListraw$value, 1)) 
        updateDateRangeInput(session, "echohistrange", label = NULL,
                             start = (max(fullehunk$m_present_time)-259200),
                             end = max(fullehunk$m_present_time),
                             min = min(fullehunk$m_present_time),
                             max = max(fullehunk$m_present_time))
        
        updateDateRangeInput(session, "echohistrange2", label = NULL,
                             start = min(fullehunk$m_present_time),
                             end = max(fullehunk$m_present_time),
                             min = min(fullehunk$m_present_time),
                             max = max(fullehunk$m_present_time))

        
        observeEvent(input$echo, {
          selectPgram$seg <- input$echo
          selectPgram$id <- match(input$echo, echoListraw$value)
        })
        
        }
      
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
      
      plotehunk <- reactive({
        validate(
          need(gliderReactor$name == "usf-stella", "These data require echosounder glider")
        )
        req(input$echohistrange)
        
        pf <- filter(fullehunk, m_present_time >= input$echohistrange[1] & m_present_time <= input$echohistrange[2]) %>%
          filter(hour >= input$echohour[1] & hour <= input$echohour[2]) %>%
          group_by(segment, r_depth) %>%
          mutate(avgDb = exp(mean(log(abs(value))))*-1) %>%
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
        validate(
          need(gliderReactor$name == "usf-stella", "These data require echosounder glider")
        )
        req(input$echohistrange2)
        
        pf <- filter(fullehunk, m_present_time >= input$echohistrange2[1] & m_present_time <= input$echohistrange2[2]) %>%
          filter(hour >= input$echohour2[1] & hour <= input$echohour2[2]) %>%
          group_by(segment) %>%
          mutate(seg_time = mean(m_present_time)) %>%
          ungroup() %>%
          mutate(seg_hour = hour(seg_time)) %>%
          mutate(cycle = case_when(seg_hour %in% c(11:23) ~ 'day',
                                   seg_hour %in% c(1:10, 24) ~ 'night')) %>% # add day/night filter
          filter(cycle %in% input$todTgram) %>%
          group_by(segment, r_depth, cycle) %>%
          mutate(avgDb = exp(mean(log(abs(value))))*-1) %>%
          #mutate(avgDbOLD = mean(value)) %>%
          ungroup()
        
        
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
        # if(input$todTgram != "day/night") {
        #   filter(plotethunk(), cycle %in% input$todTgram)
        # } else { plotethunk()
        # },

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
      
      
      
    })
    #   print(isolate(gliderReactor$name))
    # if(isolate(input$mission) == "usf-stella"){
    #   updateSelectInput(session, "echo", NULL, choices = c(echoListraw$value), selected = tail(echoListraw$value, 1))
    #   
    
 
      
      #process the requested pseudogram
      ehunk <- reactive({
        validate(
          need(gliderReactor$name == "usf-stella", "These data require echosounder glider")
        )
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
      

      
      
      #### pseudotimegram main setup ####
      # fullehunk <- reactive({
      #   req(input$fullecho | input$fullecho2)
      #
      #   elist <- list()
      #   for (i in echoListraw$value) {
      #     elist[[i]] <- pseudogram(paste0("/echos/layers/", i, ".ssv"),
      #                              paste0("/echos/depths/", i, ".ssv"))
      #   }
      #   ef <- bind_rows(elist, .id = "segment") %>%
      #     mutate(r_depth = round(q_depth, 0)) %>%
      #     mutate(day = day(m_present_time)) %>%
      #     mutate(hour = hour(m_present_time))
      #
      #   ef
      #
      # })
      
      
      

 
      
    # }
    
    
    #dynamically filter for plotting
    chunk <- reactive({
      #potential workaround for airdate picker hijacking broswer tz
      if(clientTZ != 0){
        soFar <- interval(force_tz(input$date1 - hours(clientTZ)), force_tz(input$date2 - hours(clientTZ), "UTC"))
      } else {
        soFar <- interval(input$date1, input$date2)
      }
      
      #soFar <- interval(input$date1, input$date2)
      
      df <- glider$full %>%
        filter(m_present_time %within% soFar) %>%
        #filter(status %in% c(input$status)) %>%
        #filter(!(is.na(input$display_var) | is.na(m_depth))) %>%
        filter(osg_i_depth >= input$min_depth & osg_i_depth <= input$max_depth)
      
      df
      
    })
    
    ########## science plot #########
    
    scienceChunk <- reactive({
      validate(
        need(gliderReactor$name != "", "Please click the Load Mission Data button")
      )
      #req(input$display_var)
      
      qf <- chunk() %>%
        select(m_present_time, osg_i_depth, input$display_var) %>%
        filter(!is.na(across(!c(m_present_time:osg_i_depth))))
      
      if(isTRUE(input$zeroFilter)){
        
        zf <- qf %>%
          filter(.data[[input$display_var]] > 0)
        
        zf
      } else {
        qf
      }
      
    })
    
    gg1 <- reactive({
            validate(
        need(gliderReactor$name != "", "Please click the Load Mission Data button")
      )
      
      scf <- scienceChunk() %>%
        select(m_present_time, osg_i_depth, .data[[input$display_var]]) %>%
        filter(!is.na(.data[[input$display_var]]))
      
      gcf <- chunk() %>%
        select(m_present_time, m_water_depth) %>%
        filter(!is.na(m_water_depth))
      
      # sciPlot(
      #   gliderName = missionNum$id,
      #   inGliderdf = scf,
      #   gliderFlightdf = gcf,
      #   plotVar = input$display_var,
      #   colorMin = input$min,
      #   colorMax = input$max
      # )
      
      #w = 0.02 * as.numeric((max(scf$m_present_time)-min(scf$m_present_time)))
      #h = 0.02 * (max(scf$osg_i_depth)-min(scf$osg_i_depth))
      
      baseSci <- ggplot(data = 
                          scf,
                        aes(x=m_present_time,
                            y=round(osg_i_depth, 2))) +
      
      # if (nrow(scf) > 2000000){
      #   message("big data")
      #   geom_tile(aes(fill = .data[[input$display_var]]),
      #             width = 2000,
      #             height = 8
      #   ) 
      # } else {
        geom_point(aes(color = .data[[input$display_var]]),
                   size = 2) 
      # }
      
      fullSci <- baseSci +
        scale_y_reverse() +
        geom_point(data = filter(gcf, m_water_depth > 0),
                   aes(x = m_present_time,
                       y = m_water_depth),
                   color = "black",
                   size = 0.3,
                   na.rm = TRUE
        ) +
        theme_bw() +
        labs(title = paste0(missionNum$id, " delayed data"),
             y = "Depth (m)",
             x = "Date") +
        theme(plot.title = element_text(size = 24)) +
        theme(axis.title = element_text(size = 16)) +
        theme(axis.text = element_text(size = 12)) +
        
        if (input$display_var == "sci_water_temp") {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "thermal") 
        } else if (input$display_var == "sci_water_pressure") {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "deep")
        } else if (input$display_var == "sci_water_cond") {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "haline")
        } else if (input$display_var == "sci_suna_nitrate_concentration") {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "tempo") 
        } else if (input$display_var == "sci_flbbcd_chlor_units" |
                   input$display_var == "sci_bbfl2s_chlor_scaled" ) {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "algae") 
        } else if (input$display_var == "sci_flbbcd_cdom_units" |
                   input$display_var == "sci_bbfl2s_cdom_scaled" ) {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "matter") 
        } else if (input$display_var == "sci_flbbcd_bb_units" |
                   input$display_var == "sci_bbfl2s_bb_scaled" ) {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "turbid") 
        } else if (input$display_var == "sci_oxy3835_oxygen" |
                   input$display_var == "sci_oxy4_oxygen" ) {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "oxy") 
        } else if (startsWith(input$display_var, "sci_ocr")) {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "solar") 
        } else if (input$display_var == "osg_soundvel1") {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "speed") 
        } else if (input$display_var == "osg_rho") {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "dense") 
        } else if (input$display_var == "osg_salinity") {
          scale_color_cmocean(limits = c(input$min, input$max),
                              name = "haline") 
        } else {
          scale_color_viridis_c(limits = c(input$min, input$max))
        }
      
      fullSci
      
    }) 
    
    output$sciPlot <- renderPlot({gg1()}) 
     #bindCache(missionNum$id, input$display_var)
    
    
    ##### flight plot #####
    
    flightChunk <- reactive({
      validate(
        need(gliderReactor$name != "", "Please click the Load Mission Data button")
      )
      
      select(chunk(), m_present_time, osg_i_depth, any_of(input$flight_var)) 
      # %>%
      #   pivot_longer(
      #     cols = !m_present_time,
      #     names_to = "variable",
      #     values_to = "count") %>%
      #   filter(!is.na(count))
    })
    
    # output$summary <- renderPrint({
    #   head(flightChunk())
    # })
    
    #flight plot
    gg2 <- reactive({
      
      req(input$flight_var)
      
      fliPlot(gliderName = missionNum$id,
              inGliderdf = flightChunk(),
              plotVar = input$flight_var)
      
      # if (input$flight_var == "m_roll") {
      #   flightxlabel <- "roll"
      # } else if (input$flight_var == "m_heading") {
      #   flightxlabel <- "heading"
      # }
      #req(input$load)
      # ggplot(
      #   data =
      #     flightChunk(),
      #   aes(x = m_present_time,
      #       y = count,
      #       color = variable,
      #       shape = variable)) +
      #   geom_point() +
      #   coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
      #   theme_bw() +
      #   labs(title = paste0(missionNum$id, " Flight Data"),
      #        x = "Date") +
      #   theme(plot.title = element_text(size = 32)) +
      #   theme(axis.title = element_text(size = 16)) +
      #   theme(axis.text = element_text(size = 12))
      
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
    
    output$fliPlot <- renderPlotly({gg2()})
    
    ##### derived plots #########
    gg3 <- reactive({
      validate(
        need(gliderReactor$name != "", "Please click the Load Mission Data button")
      )
      
      if (input$derivedType == "TS Plot"){
        df <- filter(chunk(), osg_salinity > 0)
        wf <- filter(chunk(), m_water_depth > 0)
        
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
        
        plot <- ggplotly(plot)
      }
      
      if (input$derivedType == "SV Plot"){
        df <- filter(chunk(), osg_soundvel1 > 0)
        wf <- filter(chunk(), m_water_depth > 0)
        
       plot <- sciPlot(
          missionNum$id,
          df,
          wf,
          input$derivedType
        )

      }
      
      if (input$derivedType == "Density"){
        df <- filter(chunk(), osg_rho > 0)
        wf <- filter(chunk(), m_water_depth > 0)
        
       plot <- sciPlot(
          missionNum$id,
          df,
          wf,
          input$derivedType
        )
        
      
      }
      
      if (input$derivedType == "Salinity"){
        df <- filter(chunk(), osg_salinity > 0)
        wf <- filter(chunk(), m_water_depth > 0)
        
       plot <- sciPlot(
          missionNum$id,
          df,
          wf,
          input$derivedType
        )
        
  
      }
      
      plot
      
    })
    
    output$tsPlot <- renderPlotly({gg3()})
    
    #### yo by yo ####
    
    #date matching to find yo in time
    observeEvent(input$yoDate, {
      yoFinder <- glider$full %>%
        slice(which.min(abs(m_present_time - input$yoDate)))
      
      selectYo$id <- yoFinder$yo_id
      updateNumericInput(session, "yo", NULL, value = yoFinder$yo_id)
    })
    
    yoChunk <- reactive({
      req(input$yo)
      
      qf <- glider$full %>%
        filter(yo_id == input$yo) %>% #grab only yo of interest
        filter(cast %in% input$cast) %>% #grab cast as needed
        select(c(m_present_time, m_water_depth, osg_i_depth, yo_id, i_lat, i_lon, any_of(input$yo_var))) 
      #filter(!is.na(across(!c(m_present_time:osg_i_depth))))
      
      # qf <- gliderdf %>%
      #   filter(yo_id == 98) %>% #grab only yo of interest
      #   filter(cast %in% "Downcast") %>% #grab cast as needed
      #   select(c(m_present_time, m_water_depth, osg_i_depth, yo_id, any_of(plotVar))) 
      #filter(!is.na(across(!c(m_present_time:osg_i_depth))))
      
      if(isTRUE(input$zeroFilterYo)){
        
        zf <- qf %>%
          pivot_longer(cols = !c(m_present_time:i_lon)) %>%
          filter(value > 0) %>%
          pivot_wider(names_from = name)
        
        zf
      } else {
        qf
      }
      
    })
    
    yoList <- reactive({
      qf <- isolate(glider$full)

      unique(qf$yo_id) %>%
        na.omit() %>%
        sort()
    })
    
    yoPlot_live <- reactive({
      
      yoPlot(missionNum$id, 
             yoChunk(), 
             input$yo_var)
      
    })
    
    output$yoPlot <- renderPlotly(yoPlot_live())
    
    output$yoDown <- downloadHandler(
      filename = function() {
        paste0(missionNum$id, "_yo", input$yo, ".csv")
      },
      content = function(file) {
        write.csv(yoChunk(), file, row.names = FALSE)
      }
    )
    
    #### Buttons to scroll through yos ####
    #initialize reactive to track with same value as yo variable
    selectYo <- reactiveValues(id = tail(yoList(), 1))
    
    #attach IDs to yo plot reactive
    observeEvent(input$yo, {
      selectYo$id <- as.numeric(input$yo)
    })
    
    observeEvent(input$oldestYo, {
      selectYo$id <- head(yoList(), 1)
      updateNumericInput(session, "yo", NULL, value = head(yoList(), 1))
      # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
      #                    options = list(minDate = startDateLive, maxDate = endDateLive,
      #                                   timeFormat = "HH:mm"))
    })
    observeEvent(input$prevYo, {
      if (selectYo$id > 1) {
        selectYo$id <- as.numeric(input$yo) - 1
        updateNumericInput(session, "yo", NULL, value = yoList()[selectYo$id])
        # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
        #                    options = list(minDate = startDateLive, maxDate = endDateLive,
        #                                   timeFormat = "HH:mm"))
      }
    })
    observeEvent(input$nextYo, {
      if (selectYo$id < length(yoList())) {
        selectYo$id <- as.numeric(input$yo) + 1
        updateNumericInput(session, "yo", NULL, value = yoList()[selectYo$id])
        # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
        #                    options = list(minDate = startDateLive, maxDate = endDateLive,
        #                                   timeFormat = "HH:mm"))
      }
    })
    observeEvent(input$latestYo, {
      selectYo$id <- tail(yoList(), 1)
      updateNumericInput(session, "yo", NULL, value = tail(yoList(), 1))
      # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
      #                    options = list(minDate = startDateLive, maxDate = endDateLive,
      #                                   timeFormat = "HH:mm"))
    })
    
    ########## explorer plot #########
    
    exploreChunk <- reactive({
      validate(
        need(gliderReactor$name != "", "Please click the Load Mission Data button")
      )
      select(chunk(), input$exploreVar1, input$exploreVar2)
      #filter(input$exploreVar1 >= input$exploreMin & input$exploreVar1 <= input$exploreMax)
      
      
        # pivot_longer(
        #   cols = !input$exploreVar2,
        #   names_to = "variable",
        #   values_to = "count") %>%
        # filter(!is.na(count))
        # 
      # if(!is.null(input$exploreFilter1)){
      #   filter()
      # }
    })

    ggExplore <- reactive({

      explorePlot <- ggplot(
        data =
          exploreChunk(),
        # aes(x = count,
        #     y = .data[[input$exploreVar2]],
        #     color = variable,
        #     shape = variable)) +
        aes(x = .data[[input$exploreVar1]],
            y = .data[[input$exploreVar2]],
            #color = .data[[input$exploreVar1]],
            #shape = variable
            )) +
        geom_point(na.rm = TRUE) +
        theme_bw() +
        labs(title = paste0(missionNum$id, " Data"),
             #x = "Value"
             ) +
        theme(plot.title = element_text(size = 32)) +
        theme(axis.title = element_text(size = 16)) +
        theme(axis.text = element_text(size = 12))
      
      if(input$exploreRevAxis == TRUE){
        explorePlot <- explorePlot + scale_y_reverse()
          
      }
      
      if(input$exploreSmooth == TRUE){
        explorePlot <- explorePlot + geom_smooth(method=NULL, se=TRUE)
      }
      
      explorePlot
      
    })
    
    output$explorePlot <- renderPlot({ggExplore()})

  })
}
