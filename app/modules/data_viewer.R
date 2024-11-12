##### Main glider dashboard module #########
library(shiny)

data_viewer_ui <- function(id) {

  ns <- NS(id)

  tagList(
    #initialize airdatepicker as UTC
    tags$script(HTML(sprintf("
      Shiny.addCustomMessageHandler('getOffset', function(message) {
        var timezoneOffset = new Date().getTimezoneOffset(); // Get the timezone offset in minutes
        console.log('Timezone offset:', timezoneOffset); // Log the value to verify it's correct
        Shiny.setInputValue('%s', timezoneOffset); // Send the offset to Shiny
      });

      $(document).ready(function() {
        console.log('Sending timezone offset...');
        Shiny.setInputValue('%s', new Date().getTimezoneOffset()); // Send the timezone offset when the document is ready
      });
    ", ns("clientOffset"), ns("clientOffset")))),
    useShinyjs(),
    fluidPage(
      box(title = "Mission Selection and Filters",
          width = 12,
          collapsible = TRUE,
      box(
        actionButton(
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
      ),
      ),
      ),
     fluidRow(
             #mainPanel(#science variable settings
             tabsetPanel(id = ns("tabs"),
               #width = 12,
               tabPanel(title = "Map",
                        leafletOutput(outputId = ns("missionmap"),
                                      height = "600px")),
               tabPanel(title = "Science Data",
                        column(2,
                               wellPanel(
                                 selectInput(
                                   inputId = ns("display_var"),
                                   label = "Which science variable to display",
                                   choices = NULL
                                 ),
                                 checkboxInput(
                                   inputId = ns("interactiveSci"),
                                   label = "Make this plot interactive",
                                   value = FALSE
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
                                 hr(),
                                 h4("Summary"),
                                 tableOutput(outputId = ns("sciSummary"))
                                 #downloadButton('downloadSciPlot')
                               )),
                        column(
                          10,
                          plotOutput(
                            outputId = ns("sciPlot"),
                            height = "75vh"
                          ),
                          plotlyOutput(
                            outputId = ns("sciPlotly"),
                            height = "75vh"
                          )
                        ),
               ),
               #flight variable settings
               tabPanel(title = "Flight Data",
                        fluidRow(
                          column(2,
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
                            8,
                            #h4("Brush and double-click to zoom (double-click again to reset)"),
                            plotlyOutput(
                              outputId = ns("fliPlot"),
                              # dblclick = "fliPlot_dblclick",
                              # brush = brushOpts(id = ns("fliPlot_brush"),
                              #                   resetOnNew = TRUE),
                              height = "75vh"
                            ) %>% withSpinner(color="#0dc5c1")
                          ),
                          column(
                            2,
                            h4("Summary"),
                            tableOutput(outputId = ns("fliSummary"))
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
                                              height = "75vh"
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
                                 height = "75vh"
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
                                            height = "75vh"
                                 ) %>% withSpinner(color="#0dc5c1")
                          )
                        )
                        ),
               selected = "Map"
             )
      )
    )
  )
}

data_viewer_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    fileList_archive <- list.files(path = paste0(fullDir, "/Data/"),
                                   pattern = "*.RData")

    missionList_archive <- str_remove(fileList_archive, pattern = ".RData")

    updateSelectInput(session, "mission", NULL, choices = c(missionList_archive), selected = tail(missionList_archive, 1))

    #mission map
    output$missionmap <- renderLeaflet({
      req(input$mission)

      if(file.exists(paste0(fullDir, "/KML/", input$mission, ".kml"))){
      #grab .kml per mission number
      raw_sf <- st_read(paste0(fullDir, "/KML/", input$mission, ".kml"),
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
        mapUp <- read.csv(paste0(fullDir, "/KML/", input$mission, ".csv")) %>%
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
        ) %>%
        addFullscreenControl()
    })

    observe({
      # Trigger the custom message manually from the server side to ensure the message is sent
      session$sendCustomMessage('getOffset', list())
    })

    # Reactive value to wait for clientOffset to be set
    clientOffsetReactive <- reactive({
      req(input$clientOffset)  # Ensure the offset is set
      input$clientOffset
    })

    missionNum <- reactiveValues()

    glider <- reactiveValues()

    gliderReactor <- reactiveValues(name = NULL)

    selectPgram <- reactiveValues(seg = NULL, id = NULL)

    observeEvent(input$load, {
      load(paste0(fullDir, "/Data/", input$mission, ".RData"))

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

      # Once we receive the client offset, adjust the date ranges
      observeEvent(input$clientOffset, {
        offset_hours <- clientOffsetReactive() / 60  # Convert offset to hours

        # Calculate adjusted start and end dates for both inputs
        adjusted_startDate <- as.POSIXct(startDate, tz = "UTC") + hours(offset_hours)
        adjusted_endDate <- as.POSIXct(endDate, tz = "UTC") + hours(offset_hours)

        # get start/end days and update data filters
        # default to last 3 weeks of data
        # Update date1Live input
        updateAirDateInput(session, "date1", NULL,
                           value = adjusted_startDate,
                           options = list(
                             minDate = as.character(adjusted_startDate),
                             maxDate = as.character(adjusted_endDate),
                             timeFormat = "HH:mm"
                           ))

        # Update date2Live input
        updateAirDateInput(session, "date2", NULL,
                           value = adjusted_endDate,
                           options = list(
                             minDate = as.character(adjusted_startDate),
                             maxDate = as.character(adjusted_endDate),
                             timeFormat = "HH:mm"
                           ))

        updateAirDateInput(session, "yoDate", NULL, value = adjusted_endDate,
                           options = list(
                             minDate = as.character(adjusted_startDate),
                             maxDate = as.character(adjusted_endDate),
                             timeFormat = "HH:mm"))
      })

      #get start/end days and update data filters
      # updateAirDateInput(session, "date1", NULL, value = startDate,
      #                    options = list(minDate = startDate, maxDate = endDate))
      # updateAirDateInput(session, "date2", NULL, value = endDate,
      #                    options = list(minDate = startDate, maxDate = endDate))
      updateSelectInput(session, "display_var", NULL, choices = c(scivars), selected = "sci_water_temp")
      updateSelectizeInput(session, "flight_var", NULL, choices = c(flightvars), selected = "m_roll")

      updateSelectInput(session, "exploreVar1", NULL, choices = c(allvars), selected = "m_present_time")
      updateSelectInput(session, "exploreVar2", NULL, choices = c(allvars), selected = head(allvars, 1))

      # updateAirDateInput(session, "yoDate", NULL, value = endDate,
      #                    options = list(minDate = startDate, maxDate = endDate,
      #                                   timeFormat = "HH:mm"))
      updateNumericInput(session, "yo", NULL, min = 0, max = max(yoNums), value = max(yoNums))
      updateSelectInput(session, "yo_var", NULL, choices = c(scivars), selected = tail(scivars, 1))

      showNotification("Data loaded", type = "message")

      message(paste0(missionNum$id, " data loaded"))

      glider$full <- df

    #dynamically filter for plotting
    chunk <- reactive({
      req(!is.null(input$date1))

    soFar <- interval(input$date1, input$date2)

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

      baseSci <- ggplot(data =
                          scf,
                        aes(x=m_present_time,
                            y=round(osg_i_depth, 2))) +
        geom_point(aes(color = .data[[input$display_var]]),
                   size = 2)

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

    gg1ly <- reactive({
      validate(
        need(gliderReactor$name != "", "Please click the Load Mission Data button")
      )

      scf <- scienceChunk() %>%
        select(m_present_time, osg_i_depth, .data[[input$display_var]]) %>%
        filter(!is.na(.data[[input$display_var]]))

      gcf <- chunk() %>%
        select(m_present_time, m_water_depth) %>%
        filter(!is.na(m_water_depth))

      #setup for interactivity if desired and warn if needed
      if (isTRUE(input$interactiveSci)) {
        if (nrow(scf) > 1000000) {
          showModal(modalDialog(
            title = "Large dataset",
            "Loading this plot may take some time",
            easyClose = TRUE
          ))
        }
        sciPlot(
          gliderName = missionNum$id,
          inGliderdf = scf,
          gliderFlightdf = gcf,
          plotVar = input$display_var,
          colorMin = input$min,
          colorMax = input$max
        )
      }
      })

    output$sciPlot <- renderPlot({gg1()})
    output$sciPlotly <- renderPlotly({gg1ly()})
     #bindCache(missionNum$id, input$display_var)

    output$sciSummary <- renderTable({
      req(input$display_var)
      tibble::enframe(summary(scienceChunk()[[input$display_var]]))
    })

    #switch for interactivitiy using shinyjs
    observeEvent(c(input$interactiveSci), {
      #req(input$group)
      if (isTRUE(input$interactiveSci)) {
        hide("sciPlot")
        #hideSpinner("sciPlot")
        show("sciPlotly")
        #showSpinner("sciPlotly")
      } else {
        hide("sciPlotly")
        #hideSpinner("sciPlotly")
        show("sciPlot")
        #showSpinner("sciPlot")
      }
    })


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

    output$fliSummary <- renderTable({
      req(input$flight_var)

      summs <- flightChunk() %>%
        pivot_longer(cols = !c(m_present_time, osg_i_depth), names_to = "vars") %>%
        group_by(vars) %>%
        summarise(min = min(value, na.rm = TRUE),
               q1  = quantile(value, 0.25, na.rm = TRUE),
               mean = mean(value, na.rm = TRUE),
               median = median(value, na.rm = TRUE),
               q3  = quantile(value, 0.75, na.rm = TRUE),
               max = max(value, na.rm = TRUE),
               sd = sd(value, na.rm = TRUE)) %>%
        pivot_longer(cols = min:sd, names_to = "stat")

      summs
    })

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
})
}
