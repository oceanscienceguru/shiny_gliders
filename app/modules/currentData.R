##### Main glider dashboard module #########
library(shiny)

currentData_ui <- function(id) {

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
    fluidPage(
      fluidRow(
        box(title = "Data Filtering",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
                        box(
                                  airDatepickerInput(
                                    inputId = ns("date1Live"),
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
                                    inputId = ns("date2Live"),
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
                                    label = "Depth Minimum (m)",
                                    value = 1,
                                    min = 0,
                                    max = 1000
                                  ),
                                  numericInput(
                                    inputId = ns("max_depth"),
                                    label = "Depth Maximum (m)",
                                    value = 1000,
                                    min = 0,
                                    max = 1500
                                  ),
                                  # checkboxGroupInput(
                                  #   inputId = "status",
                                  #   label = "Dive only?",
                                  #   choices = c("dive" = "dive",
                                  #               "climb" = "climb"),
                                  #   selected = c("dive")
                                  # )
                        )),
      ),
                 fluidRow(
                        #mainPanel(#science variable settings
                        tabsetPanel(
                          id = ns("x"),
                          #width = 12,
                          # tabPanel(title = "Map",
                          #          leafletOutput(outputId = "missionmapLive",
                          #                        height = "600px")),
                          tabPanel(title = "Science Data",
                                   column(2,
                                          wellPanel(
                                            selectInput(
                                              inputId = ns("display_varLive"),
                                              label = "Which science variable to display",
                                              choices = NULL
                                            ),
                                            checkboxInput(
                                              inputId = ns("zeroFilter"),
                                              label = "Filter data > 0?",
                                              value = TRUE
                                            ),
                                            h4("Color Scale Override"),
                                            numericInput(inputId = ns("minLive"),
                                                         label = "Sci Axis Minimum",
                                                         NULL),
                                            numericInput(inputId = ns("maxLive"),
                                                         label = "Sci Axis Maximum",
                                                         NULL),
                                            hr(),
                                            h4("Variable Summary"),
                                            tableOutput(outputId = ns("sciSummary"))
                                            #downloadButton('downloadSciPlot')
                                          )),
                                   column(
                                     10,
                                     # h4("Brush and double-click to zoom (double-click again to reset)"),
                                     # girafeOutput(
                                     #   outputId = ns("sciPlotLive"),
                                     #   # dblclick = "sciPlot_dblclick",
                                     #   # brush = brushOpts(id = "sciPlot_brush",
                                     #   #                   resetOnNew = TRUE),
                                     #   # height = "600px"
                                     # ) %>% withSpinner(color="#0dc5c1")
                                     plotlyOutput(
                                       outputId = ns("sciPlotLive"),
                                       # dblclick = "sciPlot_dblclick",
                                       # brush = brushOpts(id = "sciPlot_brush",
                                       #                   resetOnNew = TRUE),
                                       height = "75vh"
                                     ) %>% withSpinner(color="#0dc5c1"))
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
                                                inputId = ns("flight_varLive"),
                                                label = "Which flight variable(s) to display",
                                                choices = NULL,
                                                multiple = TRUE,
                                                options = list(plugins = list('remove_button'))
                                              ),
                                              hr(),
                                              h4("Variable Summary"),
                                              tableOutput(outputId = ns("fliSummary"))
                                              # downloadButton('downloadFliPlot'),
                                              # verbatimTextOutput("summary")
                                            )),
                                     column(
                                       10,
                                       # h4("Brush and double-click to zoom (double-click again to reset)"),
                                       plotlyOutput(
                                         outputId = ns("fliPlotLive"),
                                         # dblclick = "fliPlot_dblclick",
                                         # brush = brushOpts(id = "fliPlot_brush",
                                         #                   resetOnNew = TRUE),
                                         height = "75vh"
                                       ) %>% withSpinner(color="#0dc5c1")
                                     )
                                   )),
                          #sound velocity tab
                          tabPanel(title = "Derived Data",
                                   fluidRow(
                                     column(2,
                                            wellPanel(
                                              selectInput(inputId = ns("derivedTypeLive"),
                                                          label = "Which type of plot?",
                                                          NULL),
                                              # numericInput(inputId = "derivedmaxLive",
                                              #              label = "Axis Maximum",
                                              #              NULL),
                                              #downloadButton('downloadSouPlot')
                                            )),
                                     column(10,
                                            plotlyOutput(outputId = ns("tsPlotLive"),
                                                         height = "75vh"
                                            )
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
                                            selectizeInput(
                                              inputId = ns("yo"),
                                              label = "Which yo to display",
                                              choices = NULL,
                                              selected =  NULL,
                                              multiple = FALSE
                                            ),
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
                                            height = "60vh"
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
                        )
        )
      )
  )
}

currentData_server <- function(id, gliderName, session) {

  moduleServer(id, function(input, output, session) {

    ns <- NS(id)

    observe({
      # Trigger the custom message manually from the server side to ensure the message is sent
      session$sendCustomMessage('getOffset', list())
    })

    # Reactive value to wait for clientOffset to be set
    clientOffsetReactive <- reactive({
      req(input$clientOffset)  # Ensure the offset is set
      input$clientOffset
    })

    load(paste0(liveDir, "/", gliderName, "/glider_live.RData"))

    startDateLive <- as.POSIXct(min(gliderdf$m_present_time))
    endDateLive <- as.POSIXct(max(gliderdf$m_present_time))

    yoList <- reactive({
      unique(gliderdf$yo_id) %>%
        na.omit() %>%
        sort()
    })

    # yoList <- unique(gliderdf$yo_id) %>%
    #   na.omit() %>%
    #   sort()

    # Once we receive the client offset, adjust the date ranges
    observeEvent(input$clientOffset, {
      offset_hours <- clientOffsetReactive() / 60  # Convert offset to hours

      # Calculate adjusted start and end dates for both inputs
      adjusted_startDate <- as.POSIXct(startDateLive, tz = "UTC") + hours(offset_hours)
      adjusted_endDate <- as.POSIXct(endDateLive, tz = "UTC") + hours(offset_hours)

      # get start/end days and update data filters
      # default to last 3 weeks of data
      # Update date1Live input
      updateAirDateInput(session, "date1Live", NULL,
                         value = as.POSIXct(ifelse(interval(startDateLive, endDateLive) / days(1) > 21,
                                                   adjusted_endDate - days(21),
                                                   adjusted_startDate)),
                         options = list(
                           minDate = as.character(adjusted_startDate),
                           maxDate = as.character(adjusted_endDate),
                           timeFormat = "HH:mm"
                         ))

      # Update date2Live input
      updateAirDateInput(session, "date2Live", NULL,
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

    updateSelectInput(session, "display_varLive", NULL, choices = c(scivarsLive), selected = tail(scivarsLive, 1))
    updateSelectizeInput(session, "flight_varLive", NULL, choices = c(flightvarsLive), selected = "m_roll")

    updateSelectizeInput(session, "yo", NULL, choices = c(yoList()), selected = tail(yoList(), 1), server = TRUE)
    updateSelectInput(session, "yo_var", NULL, choices = c(scivarsLive), selected = tail(scivarsLive, 1))

    updateSelectInput(session, "derivedTypeLive", NULL, choices = c("Salinity", "Density", "SV Plot", "TS Plot"), selected = "Salinity")
    
    allvars <- colnames(gliderdf)
    
    updateSelectInput(session, "exploreVar1", NULL, choices = c(allvars), selected = "m_present_time")
    updateSelectInput(session, "exploreVar2", NULL, choices = c(allvars), selected = head(allvars, 1))
    
    gliderChunk_live <- reactive({
      req(!is.null(input$date1Live))

      soFar <- interval(input$date1Live, input$date2Live)
      #print(soFar)

      df <- gliderdf %>%
        filter(m_present_time %within% soFar) %>%
        #filter(status %in% c(input$status)) %>%
        filter(osg_i_depth >= input$min_depth & osg_i_depth <= input$max_depth)

      df
    })

    scienceChunk_live <- reactive({
      req(!is.null(input$date1Live))

      qf <- gliderChunk_live() %>%
        select(c(m_present_time, osg_i_depth, any_of(input$display_varLive))) %>%
        filter(!is.na(across(!c(m_present_time:osg_i_depth))))

      if(isTRUE(input$zeroFilter)){

        zf <- qf %>%
          filter(.data[[input$display_varLive]] > 0)

        zf
      } else {
      qf
      }

    })

    output$sciSummary <- renderTable({
      req(!is.null(input$date1Live))
      tibble::enframe(summary(scienceChunk_live()[[input$display_varLive]]))
    })

    flightChunk_live <- reactive({
      req(input$date1Live)

      df <- gliderChunk_live() %>%
        dplyr::select(c(m_present_time, osg_i_depth, any_of(input$flight_varLive))) %>%
        filter(m_present_time >= input$date1Live & m_present_time <= input$date2Live)

      df

    })

    gg1Live <- reactive({

      req(input$display_varLive)

      sciPlot(gliderName,
              inGliderdf = scienceChunk_live(),
              gliderFlightdf = gliderChunk_live(),
              plotVar = input$display_varLive,
              liveData = TRUE,
              colorMin = input$minLive,
              colorMax= input$maxLive,
              logoFile = switch(file.exists(paste0(logo_source, "/horiz.png")) + 1,
                                NULL,
                                paste0(logo_source, "/horiz.png")))

    })

    output$sciPlotLive <- renderPlotly(gg1Live())

    #flight plot
    gg2Live <- reactive({

      req(input$flight_varLive)

      fliPlot(gliderName,
              inGliderdf = flightChunk_live(),
              plotVar = input$flight_varLive,
              liveData = TRUE,
              logoFile = switch(file.exists(paste0(logo_source, "/horiz.png")) + 1,
                                NULL,
                                paste0(logo_source, "/horiz.png")))

    })

    output$fliPlotLive <- renderPlotly(gg2Live())

    output$fliSummary <- renderTable({
      req(input$flight_varLive)

      summs <- flightChunk_live() %>%
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

    ##### derived Live plots #########
    gg3Live <- reactive({

      req(input$derivedTypeLive)

      if (input$derivedTypeLive == "TS Plot"){
        df <- filter(gliderChunk_live(), osg_salinity > 0)
        wf <- filter(gliderChunk_live(), m_water_depth > 0 & m_water_depth >= input$min_depth & m_water_depth <= input$max_depth)

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
          theme_bw() +
          labs(title = "TS Plot",
               x = "Salinity",
               y = "Potential Temperature",
               #color = "Time",
               #caption = "Red = older ... Blue = more recent",
               #caption = "<img src='./www/cms_horiz.png' width='200'/>"
          ) +
          theme(plot.title = element_text(size = 32),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 12),
                plot.caption = element_markdown(),
                legend.position ="none",
          )

        plot <- ggplotly(plot) %>%
          toWebGL()
      }

      if (input$derivedTypeLive == "SV Plot"){
        df <- filter(gliderChunk_live(), osg_soundvel1 > 0)
        wf <- filter(gliderChunk_live(), m_water_depth > 0 & m_water_depth >= input$min_depth & m_water_depth <= input$max_depth)

        plot <- sciPlot(gliderName,
                        inGliderdf = df,
                        gliderFlightdf = wf,
                        plotVar = input$derivedTypeLive,
                        liveData = TRUE,
                        colorMin = NULL,
                        colorMax= NULL,
                        logoFile = switch(file.exists(paste0(logo_source, "/horiz.png")) + 1,
                                          NULL,
                                          paste0(logo_source, "/horiz.png")))
      }

      if (input$derivedTypeLive == "Density"){
        df <- filter(gliderChunk_live(), osg_rho > 0)
        wf <- filter(gliderChunk_live(), m_water_depth > 0 & m_water_depth >= input$min_depth & m_water_depth <= input$max_depth)

        plot <- sciPlot(gliderName,
                        inGliderdf = df,
                        gliderFlightdf = wf,
                        plotVar = input$derivedTypeLive,
                        liveData = TRUE,
                        colorMin = NULL,
                        colorMax= NULL,
                        logoFile = switch(file.exists(paste0(logo_source, "/horiz.png")) + 1,
                                          NULL,
                                          paste0(logo_source, "/horiz.png")))

      }

      if (input$derivedTypeLive == "Salinity"){
        df <- filter(gliderChunk_live(), osg_salinity > 0)
        wf <- filter(gliderChunk_live(), m_water_depth > 0 & m_water_depth >= input$min_depth & m_water_depth <= input$max_depth)

        plot <- sciPlot(gliderName,
                inGliderdf = df,
                gliderFlightdf = wf,
                plotVar = input$derivedTypeLive,
                liveData = TRUE,
                colorMin = NULL,
                colorMax= NULL,
                logoFile = switch(file.exists(paste0(logo_source, "/horiz.png")) + 1,
                                  NULL,
                                  paste0(logo_source, "/horiz.png")))
      }

      plot

    })

    output$tsPlotLive <- renderPlotly(gg3Live())

    #### yo by yo ####

    #date matching to find yo in time
    observeEvent(input$yoDate, {
      req(input$yoDate)
      yoFinder <- gliderdf %>%
        slice(which.min(abs(m_present_time - input$yoDate)))

      selectYo$id <- yoFinder$yo_id
      updateSelectizeInput(session, "yo", NULL, choices = c(yoList()), selected = yoFinder$yo_id, server = TRUE)
    })

    yoChunk <- reactive({
      req(input$yo)

      qf <- gliderdf %>%
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

    yoPlot_live <- reactive({

      yoPlot(gliderName,
             yoChunk(),
             input$yo_var,
             logoFile = switch(file.exists(paste0(logo_source, "/horiz.png")) + 1,
                               NULL,
                               paste0(logo_source, "/horiz.png")))

    })

    output$yoPlot <- renderPlotly(yoPlot_live())

    #### Buttons to scroll through yos ####
    #initialize reactive to track with same value as yo variable
    selectYo <- reactiveValues(id = tail(yoList(), 1))

    #attach IDs to yo plot reactive
    observeEvent(input$yo, {
      selectYo$id <- as.numeric(input$yo)
    })

    observeEvent(input$oldestYo, {
      selectYo$id <- head(yoList(), 1)
      updateSelectizeInput(session, "yo", NULL, choices = c(yoList()), selected = head(yoList(), 1), server = TRUE)
      # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
      #                    options = list(minDate = startDateLive, maxDate = endDateLive,
      #                                   timeFormat = "HH:mm"))
    })
    observeEvent(input$prevYo, {
      if (selectYo$id > 1) {
        selectYo$id <- as.numeric(input$yo) - 1
        updateSelectizeInput(session, "yo", NULL, choices = c(yoList()), selected = yoList()[selectYo$id], server = TRUE)
        # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
        #                    options = list(minDate = startDateLive, maxDate = endDateLive,
        #                                   timeFormat = "HH:mm"))
      }
    })
    observeEvent(input$nextYo, {
      if (selectYo$id < length(yoList())) {
        selectYo$id <- as.numeric(input$yo) + 1
        updateSelectizeInput(session, "yo", NULL, choices = c(yoList()), selected = yoList()[selectYo$id], server = TRUE)
        # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
        #                    options = list(minDate = startDateLive, maxDate = endDateLive,
        #                                   timeFormat = "HH:mm"))
      }
    })
    observeEvent(input$latestYo, {
      selectYo$id <- tail(yoList(), 1)
      updateSelectizeInput(session, "yo", NULL, choices = c(yoList()), selected = tail(yoList(), 1), server = TRUE)
      # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
      #                    options = list(minDate = startDateLive, maxDate = endDateLive,
      #                                   timeFormat = "HH:mm"))
    })

    ########## explorer plot #########

    exploreChunk <- reactive({

      select(gliderChunk_live(), input$exploreVar1, input$exploreVar2)
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
        labs(title = paste0(gliderName, " Data"),
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




