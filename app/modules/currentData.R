##### Main glider dashboard module #########
library(shiny)

currentData_ui <- function(id) {

  ns <- NS(id)

  tagList(
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
                                            #downloadButton('downloadSciPlot')
                                          )),
                                   column(
                                     9,
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
                                       # height = "600px"
                                     ) %>% withSpinner(color="#0dc5c1")),
                                   column(
                                     1,
                                     h4("Summary"),
                                     tableOutput(outputId = ns("sciSummary"))
                                   )
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
                                              # downloadButton('downloadFliPlot'),
                                              # verbatimTextOutput("summary")
                                            )),
                                     column(
                                       8,
                                       # h4("Brush and double-click to zoom (double-click again to reset)"),
                                       plotlyOutput(
                                         outputId = ns("fliPlotLive"),
                                         # dblclick = "fliPlot_dblclick",
                                         # brush = brushOpts(id = "fliPlot_brush",
                                         #                   resetOnNew = TRUE),
                                         # height = "600px"
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
                                              selectInput(inputId = ns("derivedTypeLive"),
                                                          label = "Which type of plot?",
                                                          NULL),
                                              # numericInput(inputId = "derivedmaxLive",
                                              #              label = "Axis Maximum",
                                              #              NULL),
                                              #downloadButton('downloadSouPlot')
                                            )),
                                     column(9,
                                            plotlyOutput(outputId = ns("tsPlotLive"),
                                                       #height = "600px"
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
                                            selectInput(
                                              inputId = ns("yo"),
                                              label = "Which yo to display",
                                              choices = NULL,
                                              selected =  NULL
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
                        )
        )
      )
  )
}

currentData_server <- function(id, gliderName, clientTZ, session) {

  moduleServer(id, function(input, output, session) {

    ns <- NS(id)

    load(paste0(liveDir, "/", gliderName, "/glider_live.RData"))

    # startDateLive <- as_datetime(min(gliderdf$m_present_time), tz = "UTC")
    # endDateLive <- as_datetime(max(gliderdf$m_present_time), tz = "UTC")

    startDateLive <- as.POSIXct(min(gliderdf$m_present_time))
    endDateLive <- as.POSIXct(max(gliderdf$m_present_time))

    #print(startDateLive)
    #print(endDateLive)

    yoList <- reactive({
      unique(gliderdf$yo_id) %>%
        na.omit() %>%
        sort()
    })

    # yoList <- unique(gliderdf$yo_id) %>%
    #   na.omit() %>%
    #   sort()

    #get start/end days and update data filters
    #default to last 3 weeks of data
    updateAirDateInput(session, "date1Live", NULL, value = as.POSIXct(ifelse(interval(startDateLive, endDateLive)/days(1) > 21,
                                                                             endDateLive-days(21),
                                                                             startDateLive)),
                       options = list(minDate = startDateLive, maxDate = endDateLive,
                                      timeFormat = "HH:mm"))
    updateAirDateInput(session, "date2Live", NULL, value = endDateLive,
                       options = list(minDate = startDateLive, maxDate = endDateLive,
                                      timeFormat = "HH:mm"))
    updateAirDateInput(session, "yoDate", NULL, value = endDateLive,
                       options = list(minDate = startDateLive, maxDate = endDateLive,
                                      timeFormat = "HH:mm"))

    updateSelectInput(session, "display_varLive", NULL, choices = c(scivarsLive), selected = tail(scivarsLive, 1))
    updateSelectizeInput(session, "flight_varLive", NULL, choices = c(flightvarsLive), selected = "m_roll")

    updateSelectInput(session, "yo", NULL, choices = c(yoList()), selected = tail(yoList(), 1))
    updateSelectInput(session, "yo_var", NULL, choices = c(scivarsLive), selected = tail(scivarsLive, 1))

    updateSelectInput(session, "derivedTypeLive", NULL, choices = c("Salinity", "Density", "SV Plot", "TS Plot"), selected = "Salinity")

    gliderChunk_live <- reactive({

      #potential workaround for airdate picker hijacking broswer tz
      # if(clientTZ != 0){
      #   # print("local time adjustment")
      #   # print(input$date1Live)
      #   # print(input$date2Live)
      #   # print(clientTZ)
      #   soFar <- interval(force_tz(input$date1Live - hours(clientTZ)), force_tz(input$date2Live - hours(clientTZ), "UTC"))
      # } else {
      #   # print("no local time adjustment")
      #   # print(input$date1Live)
      #   # print(input$date2Live)
      #   # print(clientTZ)
      #   soFar <- interval(input$date1Live, input$date2Live)
      # }

      soFar <- interval(input$date1Live, input$date2Live)
      #print(soFar)

      df <- gliderdf %>%
        filter(m_present_time %within% soFar) %>%
        #filter(m_present_time >= input$date1Live & m_present_time <= input$date2Live)
      #filter(status %in% c(input$status)) %>%
      #filter(!(is.na(input$display_var) | is.na(m_depth))) %>%
        filter(osg_i_depth >= input$min_depth & osg_i_depth <= input$max_depth)

      df
    })

    scienceChunk_live <- reactive({
      req(input$display_varLive)

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
      #req(input$display_varLive)
      tibble::enframe(summary(scienceChunk_live()[[input$display_varLive]]))
    })

    flightChunk_live <- reactive({
      #req(input$date1Live)

      df <- gliderChunk_live() %>%
        dplyr::select(c(m_present_time, osg_i_depth, any_of(input$flight_varLive))) %>%
        filter(m_present_time >= input$date1Live & m_present_time <= input$date2Live)

      df

    })

    gg1Live <- reactive({

      #req(input$display_varLive)

      sciPlot(gliderName,
              inGliderdf = scienceChunk_live(),
              gliderFlightdf = gliderChunk_live(),
              plotVar = input$display_varLive,
              liveData = TRUE,
              colorMin = input$minLive,
              colorMax= input$maxLive,
              logoFile = "./www/cms_horiz.png")

    })

    output$sciPlotLive <- renderPlotly(gg1Live())

    #flight plot
    gg2Live <- reactive({

      #req(input$flight_varLive)

      fliPlot(gliderName,
              inGliderdf = flightChunk_live(),
              plotVar = input$flight_varLive,
              liveData = TRUE,
              logoFile = "./www/cms_horiz.png")

    })

    output$fliPlotLive <- renderPlotly(gg2Live())

    output$fliSummary <- renderTable({
      #req(input$flight_varLive)

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

      #req(input$derivedTypeLive)

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
               caption = "<img src='./www/cms_horiz.png' width='200'/>"
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
                        logoFile = "./www/cms_horiz.png")
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
                        logoFile = "./www/cms_horiz.png")

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
                logoFile = "./www/cms_horiz.png")
      }

      plot

    })

    output$tsPlotLive <- renderPlotly(gg3Live())

    #### yo by yo ####

    #date matching to find yo in time
    observeEvent(input$yoDate, {
      yoFinder <- gliderdf %>%
        slice(which.min(abs(m_present_time - input$yoDate)))

      selectYo$id <- yoFinder$yo_id
      updateSelectInput(session, "yo", NULL, choices = c(yoList()), selected = yoFinder$yo_id)
    })

    yoChunk <- reactive({
      #req(input$yo)

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
             logoFile = "./www/cms_horiz.png")

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
      updateSelectInput(session, "yo", NULL, choices = c(yoList()), selected = head(yoList(), 1))
      # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
      #                    options = list(minDate = startDateLive, maxDate = endDateLive,
      #                                   timeFormat = "HH:mm"))
    })
    observeEvent(input$prevYo, {
      if (selectYo$id > 1) {
        selectYo$id <- as.numeric(input$yo) - 1
        updateSelectInput(session, "yo", NULL, choices = c(yoList()), selected = yoList()[selectYo$id])
        # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
        #                    options = list(minDate = startDateLive, maxDate = endDateLive,
        #                                   timeFormat = "HH:mm"))
      }
    })
    observeEvent(input$nextYo, {
      if (selectYo$id < length(yoList())) {
        selectYo$id <- as.numeric(input$yo) + 1
        updateSelectInput(session, "yo", NULL, choices = c(yoList()), selected = yoList()[selectYo$id])
        # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
        #                    options = list(minDate = startDateLive, maxDate = endDateLive,
        #                                   timeFormat = "HH:mm"))
      }
    })
    observeEvent(input$latestYo, {
      selectYo$id <- tail(yoList(), 1)
      updateSelectInput(session, "yo", NULL, choices = c(yoList()), selected = tail(yoList(), 1))
      # updateAirDateInput(session, "yoDate", NULL, value = mean(yoChunk()$m_present_time),
      #                    options = list(minDate = startDateLive, maxDate = endDateLive,
      #                                   timeFormat = "HH:mm"))
    })

    #### pseudograms ########

  })

}




