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
                                    value = 0,
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
                                   column(3,
                                          wellPanel(
                                            selectInput(
                                              inputId = ns("display_varLive"),
                                              label = "Which science variable to display",
                                              choices = NULL
                                            ),
                                            # checkboxInput(
                                            #   inputId = ns("zeroFilter"),
                                            #   label = "Filter data > 0?",
                                            #   value = TRUE
                                            # ),
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
                                     girafeOutput(
                                       outputId = ns("sciPlotLive"),
                                       # dblclick = "sciPlot_dblclick",
                                       # brush = brushOpts(id = "sciPlot_brush",
                                       #                   resetOnNew = TRUE),
                                       # height = "600px"
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
                                       9,
                                       # h4("Brush and double-click to zoom (double-click again to reset)"),
                                       girafeOutput(
                                         outputId = ns("fliPlotLive"),
                                         # dblclick = "fliPlot_dblclick",
                                         # brush = brushOpts(id = "fliPlot_brush",
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
                                              selectInput(inputId = ns("derivedTypeLive"),
                                                          label = "Which type of plot?",
                                                          NULL),
                                              # numericInput(inputId = "derivedmaxLive",
                                              #              label = "Axis Maximum",
                                              #              NULL),
                                              #downloadButton('downloadSouPlot')
                                            )),
                                     column(9,
                                            girafeOutput(outputId = ns("tsPlotLive"),
                                                       #height = "600px"
                                            )
                                     )
                                   ),),
                        )
        )
      )
  )
}

currentData_server <- function(id, gliderName) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    load(paste0("/echos/", gliderName, "/glider_live.RData"))
    
    startDateLive <- as_datetime(min(gliderdf$m_present_time), tz = "UTC")
    endDateLive <- as_datetime(max(gliderdf$m_present_time), tz = "UTC")
    
    #get start/end days and update data filters
    updateAirDateInput(session, "date1Live", NULL, value = startDateLive, 
                       options = list(minDate = startDateLive, maxDate = endDateLive,
                                      timeFormat = "HH:mm"))
    updateAirDateInput(session, "date2Live", NULL, value = endDateLive, 
                       options = list(minDate = startDateLive, maxDate = endDateLive,
                                      timeFormat = "HH:mm"))
    
    updateSelectInput(session, "display_varLive", NULL, choices = c(scivarsLive), selected = tail(scivarsLive, 1))
    updateSelectizeInput(session, "flight_varLive", NULL, choices = c(flightvarsLive), selected = "m_roll")
    
    updateSelectInput(session, "derivedTypeLive", NULL, choices = c("Salinity", "Density", "SV Plot", "TS Plot"), selected = "Salinity")
    
    #check if echosounder glider and update tabs if so
    if(gliderName == "usf-stella"){
      
      appendTab(inputId = "x",
                select = F,
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
                                          ),

                )
      )
      appendTab(inputId = "x",
                select = F,
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
                         ))
      )
      appendTab(inputId = "x",
                select = F,
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

      
    } else {
      
      removeTab(inputId = "x", target="Pseudograms")
      removeTab(inputId = "x", target="Pseudotimegram")
      removeTab(inputId = "x", target="Frequency Polygon")
      
    }
    
    gliderChunk_live <- reactive({
      soFar <- interval(input$date1Live, input$date2Live)
      #soFar <- interval(force_tz(input$date1Live, "UTC"), force_tz(input$date2Live, "UTC"))
      
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
      
      # if(isTRUE(input$zeroFilter)){
      #   qf <- qf %>%
      #     filter(input$display_varLive > 0)
      # }
      
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
    
    gg1Live <- reactive({
      sciLive <- ggplot(
        data = 
          scienceChunk_live(),#dynamically filter the sci variable of interest
        aes(x=m_present_time,
            y=osg_i_depth,
            #z=.data[[input$display_varLive]],
            colour = .data[[input$display_varLive]],
            tooltip = round(.data[[input$display_varLive]], 3)
        )) +
        geom_point_interactive(
          # size = 2,
          na.rm = TRUE
        ) +
        # coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
        #geom_hline(yintercept = 0) +
        scale_y_reverse() +
        scale_colour_viridis_c(limits = c(input$minLive, input$maxLive)) +
        geom_point(data = filter(gliderChunk_live(), m_water_depth > 0 & m_water_depth >= input$min_depth & m_water_depth <= input$max_depth),
                   aes(y = m_water_depth),
                   size = 0.3,
                   color = "black",
                   na.rm = TRUE
        ) +
        theme_bw() +
        labs(title = paste0(gliderName, " Current Data"),
          y = "Depth (m)",
          x = "Date",
          caption = "<img src='./www/cms_horiz.png' width='200'/>") +
        theme(plot.title = element_text(size = 32)) +
        theme(axis.title = element_text(size = 16)) +
        theme(axis.text = element_text(size = 12)) +
        theme(plot.caption = element_markdown())
      
      sciLive
      
    })
    
    output$sciPlotLive <- renderGirafe(girafe(code = print(gg1Live()),
                                              width_svg = 12, height_svg = 5,
                                              options = list(
                                                opts_sizing(width = .7),
                                                opts_zoom(max = 5),
                                                opts_toolbar(position = "bottomleft")
                                              )
                                              ))
    
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
            shape = variable,
            tooltip = round(count, 3))) +
        geom_point_interactive() +
        #coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
        theme_bw() +
        labs(title = paste0(gliderName, " Current Data"),
          x = "Date",
          caption = "<img src='./www/cms_horiz.png' width='200'/>") +
        theme(plot.title = element_text(size = 32)) +
        theme(axis.title = element_text(size = 16)) +
        theme(axis.text = element_text(size = 12),
              plot.caption = element_markdown())
      
      fliLive 
      
    })
    
    output$fliPlotLive <- renderGirafe(girafe(code = print(gg2Live()),
                                              width_svg = 12, height_svg = 5,
                                              options = list(
                                                opts_sizing(width = .7),
                                                opts_zoom(max = 5),
                                                opts_toolbar(position = "bottomleft")
                                              )
                                              ))
    
    ##### derived Live plots #########
    gg3Live <- reactive({
      
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
          geom_point_interactive(size = 3,
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
               #caption = "Red = older ... Blue = more recent",
               caption = "<img src='./www/cms_horiz.png' width='200'/>"
          ) +
          theme(plot.title = element_text(size = 32),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 12),
                plot.caption = element_markdown(),
                legend.position ="none",
          ) 
      }
      
      if (input$derivedTypeLive == "SV Plot"){
        df <- filter(gliderChunk_live(), osg_soundvel1 > 0)
        wf <- filter(gliderChunk_live(), m_water_depth > 0 & m_water_depth >= input$min_depth & m_water_depth <= input$max_depth)
        
        plot <- 
          ggplot(data = df,
                 aes(x=m_present_time,
                     y=osg_depth,
                     #z=osg_soundvel1
                 )) +
          geom_point_interactive(
            aes(color = osg_soundvel1, tooltip = round(osg_soundvel1, 3))
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
               caption = "Calculated using Coppens <i>et al.</i> (1981) 
               <br>
               <br>
               <img src='./www/cms_horiz.png' width='200'/>",
               y = "Depth (m)",
               x = "Date") +
          theme(plot.title = element_text(size = 32)) +
          theme(axis.title = element_text(size = 16)) +
          theme(axis.text = element_text(size = 12)) +
          theme(plot.caption = element_markdown())
      }
      
      if (input$derivedTypeLive == "Density"){
        df <- filter(gliderChunk_live(), osg_rho > 0)
        wf <- filter(gliderChunk_live(), m_water_depth > 0 & m_water_depth >= input$min_depth & m_water_depth <= input$max_depth)
        
        plot <- 
          ggplot(
            data = 
              df,
            aes(x=m_present_time,
                y=osg_depth,
                #z=.data[[input$display_varLive]],
                colour = osg_rho,
                tooltip = round(osg_rho, 3)
            )) +
          geom_point_interactive(
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
               x = "Date",
               caption = "<img src='./www/cms_horiz.png' width='200'/>") +
          theme(plot.title = element_text(size = 32)) +
          theme(axis.title = element_text(size = 16)) +
          theme(axis.text = element_text(size = 12),
                plot.caption = element_markdown())
      }
      
      if (input$derivedTypeLive == "Salinity"){
        df <- filter(gliderChunk_live(), osg_salinity > 0)
        wf <- filter(gliderChunk_live(), m_water_depth > 0 & m_water_depth >= input$min_depth & m_water_depth <= input$max_depth)
        
        plot <- 
          ggplot(
            data = 
              df,
            aes(x=m_present_time,
                y=osg_depth,
                #z=.data[[input$display_varLive]],
                colour = osg_salinity,
                tooltip = round(osg_salinity, 3)
            )) +
          geom_point_interactive(
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
               x = "Date",
               caption = "<img src='./www/cms_horiz.png' width='200'/>") +
          theme(plot.title = element_text(size = 32)) +
          theme(axis.title = element_text(size = 16)) +
          theme(axis.text = element_text(size = 12),
                plot.caption = element_markdown())
      }
      
      plot
      
    })
    
    output$tsPlotLive <- renderGirafe(girafe(code = print(gg3Live()),
                                             width_svg = 12, height_svg = 5,
                                             options = list(
                                               opts_sizing(width = .7),
                                               opts_zoom(max = 5),
                                               opts_toolbar(position = "bottomleft")
                                             )
                                             ))
    
    #### psuedograms ########
    
    #color palette source:
    #https://rdrr.io/github/hvillalo/echogram/src/R/palette.echogram.R
    # velInfo <- file.info(list.files(path = "/echos/layers/",
    #                                 full.names = TRUE)) %>%
    #   filter(size > 0)
    # 
    # velList <- rownames(velInfo) %>%
    #   basename()
    # 
    # depthInfo <- file.info(list.files(path = "/echos/depths/",
    #                                   full.names = TRUE))
    # 
    # depthList <- rownames(depthInfo) %>%
    #   basename()
    # 
    # echoListraw <- intersect(velList, depthList) %>%
    #   str_remove(pattern = ".ssv") %>%
    #   enframe() %>%
    #   mutate(ID = str_extract(value, "(?<=-)[0-9]*$")) %>%
    #   mutate(ID = as.numeric(ID)) %>%
    #   arrange(ID)
    # 
    # echoList <- echoListraw$value
    # 
    #reactive pseudogram plot identifier for scrolling
    
    selectPgram <- reactiveValues(seg = NULL, id = NULL)
    if(gliderName == "usf-stella"){
      updateSelectInput(session, "echo", NULL, choices = c(echoListraw$value), selected = tail(echoListraw$value, 1))
      
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
               colour = "dB",
               caption = "<img src='./www/cms_horiz.png' width='200'/>") +
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
      
      
      #### updater for pseudotimegram inputs ####
      observeEvent(input$fullecho | input$fullecho2, {
        
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
      })
      
      plotehunk <- reactive({
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
      
    }
  })
  
}
  
  
    

