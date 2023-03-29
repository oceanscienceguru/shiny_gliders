#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggplot2)
library(shiny)
library(leaflet)
#library(leaflet.extras)
library(sf)
#library(PlotSvalbard) #devtools::install_github("MikkoVihtakari/PlotSvalbard", upgrade = "never")
#library(patchwork)
#library(cowplot)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux?

fileList <- list.files(path = "./Data/",
           pattern = "*.rds")

missionList <- str_remove(fileList, pattern = ".rds")

icon.start <- makeAwesomeIcon(
  icon = "flag", markerColor = "green",
  library = "fa",
  iconColor = "black"
)

icon.end <- makeAwesomeIcon(
  icon = "flag", markerColor = "red",
  library = "fa",
  iconColor = "black"
)

#https://rdrr.io/github/AustralianAntarcticDivision/ZooScatR/src/R/soundvelocity.R
c_Coppens1981 <- function(D,S,T){
  t <- T/10
  D = D/1000
  c0 <- 1449.05 + 45.7*t - 5.21*(t^2)  + 0.23*(t^3)  + (1.333 - 0.126*t + 0.009*(t^2)) * (S - 35)
  c <- c0 + (16.23 + 0.253*t)*D + (0.213-0.1*t)*(D^2)  + (0.016 + 0.0002*(S-35))*(S- 35)*t*D
  return(c)
}

#https://rdrr.io/cran/wql/man/ec2pss.html
ec2pss <-
  function (ec, t, p = 0) {
    # Define conductivity ratio
    R <- ec/42.914
    
    # Estimate temperature correction (valid for -2 < t < 35)
    c <- c(0.6766097, 0.0200564, 0.0001104259, -6.9698e-07, 1.0031e-09)
    rt <- c[1] + c[2] * t + c[3] * t^2 + c[4] * t^3 + c[5] * t^4
    
    # Estimate pressure correction (validity range varies with t and S)
    d <- c(0.03426, 0.0004464, 0.4215, -0.003107)
    e <- c(2.07e-05, -6.37e-10, 3.989e-15)
    Rp <- 1 + p * (e[1] + e[2] * p + e[3] * p^2)/(1 + d[1] * t + 
                                                    d[2] * t^2 + (d[3] + d[4] * t) * R)
    
    # Estimate salinity (valid for 2 < S < 42 and -2 < t < 35).       
    Rt <- R/(Rp * rt)
    a <- c(0.008, -0.1692, 25.3851, 14.0941, -7.0261, 2.7081)
    b <- c(5e-04, -0.0056, -0.0066, -0.0375, 0.0636, -0.0144)
    ft <- (t - 15)/(1 + 0.0162 * (t - 15))
    S <- a[1] + a[2] * Rt^0.5 + a[3] * Rt + a[4] * Rt^1.5 + a[5] * 
      Rt^2 + a[6] * Rt^2.5 + ft * (b[1] + b[2] * Rt^0.5 + b[3] * 
                                     Rt + b[4] * Rt^1.5 + b[5] * Rt^2 + b[6] * Rt^2.5)
    
    # Estimate salinity correction for S < 2
    x <- 400 * Rt
    y <- 100 * Rt
    ifelse(S >= 2, S, S - a[1]/(1 + 1.5 * x + x^2) - b[1] * ft/(1 + 
                                                                  y^0.5 + y + y^1.5))
  }

# Define UI for application that draws a histogram
ui <- fluidPage(
  #force notifications to center of page
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
      )
    )
  ),
  titlePanel("The Brewery"),
wellPanel(
    #start button
    actionButton("load", "Load Mission Data", icon("arrows-rotate"), 
                 style="color: #fff; background-color: #963ab7; border-color: #2e6da4"),
      selectInput(
        "mission",
        "Which mission data to display",
        choices = c(missionList),
        selected =  NULL
      ),
    #parameter input row
    fluidRow(
      column(
        4,
        dateInput(
          "date1",
          "Start Date:",
          value = NULL,
          min = NULL,
          max = NULL,
          format = "mm/dd/yy"
        ),
        dateInput(
          "date2",
          "End Date:",
          value = NULL,
          min = NULL,
          max = NULL,
          format = "mm/dd/yy"
        )
      ),
      column(
        4,
        numericInput("min_depth", "Depth Minimum", 3, min = 0, max = 1000),
        numericInput(
          "max_depth",
          "Depth Maximum",
          150,
          min = 0,
          max = 1000
        )
      ),
      column(
        4,
        checkboxGroupInput(
          "status",
          "Dive only?",
          choices = c("dive" = "dive",
                      "climb" = "climb"),
          selected = c("dive")
        )
      )
    )),
actionButton("initialize", "Visualize", icon("plane"), 
             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    #science variable settings
    tabsetPanel(
      tabPanel("Map",
               fluidRow(
                 leafletOutput("missionmap")
               ), ),
      tabPanel("Science Data",
                fluidRow(
      column(
        3,
        wellPanel(
          selectInput(
            "display_var",
            "Which science variable to display",
            choices = NULL
          ),
          numericInput("min", "Sci Axis Minimum", NULL, min = 1, max = 100),
          numericInput("max", "Sci Axis Maximum", NULL, min = 1, max = 100)
        )
      ),
      column(
        9,
        h4("Brush and double-click to zoom (double-click again to reset)"),
        plotOutput(
          "sciplot",
          dblclick = "sciplot_dblclick",
          brush = brushOpts(id = "sciplot_brush",
                            resetOnNew = TRUE)
        )
      ))
  ),
  #flight variable settings
  tabPanel("Flight Data",
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
             selectizeInput("flight_var",
                                "Which flight variable(s) to display",
                                choices = NULL,
                            multiple = TRUE,
                            options = list(plugins= list('remove_button'))))),
             column(
               9,
               h4("Brush and double-click to zoom (double-click again to reset)"),
               plotOutput(
                 "flightplot",
                 dblclick = "flightplot_dblclick",
                 brush = brushOpts(id = "flightplot_brush",
                                   resetOnNew = TRUE)
               )
             )
           )),
  #sound velocity tab
  tabPanel("Sound Velocity",
           fluidRow(
             column(3,
                    wellPanel(
                      numericInput(
                        "soundmin",
                        "Sound Axis Minimum",
                        NULL,
                        min = 1,
                        max = 100
                      ),
                      numericInput(
                        "soundmax",
                        "Sound Axis Maximum",
                        NULL,
                        min = 1,
                        max = 100
                      )
                    )),
             column(9,
                    plotOutput("soundplot"))
           ), ),
  #sound velocity tab
)
)

# Define server logic
server <- function(input, output, session) {
  
    #glider = readRDS(fileList[1])
  glider = reactive({
    #req(input$mission)
    readRDS(paste0("./Data/", input$mission, ".rds"))
  })
    
  observeEvent(input$load, {
  #pull out science variables
  scivars <- glider() %>%
    select(starts_with("sci")) %>%
    colnames()
  
  #pull out flight variables
  flightvars <- glider() %>%
    select(!starts_with("sci")) %>%
    colnames()
  
  #get start/end days
  updateDateInput(session, "date1", NULL, min = min(glider()$m_present_time), max = max(glider()$m_present_time), value = min(glider()$m_present_time))
  updateDateInput(session, "date2", NULL, min = min(glider()$m_present_time), max = max(glider()$m_present_time), value = max(glider()$m_present_time))
  updateSelectInput(session, "display_var", NULL, choices = c(scivars))
  updateSelectizeInput(session, "flight_var", NULL, choices = c(flightvars), selected = "m_roll")
  showNotification("Data primed", type = "message")
  
  raw_sf <- st_read(paste0("./KML/", input$mission, ".kml"),
                    layer = "Surfacings")
  
  KML_sf <- raw_sf %>%
    select(Name)
  
  map_sf <- KML_sf[2:(nrow(KML_sf) - 1),]
  
  mapUp <- KML_sf %>%
    mutate(long = st_coordinates(.)[,1],
           lat = st_coordinates(.)[,2]) %>%
    st_drop_geometry()

  output$missionmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", 
                       group = "Esri.WorldImagery") %>%
      addWMSTiles('https://gis.charttools.noaa.gov/arcgis/rest/services/MCS/NOAAChartDisplay/MapServer/exts/MaritimeChartService/WMSServer',
                  layers = "0,1,2,3",
                  options = WMSTileOptions(format = "image/png", transparent = T),
                  attribution = "© NOAA",
                  group = "NOAA") %>%
      addLayersControl(baseGroups = c('NOAA', 'Esri.WorldImagery')) %>%
      addCircles(data = map_sf,
                 color = "gold",
                 popup = map_sf$Name
      ) %>%
      addAwesomeMarkers(
        lat = mapUp[1, 3],
        lng = mapUp[1, 2],
        label = "Starting point",
        icon = icon.start
      ) %>%
      addAwesomeMarkers(
        lat = mapUp[nrow(mapUp), 3],
        lng = mapUp[nrow(mapUp), 2],
        label = "Ending point",
        icon = icon.end
      )
  })
  
  })
  
  
  
  
  #ranges for plot zooms
  rangefli <- reactiveValues(x = NULL, y = NULL)
  rangesci <- reactiveValues(x = NULL, y = NULL)
  
  #dynamically filter out viewable area and calculate SV
  chunk <- eventReactive(input$initialize, {
    filter(glider(), m_present_time >= input$date1 & m_present_time <= input$date2) %>%
      filter(status %in% c(input$status)) %>%
      #filter(!(is.na(input$display_var) | is.na(m_depth))) %>%
      filter(m_depth >= input$min_depth) %>%
      mutate(osg_salinity = ec2pss(sci_water_cond*10, sci_water_temp, sci_water_pressure*10)) %>%
      mutate(soundvel1 = c_Coppens1981(m_depth,
                                       osg_salinity,
                                       sci_water_temp))
    #possible add ... from masterdata
      #mutate(new_water_depth = m_water_depth * (1500/soundvel1))
  })
  
  #science plot
  output$sciplot <- renderPlot({
    ggplot(data = filter(chunk(), !is.na(.data[[input$display_var]])),#dynamically filter the sci variable of interest
           aes(x=m_present_time,
                        y=m_depth,
                        z=.data[[input$display_var]])) +
      geom_point(
        aes(color = .data[[input$display_var]]),
        na.rm = TRUE
      ) +
      ylab("Depth (m)") +
      coord_cartesian(xlim = rangesci$x, ylim = rangesci$y, expand = FALSE) +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(input$min, input$max)) +
      geom_point(data = filter(chunk(), m_water_depth > 0),
                 aes(y = m_water_depth),
                 size = 0.1,
                 na.rm = TRUE
      ) +
      theme_minimal()
  })
  
  #flight plot zoom/click
  observeEvent(input$flightplot_dblclick, {
    brush <- input$flightplot_brush
    if (!is.null(brush)) {
      rangefli$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      rangefli$y <- c(brush$ymin, brush$ymax)
      
    } else {
      rangefli$x <- NULL
      rangefli$y <- NULL
    }
  })
  
  #science plot zoom/click
  observeEvent(input$sciplot_dblclick, {
    brush <- input$sciplot_brush
    if (!is.null(brush)) {
      rangesci$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      #REVERSED RANGE DUE TO REVERSED Y see: https://github.com/tidyverse/ggplot2/issues/4021
      rangesci$y <- c(brush$ymax, brush$ymin)
      
    } else {
      rangesci$x <- NULL
      rangesci$y <- NULL
    }
  })
  
  #flight plot
  output$flightplot <- renderPlot({
    # if (input$flight_var == "m_roll") {
    #   flightxlabel <- "roll"
    # } else if (input$flight_var == "m_heading") {
    #   flightxlabel <- "heading"
    # }
    
    ggplot(
      data =
        select(chunk(), m_present_time, all_of(input$flight_var)) %>%
        pivot_longer(
          cols = !m_present_time,
          names_to = "variable",
          values_to = "count") %>%
        filter(!is.na(count)),
      aes(x = m_present_time,
          y = count,
          color = variable,
          shape = variable)) +
      geom_point() +
      coord_cartesian(xlim = rangefli$x, ylim = rangefli$y, expand = FALSE) +
      theme_minimal()
    
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
  
  #sound velocity plot
  output$soundplot <- renderPlot({
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
      ylab("Depth (m)") +
      scale_y_reverse() +
      scale_colour_viridis_c(limits = c(limits = c(input$soundmin, input$soundmax)))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
