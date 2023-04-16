
  #force notifications to center of page
  # tags$head(tags$style(
  #   HTML(
  #     ".shiny-notification {
  #            position:fixed;
  #            top: calc(50%);
  #            left: calc(50%);
  #            }
  #            "
  #   )
  # )),
           dashboardPage(
             dashboardHeader(title = "The Brewery"),
             dashboardSidebar(
               sidebarMenu(id = "tabs",
                 menuItem("Piloting Dashboard", 
                          tabName = "dashboard", 
                          icon = icon("dashboard")),
                 menuItem("Current Mission Data", 
                          tabName = "currMissData", 
                          icon = icon("plane")),
                 menuItem("Archived Mission Data", 
                          tabName = "archMissData", 
                          icon = icon("calendar")),
                 menuItem("Data Import", 
                          tabName = "dataImport", 
                          icon = icon("th"))
               )
             ),
             dashboardBody(
               tabItems(
                 tabItem(tabName = "dashboard",
                         fluidRow(
                         box(
                           leafletOutput(outputId = "missionmapLive")
                           ),
                         box(
                           plotOutput(
                             outputId = "leakplot",
                           ),
                         # box(
                         #   plotOutput(
                         #     outputId = "battplot",
                         #   ) %>% withSpinner(color="#0dc5c1")
                         # ),
                         ),
                         fluidRow(
                           valueBoxOutput(
                             "recoBox",
                             width = 2),
                         valueBoxOutput(
                                        "LDBox",
                                        width = 2),
                         valueBoxOutput(
                                        "battBox",
                                        width = 2),
                         valueBoxOutput(
                           "powerBox1",
                           width = 2),
                         valueBoxOutput(
                           "powerBox3",
                           width = 2),
                         valueBoxOutput(
                           "powerBoxall",
                           width = 2),
                         ),
                         fluidRow(
                           box(
                             slickROutput("slick_output")
                           ),
                           ),
                         ),
                 ),
                 tabItem(tabName = "currMissData",
                         fluidPage(
                           tabsetPanel(
                             tabPanel(title = "Piloting",
                                      column(2,
                                             wellPanel(h4("Data Filtering"),
                                                       dateInput(
                                                         inputId = "date1Live",
                                                         label = "Start Date:",
                                                         value = NULL,
                                                         min = NULL,
                                                         max = NULL,
                                                         format = "mm/dd/yy"
                                                       ),
                                                       dateInput(
                                                         inputId = "date2Live",
                                                         label = "End Date:",
                                                         value = NULL,
                                                         min = NULL,
                                                         max = NULL,
                                                         format = "mm/dd/yy"
                                                       ),
                                                       # numericInput(
                                                       #   inputId = "min_depthLive",
                                                       #   label = "Depth Minimum (bar)",
                                                       #   value = NULL,
                                                       #   min = 0,
                                                       #   max = 1000
                                                       # ),
                                                       # numericInput(
                                                       #   inputId = "max_depthLive",
                                                       #   label = "Depth Maximum (bar)",
                                                       #   value = NULL,
                                                       #   min = 0,
                                                       #   max = 1000
                                                       # ),
                                                       # checkboxGroupInput(
                                                       #   inputId = "status",
                                                       #   label = "Dive only?",
                                                       #   choices = c("dive" = "dive",
                                                       #               "climb" = "climb"),
                                                       #   selected = c("dive")
                                                       # )
                                             )),
                                      column(10,
                                             #mainPanel(#science variable settings
                                             tabsetPanel(
                                               # tabPanel(title = "Map",
                                               #          leafletOutput(outputId = "missionmapLive",
                                               #                        height = "600px")),
                                               tabPanel(title = "Science Data",
                                                        column(3,
                                                               wellPanel(
                                                                 selectInput(
                                                                   inputId = "display_varLive",
                                                                   label = "Which science variable to display",
                                                                   choices = NULL
                                                                 ),
                                                                 h4("Color Scale Override"),
                                                                 numericInput(inputId = "minLive",
                                                                              label = "Sci Axis Minimum",
                                                                              NULL),
                                                                 numericInput(inputId = "maxLive",
                                                                              label = "Sci Axis Maximum",
                                                                              NULL),
                                                                 #downloadButton('downloadSciPlot')
                                                               )),
                                                        column(
                                                          9,
                                                          # h4("Brush and double-click to zoom (double-click again to reset)"),
                                                          plotOutput(
                                                            outputId = "sciPlotLive",
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
                                                                     inputId = "flight_varLive",
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
                                                            plotOutput(
                                                              outputId = "fliPlotLive",
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
                                                                   selectInput(inputId = "derivedTypeLive",
                                                                               label = "Which type of plot?",
                                                                               NULL),
                                                                   # numericInput(inputId = "derivedmaxLive",
                                                                   #              label = "Axis Maximum",
                                                                   #              NULL),
                                                                   #downloadButton('downloadSouPlot')
                                                                 )),
                                                          column(9,
                                                                 plotOutput(outputId = "tsPlotLive",
                                                                            #height = "600px"
                                                                 )
                                                          )
                                                        ),),
                                               #selected = "Map"
                                             )
                                      )
                             ),
                             tabPanel(title = "Pseudograms",
                                      column(2,
                                             wellPanel(
                                               selectInput(
                                                 inputId = "echo",
                                                 label = "Which pseudogram to display",
                                                 choices = NULL,
                                                 selected =  NULL
                                               ),
                                               selectInput(
                                                 inputId = "echoColor",
                                                 label = "Color scheme",
                                                 choices = c("EK", "magma", "viridis"),
                                                 selected =  "viridis"
                                               ),
                                               downloadButton('downloadEchoPlot')
                                             )),
                                      column(10,
                                             plotOutput(
                                               outputId = "echoPlot",
                                               #dblclick = "fliPlot_dblclick",
                                               #brush = brushOpts(id = "fliPlot_brush",
                                               #                  resetOnNew = TRUE),
                                               #height = "600px"
                                             ),
                                             hr(),
                                             column(3,
                                                    actionButton(
                                                      inputId = "oldestPgram",
                                                      label = "Oldest",
                                                      #icon("boat"),
                                                      style =
                                                        "color: #fff; background-color: #000000; border-color: #2e6da4"
                                                    ), align = "center"),
                                             column(3,
                                                    actionButton(
                                                      inputId = "prevPgram",
                                                      label = "Previous",
                                                      #icon("boat"),
                                                      style =
                                                        "color: #fff; background-color: #000000; border-color: #2e6da4"
                                                    ), align = "center"),
                                             column(3,
                                                    actionButton(
                                                      inputId = "nextPgram",
                                                      label = "Next",
                                                      #icon("boat"),
                                                      style =
                                                        "color: #fff; background-color: #000000; border-color: #2e6da4"
                                                    ), align = "center"),
                                             column(3,
                                                    actionButton(
                                                      inputId = "latestPgram",
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
                                                 inputId = "fullecho2",
                                                 label = "Plot!",
                                                 #icon("boat"),
                                                 style =
                                                   "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                                               ),
                                               dateRangeInput("echohistrange2", "Date range:",
                                                              start  = NULL,
                                                              end    = NULL,
                                                              min    = NULL,
                                                              max    = NULL,
                                                              format = "mm/dd/yy",
                                                              separator = " - "),
                                               sliderInput("echohour2",
                                                           "Hour:",
                                                           min = 0,  max = 24, value = c(0, 24)),
                                               selectInput(
                                                 inputId = "echoColor2",
                                                 label = "Color scheme",
                                                 choices = c("EK", "magma", "viridis"),
                                                 selected =  "viridis"
                                               ),
                                               downloadButton('downloadEchoHist2')
                                             )),
                                      column(10,
                                             plotOutput(
                                               outputId = "echoTime",
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
                                                 inputId = "fullecho",
                                                 label = "Plot!",
                                                 #icon("boat"),
                                                 style =
                                                   "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                                               ),
                                               dateRangeInput("echohistrange", "Date range:",
                                                              start  = NULL,
                                                              end    = NULL,
                                                              min    = NULL,
                                                              max    = NULL,
                                                              format = "mm/dd/yy",
                                                              separator = " - "),
                                               numericInput(
                                                 inputId = "depthbin",
                                                 label = "Depth Bin Size",
                                                 value = 3,
                                                 min = 1,
                                                 max = 1000
                                               ),
                                               sliderInput("echohour",
                                                           "Hour:",
                                                           min = 0,  max = 24, value = c(0, 24)),
                                               downloadButton('downloadEchoHist')
                                             )),
                                      column(10,
                                             plotOutput(
                                               outputId = "echoHist",
                                               #dblclick = "fliPlot_dblclick",
                                               #brush = brushOpts(id = "fliPlot_brush",
                                               #                  resetOnNew = TRUE),
                                               #height = "600px"
                                             ) %>% withSpinner(color="#0dc5c1")
                                      ))
                           ))
                         
                         
                         
                         ),
                 # tabItem(tabname = "archMissData",
                 #         fluidPage(
                 #           column(2,
                 #                  #parameter input row
                 #                  #sidebarLayout(
                 #                  wellPanel(#title = "Mission Selection",
                 #                            actionButton(
                 #                              inputId = "load",
                 #                              label = "Load Mission Data",
                 #                              icon("plane"),
                 #                              style =
                 #                                "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                 #                            ),
                 #                            selectInput(
                 #                              inputId = "mission",
                 #                              label = "Which mission data to display",
                 #                              choices = NULL,
                 #                              selected =  NULL
                 #                            )),
                 #                  wellPanel(#title = "Data Filtering",
                 #                            # actionButton(
                 #                            #   inputId = "initialize",
                 #                            #   label = "Visualize",
                 #                            #   icon("arrows-rotate"),
                 #                            #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                 #                            # ),
                 #                            dateInput(
                 #                              inputId = "date1",
                 #                              label = "Start Date:",
                 #                              value = NULL,
                 #                              min = NULL,
                 #                              max = NULL,
                 #                              format = "mm/dd/yy"
                 #                            ),
                 #                            dateInput(
                 #                              inputId = "date2",
                 #                              label = "End Date:",
                 #                              value = NULL,
                 #                              min = NULL,
                 #                              max = NULL,
                 #                              format = "mm/dd/yy"
                 #                            ),
                 #                            numericInput(
                 #                              inputId = "min_depth",
                 #                              label = "Depth Minimum",
                 #                              value = 3,
                 #                              min = 0,
                 #                              max = 1000
                 #                            ),
                 #                            numericInput(
                 #                              inputId = "max_depth",
                 #                              label = "Depth Maximum",
                 #                              value = 150,
                 #                              min = 0,
                 #                              max = 1000
                 #                            ),
                 #                            # checkboxGroupInput(
                 #                            #   inputId = "status",
                 #                            #   label = "Dive only?",
                 #                            #   choices = c("dive" = "dive",
                 #                            #               "climb" = "climb"),
                 #                            #   selected = c("dive")
                 #                            # )
                 #                  )
                 #           ),
                 #           column(10,
                 #                  #mainPanel(#science variable settings
                 #                  tabBox(
                 #                    # tabPanel(title = "Map",
                 #                    #          leafletOutput(outputId = "missionmap",
                 #                    #                        height = "800px")),
                 #                    tabPanel(title = "Science Data",
                 #                             column(3,
                 #                                    wellPanel(
                 #                                      selectInput(
                 #                                        inputId = "display_var",
                 #                                        label = "Which science variable to display",
                 #                                        choices = NULL
                 #                                      ),
                 #                                      #title = "Color Scale Override",
                 #                                      numericInput(inputId = "min",
                 #                                                   label = "Sci Axis Minimum",
                 #                                                   NULL),
                 #                                      numericInput(inputId = "max",
                 #                                                   label = "Sci Axis Maximum",
                 #                                                   NULL),
                 #                                      downloadButton('downloadSciPlot')
                 #                                    )),
                 #                             column(
                 #                               9,
                 #                               #"Brush and double-click to zoom (double-click again to reset)",
                 #                               plotOutput(
                 #                                 outputId = "sciPlot",
                 #                                 dblclick = "sciPlot_dblclick",
                 #                                 brush = brushOpts(id = "sciPlot_brush",
                 #                                                   resetOnNew = TRUE),
                 #                                 height = "600px"
                 #                               ) %>% withSpinner(color="#0dc5c1")
                 #                             )
                 #                    ),
                 #                    #flight variable settings
                 #                    tabPanel(title = "Flight Data",
                 #                             fluidRow(
                 #                               column(3,
                 #                                      wellPanel(
                 #                                        #   selectInput(
                 #                                        #     "flight_var",
                 #                                        #     "Which flight variable(s) to display",
                 #                                        #     choices = c(flightvars),
                 #                                        #     selected = c("m_roll")
                 #                                        #   )
                 #                                        # )),
                 #                                        selectizeInput(
                 #                                          inputId = "flight_var",
                 #                                          label = "Which flight variable(s) to display",
                 #                                          choices = NULL,
                 #                                          multiple = TRUE,
                 #                                          options = list(plugins = list('remove_button'))
                 #                                        ),
                 #                                        downloadButton('downloadFliPlot'),
                 #                                        verbatimTextOutput("summary")
                 #                                      )),
                 #                               column(
                 #                                 9,
                 #                                 #h4("Brush and double-click to zoom (double-click again to reset)"),
                 #                                 plotOutput(
                 #                                   outputId = "fliPlot",
                 #                                   dblclick = "fliPlot_dblclick",
                 #                                   brush = brushOpts(id = "fliPlot_brush",
                 #                                                     resetOnNew = TRUE),
                 #                                   height = "600px"
                 #                                 ) %>% withSpinner(color="#0dc5c1")
                 #                               )
                 #                             )),
                 #                    #sound velocity tab
                 #                    tabPanel(title = "Sound Velocity",
                 #                             fluidRow(
                 #                               column(3,
                 #                                      wellPanel(
                 #                                        numericInput(inputId = "soundmin",
                 #                                                     label = "Sound Axis Minimum",
                 #                                                     NULL),
                 #                                        numericInput(inputId = "soundmax",
                 #                                                     label = "Sound Axis Maximum",
                 #                                                     NULL),
                 #                                        downloadButton('downloadSouPlot')
                 #                                      )),
                 #                               column(9,
                 #                                      plotOutput(outputId = "souPlot",
                 #                                                 height = "600px"
                 #                                      ) %>% withSpinner(color="#0dc5c1")
                 #                               )
                 #                             ),),
                 #                    selected = "Map"
                 #                  )
                 #           )
                 #         )
                 # ),
                 tabItem(tabName = "dataImport",
                         fluidPage(
                           
                           #file upload row
                           wellPanel(
                             fileInput(
                               inputId = "upload",
                               label = "Upload New Mission Data",
                               multiple = FALSE,
                               accept = c("text/SSV", 
                                          ".ssv",
                                          ".rds",
                                          ".Rdata",
                                          ".*bd",
                                          ".kml")
                             ),
                             tableOutput('uploadTable')
                             # selectInput(
                             #   inputId = "mission",
                             #   label = "Which mission data to display",
                             #   choices = c(missionList),
                             #   selected =  NULL
                             # 
                           ))
                         
                         
                         )
                 ))
           )
