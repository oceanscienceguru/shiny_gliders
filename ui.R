
  #force notifications to center of page
  tags$head(tags$style(
    HTML(
      ".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
    )
  ),
           dashboardPage(
             dashboardHeader(title = "The Brewery"),
             dashboardSidebar(
               sidebarMenu(id = "tabs",
                 menuItem("Piloting Dashboard", startExpanded = TRUE,
                          icon = icon("dashboard"),
                          radioButtons(inputId = "gliderSelect",
                                       label = "Pick Your Glider",
                                       choices = deployedGliders$Name,
                                       selected = tail(deployedGliders$Name,1)),
                          hr(),
                          menuSubItem("Dashboard", tabName = "dashboard"),
                          menuSubItem("Routing", tabName = "routing")),
                 menuItem("Current Mission Data",
                          tabName = "currMissData",
                          icon = icon("plane")),
                 menuItem("Archived Mission Data",
                          icon = icon("calendar"),
                          expandedName = "oldData",
                          menuSubItem("View Data", tabName = "archMissData"),
                          hr(),
                          actionButton(
                            inputId = "load",
                            label = "Load Mission Data",
                            icon("plane"),
                            style =
                              "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                          ),
                          selectInput(
                            inputId = "mission",
                            label = "Which mission data to display",
                            choices = NULL,
                            selected =  NULL
                          ),
                          br(),
                          dateInput(
                            inputId = "date1",
                            label = "Start Date:",
                            value = NULL,
                            min = NULL,
                            max = NULL,
                            format = "mm/dd/yy"
                          ),
                          dateInput(
                            inputId = "date2",
                            label = "End Date:",
                            value = NULL,
                            min = NULL,
                            max = NULL,
                            format = "mm/dd/yy"
                          ),
                          numericInput(
                            inputId = "min_depth",
                            label = "Depth Minimum",
                            value = 3,
                            min = 0,
                            max = 1000
                          ),
                          numericInput(
                            inputId = "max_depth",
                            label = "Depth Maximum",
                            value = 150,
                            min = 0,
                            max = 1000
                          ),
                          hr()
                          ),
                 menuItem("Data Import", 
                          tabName = "dataImport", 
                          icon = icon("th"))
               )
             ),
             dashboardBody(
               tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
               tabItems(
                 tabItem(tabName = "dashboard",
                         gliderDashboard_ui("display")
                 ),
                 tabItem(tabName = "currMissData",
                         currentData_ui("curDisplay"),
                         ),
                 tabItem(tabName = "routing",
                         routing_ui("curRoute")
                         ),
                 tabItem(tabName = "archMissData",
                         fluidPage(
                           column(12,
                                  #mainPanel(#science variable settings
                                  tabBox(
                                    width = 12,
                                    tabPanel(title = "Map",
                                             leafletOutput(outputId = "missionmap",
                                                           height = "600px")),
                                    tabPanel(title = "Science Data",
                                             column(3,
                                                    wellPanel(
                                                      selectInput(
                                                        inputId = "display_var",
                                                        label = "Which science variable to display",
                                                        choices = NULL
                                                      ),
                                                      #title = "Color Scale Override",
                                                      numericInput(inputId = "min",
                                                                   label = "Sci Axis Minimum",
                                                                   NULL),
                                                      numericInput(inputId = "max",
                                                                   label = "Sci Axis Maximum",
                                                                   NULL),
                                                      downloadButton('downloadSciPlot')
                                                    )),
                                             column(
                                               9,
                                               #"Brush and double-click to zoom (double-click again to reset)",
                                               plotOutput(
                                                 outputId = "sciPlot",
                                                 dblclick = "sciPlot_dblclick",
                                                 brush = brushOpts(id = "sciPlot_brush",
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
                                                          inputId = "flight_var",
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
                                                   outputId = "fliPlot",
                                                   dblclick = "fliPlot_dblclick",
                                                   brush = brushOpts(id = "fliPlot_brush",
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
                                                        numericInput(inputId = "soundmin",
                                                                     label = "Sound Axis Minimum",
                                                                     NULL),
                                                        numericInput(inputId = "soundmax",
                                                                     label = "Sound Axis Maximum",
                                                                     NULL),
                                                        downloadButton('downloadSouPlot')
                                                      )),
                                               column(9,
                                                      plotOutput(outputId = "souPlot",
                                                                 height = "600px"
                                                      ) %>% withSpinner(color="#0dc5c1")
                                               )
                                             ),),
                                    selected = "Map"
                                  )
                           )
                         )
                 ),
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
)