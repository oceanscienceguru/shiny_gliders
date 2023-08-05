
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
                 menuItem("Full Mission Data",
                          icon = icon("calendar"),
                          tabName = "fullMissData"),
                          
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
                 tabItem(tabName = "fullMissData",
                         fullData_ui("gliding")
                 ),
                 
                 tabItem(tabName = "dataImport",
                         box(
                         glide(
                           height = "500px",
                           screen(
                             next_condition = "input.uploadGliderName.length > 0",
                             h3("Which glider is this?"),
                             selectInput(
                               inputId = "uploadGliderName",
                               label = "Select glider name:",
                               choices = c("",
                                           fleetGliders$V1),
                               selected = NULL
                             )),
                           screen(
                             h3("Upload whole-mission .ssv"),
                             p("SSV must have: ", code("m_present_time, m_gps_lat,
                               m_gps_lon, sci_water_cond, sci_water_temp, sci_water_pressure")),
                             br(),
                             checkboxInput(
                               inputId = "generateMap",
                               label = "Generate map using SSV?",
                               value = TRUE
                             ),
                             fileInput(
                               inputId = "upload",
                               label = "Select file:",
                               multiple = FALSE,
                               accept = c(#"text/SSV",
                                          #".kml",
                                          #".rds",
                                          #".Rdata",
                                          #".*bd",
                                          ".ssv"
                                          )
                             )
                           )
                           ))
                         # fluidPage(
                         # 
                         #   #file upload row
                         #   wellPanel(
                         #     fileInput(
                         #       inputId = "upload",
                         #       label = "Upload New Mission Data",
                         #       multiple = FALSE,
                         #       accept = c("text/SSV",
                         #                  ".ssv",
                         #                  ".rds",
                         #                  ".Rdata",
                         #                  ".*bd",
                         #                  ".kml")
                         #     ),
                         #     selectInput(
                         #       inputId = "uploadGliderName",
                         #       label = "Which glider?",
                         #       choices = c("usf-bass",
                         #                   "usf-stella"),
                         #       selected = NULL
                         #     ),
                         #     tableOutput('uploadTable')
                         #     # selectInput(
                         #     #   inputId = "mission",
                         #     #   label = "Which mission data to display",
                         #     #   choices = c(missionList),
                         #     #   selected =  NULL
                         #     #
                         #   ))


                         )
                 
                 ))
           )
)