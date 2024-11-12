
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
  #includeScript("./www/timezone.js"),
           dashboardPage(
             dashboardHeader(title = app_name),
             dashboardSidebar(
               sidebarMenu(id = "tabs",
                 menuItem("Piloting Dashboard", 
                          startExpanded = TRUE,
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
                 menuItem("Full Datasets", 
                          startExpanded = TRUE,
                          menuSubItem("Full Data Viewer",
                          tabName = "fullMissData",
                          icon = icon("database")),
                          menuSubItem("Mission Reports",
                            tabName = "fullReports",
                            icon = icon("file")
                          )
                 ),
                 # actionButton(
                 #   inputId = "load",
                 #   label = "Load Mission Data",
                 #   icon("plane"),
                 #   style = "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                 #            ),
                 # selectInput(inputId = "mission",
                 #   label = "Which mission data to display?",
                 #   choices = c(missionList_archive),
                 #              selected = tail(missionList_archive, 1)),
                 #menuItem("Multiple Missions", tabName = "multi_miss_data"),

                 # actionButton(
                 #   inputId = ns("load"),
                 #   label = "Load Mission Data",
                 #   icon("plane"),
                 #   style =
                 #     "color: #fff; background-color: #963ab7; border-color: #2e6da4"
                 # ),
                 # br(),
                 # selectInput(
                 #   inputId = ns("mission"),
                 #   label = "Which mission data to display",
                 #   choices = NULL,
                 #   selected =  NULL
                 # ),

                 menuItem("Data Import",
                          tabName = "dataImport",
                          icon = icon("file-import")),
                menuItem("Utilities",
                         tabName = "data_utilities",
                         icon = icon("wrench"))
               )
             ),
             dashboardBody(
               tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
               tabItems(
                 tabItem(tabName = "dashboard",
                         gliderDashboard_ui("display")
                 ),
                 tabItem(tabName = "currMissData",
                         current_data_handler_ui("curDisplay"),
                         ),
                 tabItem(tabName = "routing",
                         routing_ui("curRoute")
                         ),
                 tabItem(tabName = "fullMissData",
                         data_viewer_ui("gliding")
                 ),
                 # tabItem(tabName = 'multi_miss_data',
                 #         multi_mission_ui("gliders")),
                 tabItem(tabName = "fullReports",
                         mission_overview_ui("glideReport")),
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
