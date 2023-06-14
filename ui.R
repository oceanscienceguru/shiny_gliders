
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