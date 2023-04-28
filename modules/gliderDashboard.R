##### Main glider dashboard module #########
library(shiny)

id <- "dashboard"
ns <- NS(id)

gliderDashoard_ui <- function(id) {

  tabItem(tabName = "dashboard",
          fluidRow(
            radioButtons(inputId = "gliderSelect",
                         label = "Pick Your Glider",
                         choices = deployedGliders$Name,
                         selected = tail(deployedGliders$Name,1)),
            box(
              leafletOutput(outputId = "missionmapLive")
            ),
            box(
              slickROutput("img")
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
          # fluidRow(
          #   box(
          #     plotOutput(
          #       outputId = "battplot",
          #     ),
          #   ),
          # ),
  ),
  fluidRow(
    text_ui(NS(id, "metric")),
    plot_ui(NS(id, "metric"))
  )
  
}

gliderDashoard_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    load(paste0("/echos/", input$gliderSelect, "/glider_live.RData"))
    
    
    text_server("metric", df, vbl, threshhold)
    plot_server("metric", df, vbl, threshhold)
    
  })
  
}