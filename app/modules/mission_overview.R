#Define the outer module UI
mission_overview_ui <- function(id) {
  ns <- NS(id)
  #Placeholder UI that will dynamically render either the custom or default UI
  tagList(
    fluidPage(
      box(title = "Mission Selection",
          width = 12,
          collapsible = TRUE,
        # actionButton(
        #   inputId = ns("load"),
        #   label = "Load Mission Data",
        #   icon("plane"),
        #   style =
        #     "color: #fff; background-color: #963ab7; border-color: #2e6da4"
        # ),
        # br(),
        selectizeInput(
          inputId = ns("mission"),
          label = "Which mission data to display",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
      ),
      box(
        width = 12,
      htmlOutput(ns("infographic")),
      )
    )
  )
}

mission_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    mission_reports <- list.files(path = paste0(fullDir, "/reports/"),
                                   pattern = "*.html") %>%
      str_remove(., pattern = "_overview.html")
    
    updateSelectizeInput(session, "mission", NULL, choices = c(mission_reports), selected = tail(mission_reports, 1))
    
    shiny::addResourcePath("full", fullDir)
    
    output$infographic <- renderUI({
      req(input$mission)
      tags$div(
        style = "width: 100%; height: 100vh;",
        tags$iframe(
          seamless = "seamless",
          src = paste0("full/reports/", input$mission, "_overview.html"),
          style = "width: 100%; height: 100%; border: none;"
        )
      )
    })
    
    
  }
  )
}