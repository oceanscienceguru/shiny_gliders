#Define the outer module UI
current_data_handler_ui <- function(id) {
  ns <- NS(id)
  #Placeholder UI that will dynamically render either the custom or default UI
  tagList(
    uiOutput(ns("dynamic_ui"))
  )
}

current_data_handler_server <- function(id, gliderName) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Check if custom module file exists
    module_file <- file.path(paste0("./modules/", sanitizeGliderName(gliderName), ".R"))

    if (file.exists(module_file)) {
      #Load custom module functions
      ui_func <- get(paste0(sanitizeGliderName(gliderName), "_ui"))
      server_func <- get(paste0(sanitizeGliderName(gliderName), "_server"))
    } else {
      #Load default module functions
      ui_func <- currentData_ui
      server_func <- currentData_server
    }

    #Set the UI directly
    output$dynamic_ui <- renderUI({
      ui_func(ns("squid"))  #Render the selected UI with namespace
    })

    #Call the selected server function with namespace
    server_func("squid", gliderName, session = session)
  })
}

