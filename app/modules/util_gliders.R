util_gliders_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Login UI element from shinyauthr
    shinyauthr::loginUI(ns("login")),  
    uiOutput(ns("module_ui")),  # Protected content (conditionally rendered)
    div(class = "pull-right", shinyauthr::logoutUI(id = ns("logout"))),  # Logout button
  )
}

util_gliders_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Authentication logic using shinyauthr's loginServer
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = users,  # User data (you need to define this)
      user_col = "user", 
      pwd_col = "password_hashed",  # Make sure this is correct
      sodium_hashed = TRUE,
      log_out = reactive(logout_init())  # Use sodium for password hashing
    )
    
    # Logout logic
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
    
    # Conditional UI for protected content
    output$module_ui <- renderUI({
      if (credentials()$user_auth) {
        # When logged in, show the protected content UI
        box(
          title = "Deployed Gliders",
          width = 12,
          height = "75vh",
          aceEditor(
            outputId = ns("someID"),
            value = paste(readLines("config.yaml"), collapse = "\n"),
            placeholder = "Connection configuration file"
          ),
          actionButton(ns("save_yaml"), "Save YAML")
        )
      } else {
        # When logged out, show only the login UI
        tagList(
          shinyauthr::loginUI(ns("login"))
        )
      }
    })
    
    # Save YAML functionality (only accessible when logged in)
    observeEvent(input$save_yaml, {
      if (credentials()$user_auth) {
        writeLines(input$someID, con = "config.yaml")
        showNotification("YAML file saved successfully!", type = "message")
      } else {
        showNotification("You must log in first!", type = "error")
      }
    })
    
  })
}
