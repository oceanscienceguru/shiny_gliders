server <- function(input, output, session) { options(shiny.usecairo = TRUE)

  # Check if deployedGliders is empty and show modal if necessary
  observe({
    req(deployed_gliders_df())  # Wait for deployed_gliders_df to be available
    
    if (nrow(deployed_gliders_df()) == 0) {
      showModal(modalDialog(
        title = "No deployed gliders",
        "There are no deployed gliders at this time.",
        easyClose = TRUE
      ))
    }
  })

  # Create deployed_gliders_df in server.R
  deployed_gliders_df <- reactive({
    yaml_data <- read_yaml("config.yaml")  # Update with actual file path
    gliders_data <- yaml_data$gliders
    
    # Filter gliders where 'deployed' is TRUE
    deployed_gliders <- gliders_data %>%
      purrr::keep(~ .x$deployed == TRUE) %>%
      names()  # Get names of gliders

    # Convert to a dataframe
    deployed_gliders_df <- data.frame(glider = deployed_gliders)
    
    return(deployed_gliders_df)
  })
  
  # Render radioButtons based on deployed_gliders_df
  output$gliderSelector <- renderUI({

    gliders <- deployed_gliders_df()$glider  # Get the glider names

    # Render the radioButtons dynamically
    radioButtons(inputId = "gliderSelect",
                 label = "Pick Your Glider",
                 choices = gliders,
                 selected = tail(gliders, 1))  # Default to the last deployed glider
  })
  
  ######### current mission data ########
  observe({
    glider <- input$gliderSelect

    #clientTZ <- input$clientTime$clientTimeZone

    if(input$tabs == "dashboard" && length(glider > 0)){
      gliderDashboard_server("display", glider)
    }
    if(input$tabs == "currMissData" && length(glider > 0)){
      #Code for current mission data on selected glider
      current_data_handler_server("curDisplay", glider)
    }
    if(input$tabs == "routing" && length(glider > 0)){
      routing_server("curRoute", glider)
    }
    if(input$tabs == "fullMissData"){
      data_viewer_server("gliding")
    }
    if(input$tabs == "fullReports"){
      mission_overview_server("glideReport")
    }
    if(input$tabs == "data_utilities"){
      util_gliders_server("glidemod")
    }
    # if(input$tabs == "multi_miss_data"){
    #   multi_mission_server("gliders")
    # }

  })

  ####### File Upload/Processing #########
  observeEvent(input$upload, {
    #get file extension
    ext <- tools::file_ext(input$upload$name)

    #if SSV
    if (ext == "ssv") {
      message("SSV!")
      # Create a Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Processing SSV", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())

      # Create a callback function to update progress.
      # Each time this is called:
      # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
      #   distance. If non-NULL, it will set the progress to that value.
      # - It also accepts optional detail text.
      n <- 7
      updateProgress <- function(value = NULL, detail = NULL) {
        progress$inc(amount = 1/n, detail = detail)
      }
      newGlider <- ssv_to_rds(inputFile = input$upload$datapath,
                              missionNum = input$upload$name,
                              gliderName = input$uploadGliderName,
                              mapGen = input$generateMap,
                              updateProgress = updateProgress)
      session$reload()
      #if kml
    } else if (ext == "kml"){
      message("KML!")
      file.copy(input$upload$datapath, "./KML")
      session$reload()
      #file.rename(f)
    } else if (ext == "kmz"){
      showModal(modalDialog(
        title = "Warning",
        ".kmz is not accepted, .kml ONLY",
        easyClose = TRUE
      ))
      #otherwise, error
    } else {
      showModal(modalDialog(
        title = "Warning",
        "Please upload .ssv or .kml only",
        easyClose = TRUE
      ))
    }

    #topGlider <- head(newGlider)

    #showNotification(paste0(outputName, " saved"))
  })
  # output$uploadTable <- renderTable({
  #   req(input$upload)
  #
  #   topGlider
  # })



}
