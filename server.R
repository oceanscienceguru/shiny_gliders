server <- function(input, output, session) { options(shiny.usecairo = TRUE)
  
  ######### current mission data ########
  observe({
    glider <- input$gliderSelect

    gliderDashboard_server("display", glider)
    
    if(input$tabs == "currMissData"){
      #Code for current mission data on selected glider
    currentData_server("curDisplay", glider)
    }
    if(input$tabs == "routing"){
      routing_server("curRoute", glider)
    }

  })
  
  ####### archived flight data ########

  
  fullData_server("gliding")
  
  ###### download handlers #########
  
  output$downloadSciPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_sci.png")},
    content = function(file){
      ggsave(file,
             gg1(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadFliPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_fli.png")},
    content = function(file){
      ggsave(file,
             gg2(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadSouPlot <- downloadHandler(
    filename = function(){paste(input$mission, "_SV.png")},
    content = function(file){
      ggsave(file,
             gg3(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadEchoPlot <- downloadHandler(
    filename = function(){paste(input$echo, "_pseudo.png")},
    content = function(file){
      ggsave(file,
             gg4(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadEchoHist <- downloadHandler(
    filename = function(){paste(input$echo, "_pseudo.png")},
    content = function(file){
      ggsave(file,
             gg5(),
             width = 16,
             height = 9)
    }
  )
  
  output$downloadEchoHist2 <- downloadHandler(
    filename = function(){paste(input$echo, "_pseudo.png")},
    content = function(file){
      ggsave(file,
             gg6(),
             width = 16,
             height = 9)
    }
  )
  
  ####### File Upload/Processing #########
  observeEvent(input$upload, {
    #get file extension
    ext <- tools::file_ext(input$upload$name)
    
    #if SSV
    if (ext == "ssv") {
      message("SSV!")
      newGlider <- ssv_to_rds(inputFile = input$upload$datapath,
                              missionNum = input$upload$name,
                              gliderName = input$uploadGliderName,
                              mapGen = input$generateMap)
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
