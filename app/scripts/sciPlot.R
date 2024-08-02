sciPlot <- function(gliderName, inGliderdf, gliderFlightdf, plotVar, liveData = FALSE, colorMin = NULL, colorMax = NULL, logoFile = NULL){
  
  #gliderName <- name of glider for plot displays
  #inGliderdf <- dataframe containing m_present_time, osg_i_depth and all of plotVar
  #gliderFlightdf <- dataframe containing m_present_time and m_water_depth. Can be same as inGliderdf
  #plotVar <- vector of strings of column names of variables of interest within inGliderdf
  #liveData <- boolean, whether data is "real-time" or not
  #colorMin <- color scale minimum value override
  #colorMax <- color scale maximum value override
  
  #check if incoming color values are legitimate (only numeric if non-Null or non-NA passed in on reactive)
  if(is.numeric(colorMin) | is.numeric(colorMax)){
    colorOverride = TRUE
  } else {
    colorOverride = FALSE
  }
  
  #setup color schemes
  if (plotVar == "sci_water_temp") {
    oceanColor = cmocean("thermal")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "sci_water_pressure") {
    oceanColor = cmocean("deep")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "sci_water_cond") {
    oceanColor = cmocean("haline")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "sci_suna_nitrate_concentration") {
    oceanColor = cmocean("tempo")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "sci_flbbcd_chlor_units"|
             plotVar == "sci_bbfl2s_chlor_scaled" ) {
    oceanColor = cmocean("algae")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "sci_flbbcd_cdom_units"|
             plotVar == "sci_bbfl2s_cdom_scaled" ) {
    oceanColor = cmocean("matter")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "sci_flbbcd_bb_units"|
             plotVar == "sci_bbfl2s_bb_scaled" ) {
    oceanColor = cmocean("turbid")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "sci_oxy3835_oxygen" |
             plotVar == "sci_oxy4_oxygen" ) {
    oceanColor = cmocean("oxy")(length(unique(inGliderdf[[plotVar]])))
  } else if (startsWith(plotVar, "sci_ocr507")) {
    oceanColor = cmocean("solar")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "Salinity"){
    plotVar <- "osg_salinity"
    oceanColor = cmocean("haline")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "SV Plot"){
    plotVar <- "osg_soundvel1"
    oceanColor = cmocean("speed")(length(unique(inGliderdf[[plotVar]])))
  } else if (plotVar == "Density"){
    plotVar <- "osg_rho"
    oceanColor = cmocean("dense")(length(unique(inGliderdf[[plotVar]])))
  } else {
    oceanColor = NULL
  }
  
  #set up units
  if (exists(plotVar, where = sensor_defs)){
    varUnits <- sensor_defs[[plotVar]]$attrs$units
  } else {
    varUnits <- NULL
  }

  
  # #requires arranging data in order or bgcolor breaks in webGL
  # X <- scienceChunk_live() %>%
  #   ungroup() %>%
  #   arrange(.data[[input$display_varLive]])
  # 
  # #factorize color vector for bgcolor
  # oceanColor <- oceanColor[factor(X[[input$display_varLive]])]
  
  #build figure
  fig <- plot_ly(data = inGliderdf,
                 x = ~as.character(m_present_time),
                 y = ~osg_i_depth
  ) %>%
    #main color trace
    add_trace(   type = "scatter",
                 mode = "markers",
                 color = ~.data[[plotVar]],
                 colors = oceanColor,
                 text = ~paste0("Date: ", m_present_time, "\ni_depth: ", round(osg_i_depth, 3), "\n",
                                plotVar, ": ", round(.data[[plotVar]], 3)),
                 hoverinfo = "text",
                 hoverlabel = list(bgcolor = "black")
    ) %>%
    #water depth trace
    add_trace(data = filter(gliderFlightdf, m_water_depth > 0),
              y = ~m_water_depth,
              type = "scatter",
              mode = "markers",
              text = ~paste0("Date: ", m_present_time, "\nm_water_depth: ", round(m_water_depth, 3)),
              hoverinfo = "text",
              marker = list(size = 2, color = "black"),
              showlegend = FALSE
    ) %>%
    #formatting
    layout(yaxis = list(autorange = "reversed",
                        title = "Depth (m)"),
           legend = list(title = as.character(plotVar)),
           xaxis = list(title = "Date",
                        type = 'date',
                        tickformatstops = list(
                          list(dtickrange=list(NULL, 1000), value="%H:%M:%S.%L ms"),
                          list(dtickrange=list(1000, 60000), value="%H:%M:%S"),
                          list(dtickrange=list(60000, 3600000), value="%m/%d, %H:%M"),
                          list(dtickrange=list(3600000, 86400000), value="%m/%d, %H:%M"),
                          list(dtickrange=list(86400000, 604800000), value="%Y/%m/%d"),
                          list(dtickrange=list(604800000, "M1"), value="%Y/%m/%d"),
                          list(dtickrange=list("M1", "M12"), value="%b '%y"),
                          list(dtickrange=list("M12", NULL), value="%Y")
                        ))
    ) %>%
    #explicitly name title
    colorbar(title = paste0(plotVar, "\n", varUnits)
    ) %>%
    #much faster rendering
    toWebGL()
  
  #add logo
  if(!is.null(logoFile)){
    fig <- fig %>%
      layout(           
      images = list(
        list(source = base64enc::dataURI(file = logoFile),
             xref = "paper",
             yref = "paper",
             x= 1.2,
             y= 0.03,
             sizex = 0.2,
             sizey = 0.2,
             xanchor="right",
             yanchor="bottom" 
        )))
  }
  
  if(isTRUE(liveData)){
    fig <- fig %>%
      layout(title = list(text = paste0(gliderName, " real-time data"),
                          x = 0.03,
                          y = 0.97))
  } else {
    fig <- fig %>%
      layout(title = list(text = paste0(gliderName, " delayed data"),
                          x = 0.03,
                          y = 0.97))
  }
  
  #handle color override conditions
  #plotly needs both limits defined when overriding so provide min/max as appropriate
  if(colorOverride){
    if(is.numeric(colorMin) & !is.numeric(colorMax)){
      fig <- fig %>%
        colorbar(limits = c(colorMin, max(inGliderdf[[plotVar]]))
        )
    } else if(!is.numeric(colorMin) & is.numeric(colorMax)){
      fig <- fig %>%
        colorbar(limits = c(min(inGliderdf[[plotVar]]), colorMax)
        )
    } else if(is.numeric(colorMin) & is.numeric(colorMax))
    fig <- fig %>%
      colorbar(limits = c(colorMin, colorMax)
    )
  }
  
  #final output
  fig
  
  }