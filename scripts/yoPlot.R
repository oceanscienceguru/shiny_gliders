yoPlot <- function(gliderName, inGliderdf, plotVar){
  
  gliderName <- "usf-gansett"
  load(paste0("/echos/", gliderName, "/glider_live.RData"))
  plotVar <- c("sci_water_temp", "sci_oxy3835_oxygen", "sci_flbbcd_chlor_units")
  inGliderdf <- gliderdf %>%
    filter(yo_id == 98 & cast == "Downcast") %>%
    select(c(m_present_time, osg_i_depth, any_of(plotVar)))
  
  # %>%
  #   pivot_longer(cols = !c(m_present_time, osg_i_depth)) %>%
  #   filter(value > 0)
  
  #setup color schemes
  # if (plotVar == "sci_water_temp") {
  #   oceanColor = cmocean("thermal")(length(unique(inGliderdf[[plotVar]])))
  # } else if (plotVar == "sci_water_pressure") {
  #   oceanColor = cmocean("deep")(length(unique(inGliderdf[[plotVar]])))
  # } else if (plotVar == "sci_water_cond") {
  #   oceanColor = cmocean("haline")(length(unique(inGliderdf[[plotVar]])))
  # } else if (plotVar == "sci_suna_nitrate_concentration") {
  #   oceanColor = cmocean("tempo")(length(unique(inGliderdf[[plotVar]])))
  # } else if (plotVar == "sci_flbbcd_chlor_units"|
  #            plotVar == "sci_bbfl2s_chlor_scaled" ) {
  #   oceanColor = cmocean("algae")(length(unique(inGliderdf[[plotVar]])))
  # } else if (plotVar == "sci_flbbcd_cdom_units"|
  #            plotVar == "sci_bbfl2s_cdom_scaled" ) {
  #   oceanColor = cmocean("matter")(length(unique(inGliderdf[[plotVar]])))
  # } else if (plotVar == "sci_flbbcd_bb_units"|
  #            plotVar == "sci_bbfl2s_bb_scaled" ) {
  #   oceanColor = cmocean("turbid")(length(unique(inGliderdf[[plotVar]])))
  # } else if (plotVar == "sci_oxy3835_oxygen" |
  #            plotVar == "sci_oxy4_oxygen" ) {
  #   oceanColor = cmocean("oxy")(length(unique(inGliderdf[[plotVar]])))
  # } else if (startsWith(plotVar, "sci_ocr507")) {
  #   oceanColor = cmocean("solar")(length(unique(inGliderdf[[plotVar]])))
  # } else {
  #   oceanColor = NULL
  # }
  
  #build figure
  fig <- plot_ly(data = inGliderdf)
  
  # need to use lapply because for loop is lazy eval
  invisible(lapply(seq(length(plotVar)), FUN = function(i) {
    #init data
    dfk <- data.frame(x = inGliderdf[[plotVar[i]]], y = inGliderdf[["osg_i_depth"]], m_present_time = inGliderdf[["m_present_time"]])
    
    #grab global fig and reassign to global fig
    fig <<- get("fig", envir = globalenv()) %>%
      add_trace(fig,
                     data = dfk,
                     x = ~x,
                     y = ~y,
                     type = "scatter",
                     mode = "line",
                name = plotVar[i],
                     xaxis = paste0("x", i),
                text = ~paste0("Date: ", m_present_time, "\ni_depth: ", round(y, 3), "\n",
                               plotVar, ": ", round(x, 3)),
                hoverinfo = "text",
                hoverlabel = list(bgcolor = "black")
                     # Add other parameters as needed
    )
    
    return(fig)
  }))
  
  m <- list(
    l = 200,
    r = 50,
    b = 100,
    t = 100,
    pad = 30
  )
  
  fig %>%
    layout(
      yaxis = list(autorange = "reversed"),
      # xaxis1 = list(overlaying = "x",
      #               side = "bottom",
      #               #position = -.3,
      #               anchor="free"
      # ),
      xaxis2 = list(overlaying = "x",
                    side = "bottom",
                    position = .05,
                    anchor="free",
                    automargin = TRUE,
                   
                   showgrid = F,
                   showline= T, linewidth=2, linecolor='red'
      ),
      xaxis3 = list(overlaying = "x",
                    side = "bottom",
                    position = .1,
                    anchor="free",
                    automargin = TRUE,

               showgrid = F,
               showline= T, linewidth=2, linecolor='blue'
      )
      #margin = m
    )
  
    
  
  ###take2
  
  plots <- lapply(plotVar, function(var) {
    plot_ly(inGliderdf, y = ~osg_i_depth, x = as.formula(paste0("~", var))) %>%
      add_lines(name = var)
  })
  subplot(plots, nrows = length(plots), shareY = TRUE, titleX = FALSE)
  
  
  
    
    #water depth trace
    add_trace(data = filter(gliderFlightdf, m_water_depth > 0),
              y = ~m_water_depth,
              type = "scatter",
              mode = "markers",
              text = ~paste0("Date: ", m_present_time, "\nm_water_depth: ", round(m_water_depth, 3)),
              hoverinfo = "text",
              marker = list(size = 1, color = "black"),
              showlegend = FALSE
    ) %>%
    #formatting
    layout(title = list(text = paste0(gliderName, " real-time data"),
                        x = 0.03,
                        y = 0.97),
           yaxis = list(autorange = "reversed",
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
                        )),
           #add logo
           images = list(
             list(source = base64enc::dataURI(file = "./www/cms_horiz.png"),
                  xref = "paper",
                  yref = "paper",
                  x= 1.2,
                  y= 0.03,
                  sizex = 0.2,
                  sizey = 0.2,
                  xanchor="right",
                  yanchor="bottom" 
             ))
    ) %>%
    #explicitly name title
    colorbar(title = as.character(plotVar)
    ) %>%
    config(displayModeBar = FALSE) %>% 
    #much faster rendering
    toWebGL()
  
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
