yoPlot <- function(gliderName, inGliderdf, plotVar){
  
  # gliderName <- "usf-gansett"
  # load(paste0("/echos/", gliderName, "/glider_live.RData"))
  # plotVar <- c("sci_water_temp", "sci_oxy3835_oxygen", "sci_flbbcd_chlor_units")
  # inGliderdf <- gliderdf %>%
  #   filter(yo_id == 98 & cast == "Downcast") %>%
  #   select(c(m_present_time, osg_i_depth, yo_id, i_lat, i_lon, any_of(plotVar)))
  # 
  # %>%
  #   pivot_longer(cols = !c(m_present_time, osg_i_depth)) %>%
  #   filter(value > 0)

fig <- plot_ly()

  # need to use lapply because for loop is lazy eval
invisible(lapply(seq(length(plotVar)), FUN = function(i) {
    #init data
    dfk <- data.frame(x = inGliderdf[[plotVar[i]]], y = inGliderdf[["osg_i_depth"]], m_present_time = inGliderdf[["m_present_time"]])
    
    #grab global fig and reassign to global fig
    fig <<- get("fig", envir = environment()) %>% 
    #plot_ly() %>%
      add_trace(#fig,
                data = dfk,
                x = ~x,
                y = ~y,
                type = "scatter",
                mode = "line",
                #color = 
                name = paste0(plotVar[i]),
                xaxis = paste0("x", i),
                text = ~paste0("Date: ", m_present_time, "\ni_depth: ", round(y, 3), "\n",
                               plotVar[i], ": ", round(x, 3)),
                hoverinfo = "text",
                hoverlabel = list(bgcolor = "black")) %>%
      layout(
        yaxis = list(#rangemode = "tozero",
          range = c(max(dfk$y) + (.05*max(dfk$y)), 0)),
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5)
        #xaxis = list(title = paste0(plotVar[i]))
      )
    
    #return(fig)
  }))
  
  out <- 
    subplot(fig,
          shareY = TRUE, titleX = F) %>%
    layout(title = list(text = paste0(gliderName, " real-time data - yo: ", max(inGliderdf$yo_id, na.rm = TRUE), "\n",
                                      "profile position: ", round(mean(inGliderdf$i_lat), 4), ", ", round(mean(inGliderdf$i_lon), 4)),
                        x = 0.03,
                        y = 0.96),
           margin = list(
             t = 50
           ),
           yaxis = list(title = "Depth (m)"),
           showlegend = T,
           #add logo
           images = list(
             list(source = base64enc::dataURI(file = "./www/cms_horiz.png"),
                  xref = "paper",
                  yref = "paper",
                  x= .5,
                  y= 0.03,
                  sizex = 0.2,
                  sizey = 0.2,
                  xanchor="right",
                  yanchor="bottom" 
             ))) %>% 
      #much faster rendering
      toWebGL()
  
  
  ##### Overlapped axes version ####
  
  # m <- list(
  #   #l = 200,
  #   #r = 50,
  #   #b = 100,
  #   t = 50,
  #   pad = 30
  # )
  # 
  # fig %>%
  #   layout(
  #     legend = list(orientation = "h",   # show entries horizontally
  #                   xanchor = "center",  # use center of legend as anchor
  #                   x = 0.5),
  #     yaxis = list(autorange = "reversed"),
  #     xaxis = list(side = "top",
  #       showline = T
  #       #position = 1
  #     ),
  #     xaxis2 = list(overlaying = "x",
  #                   side = "top",
  #                   position = 1,
  #                   anchor="free",
  #                   showgrid = F,
  #                   showline = T
  #     ),
  #     xaxis3 = list(overlaying = "x",
  #                   side = "top",
  #                   position = .9,
  #                   anchor="free",
  #                   showgrid = F,
  #                   showline = T
  #     ),
  #     margin = m
  #   )
  # 
  #   
  # 
  # ###take2
  # 
  # plots <- lapply(plotVar, function(var) {
  #   plot_ly() %>%
  #     add_trace(#fig,
  #               data = inGliderdf,
  #               x = ~as.formula(paste0("~", var)),
  #               y = ~osg_i_depth,
  #               type = "scatter",
  #               mode = "line",
  #               #color = 
  #               name = paste0(var, " (units)"),
  #               #xaxis = paste0("x", i),
  #               text = ~paste0("Date: ", m_present_time, "\ni_depth: ", round(y, 3), "\n",
  #                              var, ": ", round(x, 3)),
  #               hoverinfo = "text",
  #               hoverlabel = list(bgcolor = "black")) %>%
  #     layout(
  #       legend = list(orientation = "h",   # show entries horizontally
  #                     xanchor = "center",  # use center of legend as anchor
  #                     x = 0.5),
  #       yaxis = list(autorange = "reversed"))
  #   
  #   
  #   # plot_ly(inGliderdf, y = ~osg_i_depth, x = as.formula(paste0("~", var))) %>%
  #   #   add_lines(name = var)
  # })
  # 
  # 
  # 
  #   #formatting
  #   layout(title = list(text = paste0(gliderName, " real-time data"),
  #                       x = 0.03,
  #                       y = 0.97),
  #          yaxis = list(autorange = "reversed",
  #                       title = "Depth (m)"),
  #          legend = list(title = as.character(plotVar)),
  #          xaxis = list(title = "Date",
  #                       type = 'date',
  #                       tickformatstops = list(
  #                         list(dtickrange=list(NULL, 1000), value="%H:%M:%S.%L ms"),
  #                         list(dtickrange=list(1000, 60000), value="%H:%M:%S"),
  #                         list(dtickrange=list(60000, 3600000), value="%m/%d, %H:%M"),
  #                         list(dtickrange=list(3600000, 86400000), value="%m/%d, %H:%M"),
  #                         list(dtickrange=list(86400000, 604800000), value="%Y/%m/%d"),
  #                         list(dtickrange=list(604800000, "M1"), value="%Y/%m/%d"),
  #                         list(dtickrange=list("M1", "M12"), value="%b '%y"),
  #                         list(dtickrange=list("M12", NULL), value="%Y")
  #                       )),
  #          #add logo
  #          images = list(
  #            list(source = base64enc::dataURI(file = "./www/cms_horiz.png"),
  #                 xref = "paper",
  #                 yref = "paper",
  #                 x= 1.2,
  #                 y= 0.03,
  #                 sizex = 0.2,
  #                 sizey = 0.2,
  #                 xanchor="right",
  #                 yanchor="bottom" 
  #            ))
  #   ) %>%
  #   #explicitly name title
  #   colorbar(title = as.character(plotVar)
  #   ) %>%
  #   config(displayModeBar = FALSE) %>% 
  #   #much faster rendering
  #   toWebGL()
  
  #handle color override conditions
  #plotly needs both limits defined when overriding so provide min/max as appropriate
  # if(colorOverride){
  #   if(is.numeric(colorMin) & !is.numeric(colorMax)){
  #     fig <- fig %>%
  #       colorbar(limits = c(colorMin, max(inGliderdf[[plotVar]]))
  #       )
  #   } else if(!is.numeric(colorMin) & is.numeric(colorMax)){
  #     fig <- fig %>%
  #       colorbar(limits = c(min(inGliderdf[[plotVar]]), colorMax)
  #       )
  #   } else if(is.numeric(colorMin) & is.numeric(colorMax))
  #     fig <- fig %>%
  #       colorbar(limits = c(colorMin, colorMax)
  #       )
  # }
  # 
  #final output
  out
  
}
