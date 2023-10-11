fliPlot <- function(gliderName, inGliderdf, plotVar){
  
  # gliderName <- "usf-gansett"
  # load(paste0("/echos/", gliderName, "/glider_live.RData"))
  # plotVar <- c("m_roll", "m_battpos", "m_ballast_pumped")
  # inGliderdf <- gliderdf %>%
  #   #filter(yo_id == 98 & cast == "Downcast") %>%
  #   select(c(m_present_time, osg_i_depth, any_of(plotVar)))
  
  fig <- plot_ly()
  
  # need to use lapply because for loop is lazy eval
  invisible(lapply(seq(length(plotVar)), FUN = function(i) {
    #init data
    dfk <- data.frame(y = inGliderdf[[plotVar[i]]], 
                      osg_i_depth = inGliderdf[["osg_i_depth"]], 
                      m_present_time = inGliderdf[["m_present_time"]])
    
    #grab global fig and reassign to global fig
    fig <<- get("fig", envir = environment()) %>% 
      #plot_ly() %>%
      add_trace(#fig,
        data = dfk,
        x = ~m_present_time,
        y = ~y,
        #z = ~osg_i_depth,
        type = "scatter",
        mode = "line",
        #color = 
        name = paste0(plotVar[i]),
        yaxis = paste0("y", i),
        text = ~paste0("Date: ", m_present_time, "\n",
                       plotVar[i], ": ", round(y, 3), "\n",
                       "i_depth: ", round(osg_i_depth, 3)),
        hoverinfo = "text",
        hoverlabel = list(bgcolor = "black")) %>%
      layout(
        yaxis = list(),
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5)
      #xaxis = list(title = paste0(plotVar[i]))
      )
    
    #return(fig)
  }))
  
  out <- 
    subplot(fig, nrows = length(plotVar),
            shareX = TRUE, titleY = F) %>%
    layout(title = list(text = paste0(gliderName, " real-time flight data"),
                        x = 0.03,
                        y = 0.96),
           margin = list(
             t = 50
           ),
           #yaxis = list(title = "Depth (m)"),
           showlegend = T,
           hovermode = "x unified",
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
  
  # #build figure
  # fig <- plot_ly(data = inGliderdf,
  #                x = ~as.character(m_present_time),
  #                y = ~osg_i_depth
  # ) %>%
  #   #main color trace
  #   add_trace(   type = "scatter",
  #                mode = "markers",
  #                color = ~.data[[plotVar]],
  #                colors = oceanColor,
  #                text = ~paste0("Date: ", m_present_time, "\ni_depth: ", round(osg_i_depth, 3), "\n",
  #                               plotVar, ": ", round(.data[[plotVar]], 3)),
  #                hoverinfo = "text",
  #                hoverlabel = list(bgcolor = "black")
  #   ) %>%
  #   #water depth trace
  #   add_trace(data = filter(gliderFlightdf, m_water_depth > 0),
  #             y = ~m_water_depth,
  #             type = "scatter",
  #             mode = "markers",
  #             text = ~paste0("Date: ", m_present_time, "\nm_water_depth: ", round(m_water_depth, 3)),
  #             hoverinfo = "text",
  #             marker = list(size = 1, color = "black"),
  #             showlegend = FALSE
  #   ) %>%
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
  #   colorbar(title = paste0(plotVar, "\n", varUnits)
  #   ) %>%
  #   #much faster rendering
  #   toWebGL()
  
  #handle color override conditions
  #plotly needs both limits defined when overriding so provide min/max as appropriate
  
  #final output
  out
  
  }