reportPlot <- function(mission, inGliderdf, plotVar, liveData = FALSE, colorMin = NULL, colorMax = NULL, logoFile = NULL){

  scf <- inGliderdf %>%
    select(m_present_time, osg_i_depth, all_of(plotVar)) %>%
    filter(!is.na(.data[[plotVar]]) & .data[[plotVar]] != 0)
  
  gcf <- inGliderdf %>%
    select(m_present_time, m_water_depth) %>%
    filter(!is.na(m_water_depth)) %>%
    filter(m_water_depth != 0)
  
  baseSci <- ggplot(data =
                      scf,
                    aes(x=m_present_time,
                        y=round(osg_i_depth, 2))) +
    geom_point(aes(color = .data[[plotVar]]),
               size = 2)
  
  fullSci <- baseSci +
    scale_y_reverse() +
    geom_point(data = gcf,
               aes(x = m_present_time,
                   y = m_water_depth),
               color = "black",
               size = 0.3,
               na.rm = TRUE
    ) +
    theme_bw() +
    labs(title = paste0(mission, " ", plotVar),
         y = "Depth (m)",
         x = "Date") +
    #scale_color_cmocean(name = "thermal") +
    
    if (plotVar == "sci_water_temp") {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "thermal")
    } else if (plotVar == "sci_water_pressure") {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "deep")
    } else if (plotVar == "sci_water_cond") {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "haline")
    } else if (plotVar == "sci_suna_nitrate_concentration") {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "tempo")
    } else if (plotVar == "sci_flbbcd_chlor_units" |
               plotVar == "sci_bbfl2s_chlor_scaled" ) {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "algae")
    } else if (plotVar == "sci_flbbcd_cdom_units" |
               plotVar == "sci_bbfl2s_cdom_scaled" ) {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "matter")
    } else if (plotVar == "sci_flbbcd_bb_units" |
               plotVar == "sci_bbfl2s_bb_scaled" ) {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "turbid")
    } else if (plotVar == "sci_oxy3835_oxygen" |
               plotVar == "sci_oxy4_oxygen" ) {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "oxy")
    } else if (startsWith(plotVar, "sci_ocr")) {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "solar")
    } else if (plotVar == "osg_soundvel1") {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "speed")
    } else if (plotVar == "osg_rho") {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "dense")
    } else if (plotVar == "osg_salinity") {
      scale_color_cmocean(#limits = c(input$min, input$max),
                          name = "haline")
    } else {
      scale_color_viridis_c(#limits = c(input$min, input$max))
      )
    }
}