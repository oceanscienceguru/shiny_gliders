identify_casts_smooth <- function(data, surface_threshold, rolling_window_size) {
  data$cast <- NA
  cast_state <- "Downcast"
  
  # Smooth the depth data with a rolling average
  data$smoothed_depth <- zoo::rollapply(data$osg_i_depth, rolling_window_size, mean, align = "right", fill = NA)
  
  for (i in 1:nrow(data)) {
    if (is.na(data$smoothed_depth[i])) {
      cast_state <- "Unknown"
    } else if (is.na(data$smoothed_depth[i - 1])) {
      cast_state <- "Unknown"
    } else if (data$smoothed_depth[i] <= surface_threshold) {
      cast_state <- "Surface"
    } else if (cast_state == "Surface" && data$smoothed_depth[i] > surface_threshold) {
      cast_state <- "Downcast"
    } else if (data$smoothed_depth[i] > data$smoothed_depth[i - 1]) {
      cast_state <- "Downcast"
    } else if (data$smoothed_depth[i] < data$smoothed_depth[i - 1]) {
      cast_state <- "Upcast"
    }
    
    data$cast[i] <- cast_state
  }
  
  # Remove the temporary smoothed_depth column
  data <- data[, -ncol(data)]
  
  return(data)
}
