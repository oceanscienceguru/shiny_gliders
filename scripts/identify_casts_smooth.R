identify_casts_smooth <- function(data, surface_threshold = 0.1, smoothing_window = 5) {
  # Initialize vectors to store cast information
  casts <- character(nrow(data))
  
  # Find the index where depth data becomes available
  first_depth_index <- which(!is.na(data$osg_i_depth))[1]
  
  # Initialize casts for missing depth rows as "Unknown"
  casts[1:(first_depth_index - 1)] <- "Unknown"
  
  # Initialize the first cast based on the second data point with depth
  if (!is.na(data$osg_i_depth[first_depth_index + 1])) {
    if (data$osg_i_depth[first_depth_index + 1] > data$osg_i_depth[first_depth_index]) {
      casts[first_depth_index] <- "Downcast"
    } else {
      casts[first_depth_index] <- "Upcast"
    }
  }
  
  # Loop through the data to identify casts
  for (i in (first_depth_index + 1):nrow(data)) {
    if (!is.na(data$osg_i_depth[i]) && !is.na(data$osg_i_depth[i - 1])) {
      if (data$osg_i_depth[i] > data$osg_i_depth[i - 1]) {
        casts[i] <- "Downcast"
      } else if (data$osg_i_depth[i] < data$osg_i_depth[i - 1]) {
        casts[i] <- "Upcast"
      } else {
        casts[i] <- "Surface"
      }
    }
  }
  
  # Apply a smoothing filter to the cast column
  casts <- zoo::rollapply(casts, smoothing_window, FUN = function(x) {
    if (all(x == "Downcast")) {
      "Downcast"
    } else if (all(x == "Upcast")) {
      "Upcast"
    } else {
      "Surface"
    }
  }, fill = "NA")
  
  # Assign "Surface" to points with depth near zero or below the surface threshold
  casts[data$osg_i_depth < surface_threshold] <- "Surface"
  
  # Create a new column in the dataframe to store the cast information
  data$cast <- casts
  
  return(data)
}