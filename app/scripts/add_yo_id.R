add_yo_id <- function(df) {
  df$yo_id <- cumsum(df$cast == "Downcast" & c(FALSE, df$cast[-length(df$cast)] == "Upcast")) + 1
  return(df)
}