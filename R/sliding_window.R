utils::globalVariables(c(
  "combined",    # used in make_plot
  "filtered",    # used in make_plot
  "time"         # used in make_plot
))

#' Combine acceleration vector
#'
#' @description
#' Helper function to import .csv, convert time to POSIXct and create a combined vector.
#'
#' @param sourcefile path to .csv file with time and three dimensions (x, y, and z)
#' @returns dataframe with added combined vector
combine_to_accvector <- function(sourcefile) {
  # Read source file
  df <- utils::read.csv(sourcefile)
  # Convert time to POSIXct
  df[,"time"] <- as.POSIXct(df[,"time"])
  # Make combined
  df[,"combined"] <- sqrt(
    df[,"x"] ^ 2 +
    df[,"y"] ^ 2 +
    df[,"z"] ^ 2
  )
  return(df)
}

#' Create plot
#'
#' @description
#' Reads a source .csv containing at least time, x, y and z.
#' Makes a combined acceleration vector using the eucledian method.
#' Applies a sliding window filter to the plot using either mean or binary method
#'
#'
#' @param sourcefile a .csv file with at least time, and three dimensions x, y, and z
#' @param type type of window function to apply, mean or binary (default = mean)
#' @param windowsize size of window (default = 1000)
#' @param threshold threshold to apply (default = 0.5)
#'
#' @returns a plot
#' @export
make_plot <- function(sourcefile, type = c("threshold", "binary"), windowsize = 1000, threshold = 0.5) {
  # Import and combine data
  df <- combine_to_accvector(sourcefile)
  df_filtered <- apply_window_function(df, fun = match.arg(type), w = windowsize, t = threshold)
  # Create plot
  ggplot2::ggplot(df_filtered, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = combined)) +
    ggplot2::geom_line(ggplot2::aes(y = filtered), color = "red", alpha = .7) +
    ggplot2::labs(
      x = "time",
      y = "eucledian"
    ) +
    ggplot2::theme_classic()
}

#' Window threshold
#'
#' @param vec a vector from a window
#' @param t threshold value
#'
#' @returns new value if change exceed threshold
window_threshold <- function(vec, t) {
  # Set start of window
  s <- vec[1]
  # Find minimum in window
  mi <- min(vec)
  # Find maximum in window
  ma <- max(vec)

  # Set change to min or max if the exceed threshold
  if(ma - s > s - mi & ma - s > t) {
    return(ma - s)
  } else if(s - mi > t) {
    return(s - mi)
  } else {
    return(s)
  }
}

#' Window binary
#'
#' @param vec a vector from a window
#'
#' @returns +1 or -1 depending of mean change in window
window_binary <- function(vec) {
  s <- vec[1]
  e <- vec[length(vec)]
  if (e > s) return(+1) else return(-1)
}

#' Apply window function
#'
#' @description
#' Applies a window function to the supplied dataframe. Available window functions are threshold and binary.
#'
#'
#' @param df a dataframe
#' @param col the column of the dataframe to apply function to
#' @param fun the filter function to apply to the window
#' @param w the window size
#' @param t the threshold (only used of fun = "threshold")
#'
#' @returns a dataframe with added filtered column
#' @export
apply_window_function <- function(df, col = "combined", fun = c("threshold","binary"), w = 1000, t = .5) {
  vec <- df[,col]
  n <- length(vec)
  res <- numeric(n)
  switch(fun,
    threshold = for(i in seq_len(n)) {
      e <- min(i + w - 1, n)
      res[i] <- window_threshold(vec[i:e],t)
    },
    binary =
      for(i in seq_len(n)) {
        e <- min(i + w - 1, n)
        res[i] <- window_binary(vec[i:e])
      }
  )
  df[,"filtered"] <- res
  return(df)
}
