#' Image to Grayscale
#'
#' @description
#' Converts an image to grayscale
#'
#' @param image_path path to an image file
#' @param output path to output file, defaults to input with added _grayscale
#'
#' @returns a grayscale iamge
#' @export
image_to_grayscale <- function(image_path, output = "default"){
  # Check if input exists.
  if (!file.exists(image_path)) {
    cat("Input file does not exist.")
    return(invisible(NULL))
  }
  # If default input, add _grayscale to the input
  if (output == "default") {
    output <- sub("\\.(?=[A-Za-z]{3,4}$)", "_grayscale.", image_path, perl = TRUE)
  }
  # Warn if output exists
  if (file.exists(output)) {
    cat("Output already exists. Do you want to overwrite?\n")
    cat("1) No\n2) Yes\n")
    # Read user input
    choice <- readline(prompt = "Enter 1 or 2: ")
    # Parse answer
    if (choice == "1") {
      cat("Exiting function. File not overwritten.\n")
      return(invisible(NULL))  # exit function
    } else if (choice == "2") {
      cat("Overwriting file...\n")
      # continue with your function
    } else {
      stop("Invalid input. Please enter 1 or 2.")
    }
  }
  # Read the file
  image <- magick::image_read(image_path)
  # We could just use image_modulate from the magick package
  # image_grayscale <- magick::image_modulate(image, saturation = 0)
  # But we want to use our own function
  image_grayscale <- convert_rgb(image, conversion = "grayscale")
  # Save the file
  magick::image_write(image_grayscale, path = output)
}

#' Image to cutoff
#'
#' @param image_path path to an image file
#' @param cutoff a percentage value where to apply cutoff
#' @param output path to output file, defaults to input with added _grayscale
#'
#' @returns image with cutoff applied
#' @export
image_to_cutoff <- function(image_path, output = "default", cutoff = 50) {
  # Check if input exists.
  if (!file.exists(image_path)) {
    cat("Input file does not exist.")
    return(invisible(NULL))
  }
  # If default input, add _grayscale to the input
  if (output == "default") {
    output <- sub("\\.(?=[A-Za-z]{3,4}$)", "_cutoff.", image_path, perl = TRUE)
  }
  # Warn if output exists
  if (file.exists(output)) {
    cat("Output already exists. Do you want to overwrite?\n")
    cat("1) No\n2) Yes\n")
    # Read user input
    choice <- readline(prompt = "Enter 1 or 2: ")
    # Parse answer
    if (choice == "1") {
      cat("Exiting function. File not overwritten.\n")
      return(invisible(NULL))  # exit function
    } else if (choice == "2") {
      cat("Overwriting file...\n")
      # continue with your function
    } else {
      stop("Invalid input. Please enter 1 or 2.")
    }
  }
  # Read the file
  image <- magick::image_read(image_path)
  # Create cutoff image
  image_cutoff <- convert_rgb(image, conversion = "cutoff", percent = cutoff)
  # Save the file
  magick::image_write(image_cutoff, path = output)
}


#' Convert RGB
#'
#' @param image A magick image or a path to an image
#' @param conversion Conversion to apply, grayscale, red, green, blue, or cutoff
#' @param percent Percentage to use for cutoff
#'
#' @returns An image with applied conversions
#' @export
convert_rgb <- function(image, conversion = c("grayscale", "red", "green", "blue", "cutoff"), percent = 50) {
  # Load image if it's a path
  if(typeof(image) == "character") {
    if(file.exists(image)) {
      image <- magick::image_read(image)
    } else if(!file.exists(image)) {
      cat("Input file does not exist.")
      return(invisible(NULL))
    }
  }

  conversion <- match.arg(conversion)
  # Extract bitmap from image
  bitmap <- image[[1]]

  # Convert to integer
  r <- as.integer(bitmap[1,,])
  g <- as.integer(bitmap[2,,])
  b <- as.integer(bitmap[3,,])

  # Compute grayscale
  gray <- 0.299 * r + 0.587 * g + 0.114 * b

  # Apply conversion
  if(conversion == "grayscale") {
    r <- gray
    g <- gray
    b <- gray
  } else if (conversion == "cutoff") {
    cutoff <- percent / 100 * 255
    r <- ifelse(gray < cutoff, 255, 0)
    g <- 0
    b <- ifelse(gray < cutoff, 255, 0)
  } else if (conversion == "red") {
    g <- 0
    b <- 0
  } else if (conversion == "green") {
    r <- 0
    b <- 0
  } else if (conversion == "blue") {
    r <- 0
    g <- 0
  }

  # Assign back to channels
  bitmap[1,,] <- as.raw(round(r))
  bitmap[2,,] <- as.raw(round(g))
  bitmap[3,,] <- as.raw(round(b))
  image <- magick::image_read(bitmap)
  return(image)
}

#' Convert to grayscale
#'
#' @description
#' Legacy, instead use convert_rgb which builds further on this function allowing conversion to grayscale or cutoff
#'
#' @param image a magick image
#'
#' @returns a magick image
convert_grayscale <- function(image) {
  bitmap <- image[[1]]
  # convert to integer (if not already)
  r <- as.integer(bitmap[1,,])
  g <- as.integer(bitmap[2,,])
  b <- as.integer(bitmap[3,,])

  # compute weighted grayscale
  gray <- 0.299 * r + 0.587 * g + 0.114 * b

  # round and convert to raw if needed
  gray <- as.raw(round(gray))

  # assign back to all channels
  bitmap[1,,] <- gray
  bitmap[2,,] <- gray
  bitmap[3,,] <- gray
  image <- magick::image_read(bitmap)

  return(image)
}


#' Modify pixel
#'
#' @description
#' This function is just for demonstration purposes and according to the instructions for the exam.
#' This per pixel iterative approach is O(^2) and slow. A vectorized approach is preferred.
#'
#'
#' @param pixel a raw pixel
#' @param conversion conversion to apply
#' @param percent percentage to use for cutoff
#'
#' @returns A raw pixel with applied conversion
modify_pixel <- function(pixel, conversion = c("grayscale", "red", "green", "blue","cutoff"), percent = 50) {
  modification <- match.arg(modification)
  rgb <- as.integer(pixel)
  r <- rgb[1]
  g <- rgb[2]
  b <- rgb[3]
  switch(modification,
         grayscale = return(as.raw(rep(round(r * 0.299 + g * 0.587 + b * 0.114),3))),
         red = return(as.raw(c(r,0,0))),
         green = return(as.raw(c(0,g,0))),
         blue = return(as.raw(c(0,0,b))),
         cutoff = {
           cut <- percent / 100 * 255
           gray <- round(r * 0.299 + g * 0.587 + b * 0.114)
           if(gray < cut) r <- 255 else r <- 0
           g <- 0
           if(gray < cut) b <- 255 else b <- 0
           return(as.raw(c(r,g,b)))
         }
         )

}

#' Iterative converter
#'
#' @description
#' This function is just for demonstration purposes and according to the instructions for the exam.
#' I highly recommend not to use it.
#' This per pixel iterative approach is O(^2) and slow. A vectorized approach is preferred.
#' For a small image this is 100 times slower than the vectorized approach.
#'
#'
#' @param image an image
#' @param conversion conversion to apply
#' @param percent percentage to use for cutoff
#'
#' @returns An image with applied conversions
iterative_converter <- function(image,conversion = c("grayscale", "red", "green", "blue","cutoff"), percent = 50) {
  modification <- match.arg(modification)
  bitmap <- image[[1]]
  h <- dim(bitmap)[2]
  w <- dim(bitmap)[3]
  for(i in seq_len(h)) {
    for(j in seq_len(w)) {
      bitmap[,i,j] <- modify_pixel(bitmap[,i,j],modification)
    }
  }
  image <- magick::image_read(bitmap)
  return(image)
}
