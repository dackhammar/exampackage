<!-- badges: start -->
[![R-CMD-check](https://github.com/dackhammar/exampackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dackhammar/exampackage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## exampackage

`exampackage` was developed for the group examination of the Intermediate R course. 

This package contains three groups of functions:

1. **Image filter** - reads and converts images to grayscale, colors or cutoff based
2. **Sliding window** - applies filter functions to and plots accelerometer data
3. **Semantic search** - reads a text and determines if it's positive, neutral or negative

## Installation
**exampackage** can be installed from github using the **remotes** package.

```r
# Using remotes
install.packages("remotes")
remotes::install_github("dackhammar/exampackage")
```

**exampackage** depends on **magick** and **ggplot2**.

## Usage

### Image filter

The image filter functions reads images and outputs them with different filters applied.

There are three functions in the image filter function group:

* `image_to_grayscale()`
* `image_to_cutoff()`
* `convert_rgb()`

`image_to_grayscale` takes an input image path and outputs a grayscale version of the same image. If no output path is supplied `_grayscale` is appended to the input file.

``` r
image_to_grayscale("image.jpg") # Outputs image_grayscale.jpg
image_to_grayscale("image.jpg", "grayscale/image.jpg") # Outputs grayscale/image.jpg
```

`image_to_cutoff` takes an input image and applies a cutoff filter. Cutoff defaults to 50% but custom values can be used. If no output path is supplied `_cutoff` is appended to the input file.

``` r
image_to_cutoff("image.jpg") # Outputs image_cutoff.jpg with cutoff at 50%
image_to_cutoff("image.jpg", "cutoff/image.jpg", cutoff = 70) # Outputs cutoff/image.jpg with cutoff at 70%
```

`convert_rgb` is a helper function that takes a magick image or a filepath and outputs a magick image.

### Sliding window

The sliding window functions works with accelerometer data. It creates an acceleration vector using the eucladian method and applies a threshold or binary filter.

There are two functions in the sliding window group:

* `make_plot()`
* `apply_window_function()`

`make_plot()` reads data in .csv format, creates an acceleration vector, applies a sliding window filter and prints a plot.

```r
make_plot("data.csv",type = "threshold",windowsize = 500,threshold = 0.2)
````

`apply_window_function` is a helper function that takes a dataframe and applies a sliding window filter and adds a filtered column to the dataframe.

### Semantic search

The semantic search function works with text and detects the mood.

There is one function in the sematic search group:

* `text_mood()`

`text_mood()` reads a textfile and prints the mood of the text to the console.

```r
text_mood("textfile.txt")
```
