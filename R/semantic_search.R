#' Text mood
#'
#' @description
#' Detects the mood of the text
#'
#'
#' @param text Path to a text file
#'
#' @returns Prints summary. Returns a list of ratio, positive and negative word counts.
#'
#' @examples
#' # Load test file
#' text_file <- system.file("extdata", "testtext.txt", package = "exampackage")
#' # Run function
#' text_mood(text_file)
#'
#' @export
text_mood <- function(text) {
  # Read text to t, make lowercase and separate by regex on non-word characters
  words <- suppressWarnings(readLines(text)) |>
    tolower() |> # converts to lowercase
    strsplit("[^a-z']+") |> # splits on anything that is not a-z or '
    unlist() # strsplit returns a list and we want vector

  n_pos <- count_matches(words, pos_patterns)
  n_neg <- count_matches(words, neg_patterns)

  if(n_pos > n_neg) {
    cat(paste0("Text is positive :)\nIt contains ",
               n_pos,
               " positive words and ",
               n_neg,
               " negative words."))
    return (
      list(
        "ratio" = n_pos/n_neg,
        "positive" = n_pos,
        "negative" = n_neg
        )
    )
  } else if (n_pos == n_neg) {
    cat(paste0("Text is neutral :|\nIt contains ",
               n_pos,
               " positive words and ",
               n_neg,
               " negative words."))
    return (
      list(
        "ratio" = n_pos/n_neg,
        "positive" = n_pos,
        "negative" = n_neg
      )
    )
  } else if (n_pos < n_neg) {
    cat(paste0("Text is negative :(\nIt contains ",
               n_pos,
               " positive words and ",
               n_neg,
               " negative words."))
    return (
      list(
        "ratio" = n_pos/n_neg,
        "positive" = n_pos,
        "negative" = n_neg
      )
    )
  }
}

#' Negative word patterns
#' @keywords internal
neg_patterns <- c(
  "fatigue.*", "tired.*", "sleep.*", "exhaust.*", "weary.*",
  "nausea.*", "sick.*", "ill.*", "unwell.*",
  "frustrat.*", "annoy.*", "stress.*", "overwhelm.*",
  "pain.*", "ache.*", "heavy.*", "burden.*", "struggle.*", "trouble.*",
  "hopeless.*", "sad.*", "depress.*", "miserable.*",
  "weak.*", "slow.*", "unmotiv.*", "trapped.*"
)

#' Positive word patterns
#' @keywords internal
pos_patterns <- c(
  "good.*", "well.*", "better.*", "recover.*", "healthy.*",
  "hope.*", "relax.*", "rest.*", "sleep.*", "energ.*",
  "happy.*", "smile.*", "joy.*", "love.*", "grateful.*",
  "strong.*", "motiv.*", "cheer.*", "calm.*", "peace.*"
)

#' Count matches in words using pattern list
#' @param words character vector of words
#' @param patterns character vector of regex patterns
#' @return integer count
count_matches <- function(words, patterns) {
  sum(sapply(patterns, \(p) any(grepl(p,words))))
}
