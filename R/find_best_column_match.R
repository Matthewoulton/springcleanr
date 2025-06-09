#' Find the Best Approximate Column Name Match
#'
#' This function searches for the closest approximate match to a given input string
#' within a vector of candidate column names. It is useful for handling non-standard
#' evaluation errors, such as quosure issues, by performing a case-insensitive and
#' edit-distance-based comparison.
#'
#' @param input_str A character string to be matched against the candidate names.
#' @param candidate_names A character vector of possible column names to match against.
#'
#' @return A single character string representing the best matching candidate name.
#' If multiple equally good matches are found, the function throws an error indicating ambiguity.
#' If no match is found, the function stops with an error message.
#'
#' @details
#' The function first filters candidate names using `agrep()` to find approximate matches.
#' It then computes string edit distances using `adist()` to select the closest match.
#' In the event of a tie for the best match, an error is raised to prevent ambiguous resolution.
#'
#' @examples
#' find_best_column_match("nmae", c("name", "age", "gender"))
#' # Returns "name"
#'
#' find_best_column_match("id", c("name", "age", "gender"))
#' # Error: No close match found for 'id'.
#'
#' @export

tools
# This function looks at an input string and finds the closest match
# This is useful as a way to get around quosure errors, because it is relatively insensitive to quotes.

find_best_column_match <- function(input_str, candidate_names) {
  # Find approximate matches (using agrep explicitly)
  matches <- agrep(input_str, candidate_names, ignore.case = TRUE, value = TRUE)
  if (length(matches) == 0) {
    stop("No close match found for '", input_str, "'.")
  }

  # Compute edit distances between the lower-case input and each candidate
  distances <- adist(base::tolower(input_str), base::tolower(matches))
  best_distance <- min(distances)

  # Get all candidates that share the best (minimum) distance
  best_matches <- matches[distances == best_distance]

  # If there is more than one candidate with the same best distance,
  # consider it ambiguous and throw an error
  if (length(best_matches) > 1) {
    stop("Ambiguous match: input '", input_str,
         "' is equally close to multiple candidate names: ",
         paste(best_matches, collapse = ", "))
  }

  best_match <- best_matches[1]
  return(best_match)
}
