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
