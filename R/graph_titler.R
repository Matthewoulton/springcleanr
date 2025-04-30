#' Create a Title-Case Label from a Variable Name for Graphs
#'
#' This function converts a variable name (given as a symbol or string) into a title-cased, human-readable label
#' suitable for use in graph titles or axis labels. It handles common transformations such as replacing underscores,
#' converting to title case, preserving acronyms, formatting numeric ranges, and handling special words.
#'@importFrom rlang enquo as_label
#'@importFrom stringr str_replace_all str_to_title str_replace regex
#'
#' @param var A variable name or character string. Can be passed unquoted (as a symbol) or quoted (as a string).
#'
#' @return A character string with a clean, title-cased label suitable for use in `ggplot2::labs()` or other plot annotations.
#'
#' @details The function performs the following transformations:
#' - Replaces underscores (`_`) with spaces.
#' - Converts to title case (e.g., "employment_rate" → "Employment Rate").
#' - Preserves capitalization for common acronyms (e.g., "GDP", "UK", "UN").
#' - Converts numeric ranges (e.g., "16_24" → "16–24").
#' - Keeps words like "vs." and "per" in lowercase.
#' - Properly formats ordinal numbers (e.g., "1st", "2nd").
#'
#' @examples
#' graph_titler(daily_award_level)
#' graph_titler("employment_rate_16_24")
#' graph_titler("cost_per_unit")
#' graph_titler("fbi_crime_statistics")
#'
#' @export



graph_titler <- function(var) {

  # Convert unquoted variable to string if necessary
  var_name <- rlang::as_label(rlang::enquo(var))

  # Replace underscores with spaces
  var_name <- stringr::str_replace_all(var_name, "_", " ")

  # Convert to title case
  var_name <- stringr::str_to_title(var_name)

  # Ensure common acronyms remain uppercase (only whole words)
  acronyms <- c("GDP", "UK", "EU", "US", "OECD", "UN", "IMF", "FBI", "AI")
  for (acronym in acronyms) {
    var_name <- stringr::str_replace_all(
      var_name, stringr::regex(paste0("\\b", stringr::str_to_title(acronym), "\\b")), acronym
    )
  }

  # Ensure "vs." and "per" remain lowercase
  var_name <- stringr::str_replace_all(var_name, "\\bVs\\b", "vs.")
  var_name <- stringr::str_replace_all(var_name, "\\bPer\\b", "per")

  # Handle ordinal numbers correctly
  var_name <- stringr::str_replace_all(var_name, "\\b([0-9]+)(St|Nd|Rd|Th)\\b", "\\1\\L\\2")

  # Handle numeric ranges with underscores (e.g., 16_24 -> 16–24)
  var_name <- stringr::str_replace_all(var_name, "(\\d+) (\\d+)", "\\1–\\2")

  return(var_name)
}
