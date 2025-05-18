
#' Identify and Handle Duplicate Rows
#'
#' This function detects and optionally handles duplicated rows in a dataset,
#' based on one or more specified variables. You can generate a summary report,
#' drop duplicates, select duplicates, or assert that none exist.
#'
#' @param data A dataframe or tibble to check for duplicates.
#' @param variables A character vector of variable names to check for duplicates.
#'   If `NULL`, all variables in the dataset are used.
#' @param method A string specifying the method to use. Options are:
#'   \itemize{
#'     \item `"report"`: (default) Returns a summary count of how many rows occur at each duplication level.
#'     \item `"drop"`: Returns the dataset with duplicates removed (keeping the first occurrence) and prints a summary.
#'     \item `"select"`: Returns only the duplicated rows (those that appear more than once).
#'     \item `"assert"`: Raises an error if duplicates are found, otherwise returns the original data.
#'   }
#'
#' @return A dataframe or tibble, depending on the method:
#' \describe{
#'   \item{`"report"`}{A summary tibble showing duplication levels and how many rows occur at each level.}
#'   \item{`"drop"`}{The deduplicated dataset, with a printed summary.}
#'   \item{`"select"`}{A tibble of only the rows that are duplicated.}
#'   \item{`"assert"`}{The original dataset if no duplicates are found; otherwise, an error is thrown.}
#' }
#'
#' @examples
#' df <- dplyr::tibble(
#'   id = c(1, 1, 2, 3, 3, 3),
#'   value = c("a", "a", "b", "c", "c", "c")
#' )
#'
#' # Report duplicate levels
#' duplicates_function(df, method = "report")
#'
#' # Drop duplicates
#' deduped <- duplicates_function(df, method = "drop")
#'
#' # Select only duplicates
#' dupes_only <- duplicates_function(df, method = "select")
#'
#' # Assert uniqueness
#' # duplicates_function(df, method = "assert")  # Will raise an error if duplicates are found
#'
#' @export

duplicates_function <- function(data, variables = dplyr::everything(), method = "report") {

    if (!requireNamespace("dplyr", quietly = TRUE) ||
        !requireNamespace("tidyselect", quietly = TRUE) ||
        !requireNamespace("rlang", quietly = TRUE)) {
      stop("Packages 'dplyr', 'tidyselect', and 'rlang' are required but not all are installed.")
    }

    var_expr <- rlang::enquo(variables)
    variables <- tidyselect::eval_select(var_expr, data)

    if (length(variables) == 0) {
      stop("No variables were selected.")
    }

    var_names <- names(variables)

  # Group by the specified variables and calculate counts
  grouped_data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(variables))) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  if (method == "report") {

    # REPORT METHOD:
    # Show how many rows appear at each 'count' (level of duplication).
    result <- grouped_data %>%
      dplyr::count(count, name = "num_occurrences") %>%
      dplyr::rename(Observations = count)

    return(result)

  } else if (method == "drop") {

    # DROP METHOD:
    # Return a deduplicated dataset (drop duplicates).
    deduped_data <- data %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(variables)), .keep_all = TRUE)

    report <- grouped_data %>%
      dplyr::count(count, name = "num_occurrences") %>%
      dplyr::rename(Copies = count)

    print(report)

    return(deduped_data)

  } else if (method == "select") {

    # SELECT METHOD:
    # Return only the rows that are duplicates (appear > 1 time).
    duplicates_only <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(variables))) %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::ungroup()

    return(duplicates_only)

  } else if (method == "assert") {

    # ASSERT METHOD:
    # Error if duplicates exist; otherwise, return the original data.

    # Check for duplicates
    if (any(grouped_data$count > 1)) {
      # If duplicates exist, create the same summary as in 'report'
      duplication_summary <- grouped_data %>%
        dplyr::count(count, name = "Observations") %>%
        dplyr::rename(Copies = count)

      # Convert the summary to text for the error message
      summary_text <- paste(capture.output(print(duplication_summary)), collapse = "\n")

      stop(
        "Duplicates were found in the data. Here is the summary:\n",
        summary_text,
        call. = FALSE
      )

    } else {
      # No duplicates, so just return the original data
      return(data)
    }

  } else {
    stop("Invalid method. Use 'report', 'drop', 'select', or 'assert'.")
  }
}
