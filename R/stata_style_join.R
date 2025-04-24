#' Stata-style Join Function
#'
#' This function mimics Stata’s merging functionality while retaining R’s capabilities
#' and adding additional features. It allows for different types of joins, labeled merge indicators,
#' column updates, and summary outputs.
#'
#' @param first_data A dataframe or tibble. The primary dataset to be merged.
#' @param second_data A dataframe or tibble. The secondary dataset to be merged.
#' @param join_type A character string specifying the type of join.
#'   Options: `"left"`, `"right"`, `"inner"`, `"full"`, `"1:1"`, `"1:m"`, `"m:1"`, `"m:m"`.
#'   `"1:1"` ensures uniqueness in both datasets, `"1:m"` enforces uniqueness in `first_data`,
#'   `"m:1"` enforces uniqueness in `second_data`, and `"m:m"` allows many-to-many matches.
#' @param by A character vector specifying the variable(s) to join by.
#'   If `NULL`, common variables between both datasets will be used.
#' @param merge_var A string or `FALSE`. If specified, adds a column indicating merge status:
#'   `1 = matched`, `2 = first only`, `3 = second only`. If `TRUE`, defaults to `"merge_var"`.
#'   Set to `FALSE` to disable.
#' @param drop_summaries Logical. If `TRUE`, prints a summary of the number of matched and unmatched observations.
#' @param column_update Logical or character vector. If `TRUE`, updates all common columns (excluding `by` variables)
#'   by replacing `.x` columns with `.y` values if `.x` is `NA`. If a character vector, only the specified variables
#'   are updated.
#'
#' @return A dataframe with the merged results, optionally including `merge_var` and updated columns.
#'
#' @details
#' - **Join Types**: Supports Stata-style joins (`1:1`, `1:m`, `m:1`, `m:m`) and standard joins (`left`, `right`, `inner`, `full`).
#' - **Column Updates**: Allows updating overlapping variables instead of keeping `.x` and `.y` suffixes.
#' - **Merge Indicator**: Adds a labeled variable indicating match status.
#' - **Summaries**: Prints counts of matched/unmatched observations if `drop_summaries = TRUE`.
#'
#' @examples
#' # Example datasets
#' first_data <- dplyr::tibble(
#'   id = c(1, 2, 3),
#'   value = c(10, NA, 30),
#'   category = c("A", "B", "C")
#' )
#'
#' second_data <- dplyr::tibble(
#'   id = c(2, 3, 4),
#'   value = c(20, 40, 50),
#'   category = c("X", "Y", "Z")
#' )
#'
#' # Left join with automatic column updates
#' stata_style_join(first_data, second_data, join_type = "left", by = "id", column_update = TRUE)
#'
#' # Inner join with merge variable
#' stata_style_join(first_data, second_data, join_type = "inner", by = "id", merge_var = TRUE)
#'
#' # Many-to-many join with a warning
#' stata_style_join(first_data, second_data, join_type = "m:m", by = "id")
#'
#' @export


stata_style_join <- function(first_data,
                             second_data,
                             join_type = "left",
                             by = NULL,
                             merge_var = "merge_var",
                             drop_summaries = FALSE,
                             column_update = FALSE) {
  # Determine join function based on join_type
  join_function <- base::switch(
    join_type,
    "left"  = dplyr::left_join,
    "right" = dplyr::right_join,
    "inner" = dplyr::inner_join,
    "full"  = dplyr::full_join,
    "1:1"   = dplyr::inner_join,
    "1:m"   = dplyr::left_join,
    "m:1"   = dplyr::right_join,
    "m:m"   = dplyr::full_join,
    stop("Invalid join_type specified.")
  )

  # Error handling for `by` variables
  if (is.null(by)) {
    by <- intersect(names(first_data), names(second_data))
    if (length(by) == 0) {
      stop("No common variables found between the datasets to join on.")
    }
  }

  if (merge_var == TRUE) {
    merge_var <- "merge_var"
  } else if (merge_var == FALSE) {
    merge_var <- FALSE
  } else if (!is.character(merge_var) || length(merge_var) != 1) {
    stop("merge_var must be a single string or FALSE.")
  }

  # Function to check for duplicate keys
  check_uniqueness <- function(data, keys) {
    any(duplicated(data[keys]))
  }

  # Checks for specific join types
  if (join_type == "1:1" && (check_uniqueness(first_data, by) || check_uniqueness(second_data, by))) {
    stop("Error: The `by` variable(s) must be unique in both datasets for a 1:1 join.")
  }

  if (join_type == "1:m" && check_uniqueness(first_data, by)) {
    stop("Error: The `by` variable(s) must be unique in the first dataset for a 1:m join.")
  }

  if (join_type == "m:1" && check_uniqueness(second_data, by)) {
    stop("Error: The `by` variable(s) must be unique in the second dataset for an m:1 join.")
  }

  # Many-to-Many Join Warning
  if (join_type == "m:m" && check_uniqueness(first_data, by) && check_uniqueness(second_data, by)) {
    warning("Detected a many-to-many relationship. Ensure this is intended.")
  }

  # Annotate datasets with markers for merge_var
  first_data <- first_data %>% dplyr::mutate(first_data_marker = 1)
  second_data <- second_data %>% dplyr::mutate(second_data_marker = 1)

  # Perform the join
  joined_data <- join_function(first_data, second_data, by = by)

  # Handle column updates
  if (!identical(column_update, FALSE)) {
    common_vars <- setdiff(intersect(names(first_data), names(second_data)), by)

    if (isTRUE(column_update)) {
      update_vars <- common_vars
    } else if (is.character(column_update)) {
      update_vars <- intersect(column_update, common_vars)
      if (length(update_vars) != length(column_update)) {
        stop("Some specified column_update variables are not in both datasets.")
      }
    } else {
      stop("column_update must be TRUE, FALSE, or a character vector of variable names.")
    }

    # Create updated columns
    for (var in update_vars) {
      x_var <- paste0(var, ".x")
      y_var <- paste0(var, ".y")

      if (x_var %in% names(joined_data) && y_var %in% names(joined_data)) {
        joined_data <- joined_data %>%
          dplyr::mutate(
            !!var := dplyr::coalesce(.data[[x_var]], .data[[y_var]])
          ) %>%
          dplyr::select(-!!x_var, -!!y_var)  # Drop old columns
      }
    }
  }

  # Handle merge_var logic
  if (!identical(merge_var, FALSE)) {
    joined_data <- joined_data %>%
      dplyr::mutate(
        !!merge_var := dplyr::case_when(
          !is.na(.data$first_data_marker) & !is.na(.data$second_data_marker) ~ 1,  # matched
          !is.na(.data$first_data_marker) & is.na(.data$second_data_marker) ~ 2,   # first_only
          is.na(.data$first_data_marker) & !is.na(.data$second_data_marker) ~ 3    # second_only
        )
      ) %>%
      dplyr::mutate(
        !!merge_var := labelled::labelled(.data[[merge_var]], labels = c("matched" = 1, "first_only" = 2, "second_only" = 3))
      )
  }

  # Drop temporary markers
  joined_data <- joined_data %>% dplyr::select(-first_data_marker, -second_data_marker)

  # Optionally drop summaries
  if (!identical(drop_summaries, FALSE)) {
    unmatched_first <- base::sum(joined_data[[merge_var]] == 2)
    unmatched_second <- base::sum(joined_data[[merge_var]] == 3)

    # Compute summaries
    match_summary <- dplyr::tibble(
      category = c("matched", "first_only", "second_only"),
      count = c(
        base::sum(joined_data[[merge_var]] == 1),
        unmatched_first,
        unmatched_second
      )
    )

    totals_summary <- dplyr::tibble(
      category = c("total_first_data", "total_second_data", "total_joined_data"),
      count = c(
        base::nrow(first_data),
        base::nrow(second_data),
        base::nrow(joined_data)
      )
    )

    # Print summaries
    base::print(match_summary)
    base::print(totals_summary)
  }

  joined_data
}
