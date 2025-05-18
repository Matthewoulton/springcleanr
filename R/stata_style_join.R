#' Stata-style Join Function
#'
#' This function mimics Stata’s merging functionality while retaining R’s capabilities
#' and adding additional features. It supports a range of join types (including 1:1, 1:m, m:1),
#' provides labelled merge indicators, optional coalescing column updates, and summary output.
#'
#' @param first_df A data frame or tibble. The primary dataset to be merged.
#' @param second_df A data frame or tibble. The secondary dataset to be merged.
#' @param join_type A character string specifying the type of join to perform.
#'   Options: `"left"`, `"right"`, `"inner"`, `"full"`, `"1:1"`, `"1:m"`, `"m:1"`, `"m:m"`.
#' @param by A character vector of column names to join by. If `NULL`, common variables between the datasets are used.
#' @param merge_var A string or `FALSE`. If not `FALSE`, a labelled merge indicator column is added to the result.
#'   If `TRUE`, defaults to `"merge_var"`.
#' @param drop_summaries Logical. If `TRUE`, prints a summary of matched and unmatched row counts and dataset sizes.
#' @param column_update Logical or character vector. If `TRUE`, overlapping columns (excluding `by`) are updated by coalescing `.x` and `.y` columns.
#'
#'#' @param suffix A character vector of length 2 specifying suffixes for overlapping column names
#'   from `first_df` and `second_df`. Defaults to `c(".x", ".y")`. Passed to `*_join()` methods.
#' @return A data frame with the merged result. May include:
#' - A labelled merge indicator column (if `merge_var` is set).
#' - Coalesced columns (if `column_update` is used).
#'
#' @export
#' Stata-style Join Function
#'
#' This function mimics Stata’s merging functionality while retaining R’s capabilities
#' and adding additional features. It supports a range of join types (including 1:1, 1:m, m:1),
#' provides labelled merge indicators, optional coalescing column updates, and summary output.
#'
#' @param first_df A data frame or tibble. The primary dataset to be merged.
#' @param second_df A data frame or tibble. The secondary dataset to be merged.
#' @param join_type A character string specifying the type of join to perform.
#'   Options: `"left"`, `"right"`, `"inner"`, `"full"`, `"1:1"`, `"1:m"`, `"m:1"`, `"m:m"`.
#' @param by A character vector of column names to join by. If `NULL`, common variables between the datasets are used.
#' @param merge_var A string or `FALSE`. If not `FALSE`, a labelled merge indicator column is added to the result.
#'   If `TRUE`, defaults to `"merge_var"`.
#' @param drop_summaries Logical. If `TRUE`, prints a summary of matched and unmatched row counts and dataset sizes.
#' @param column_update Logical or character vector. If `TRUE`, overlapping columns (excluding `by`) are updated by coalescing suffix columns.
#' @param suffix A character vector of length 2 specifying suffixes for overlapping column names
#'   from `first_df` and `second_df`. Defaults to `c(".x", ".y")`.
#'
#' @return A data frame with the merged result. May include:
#' - A labelled merge indicator column (if `merge_var` is set).
#' - Coalesced columns (if `column_update` is used).
#'
#' @export
#'
stata_style_join <- function(first_df,
                             second_df,
                             join_type = "left",
                             by = NULL,
                             merge_var = "merge_var",
                             drop_summaries = TRUE,
                             column_update = FALSE,
                             suffix = c(".x", ".y")) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  if (!requireNamespace("labelled", quietly = TRUE)) {
    stop("Package 'labelled' is required but not installed.")
  }

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

  if (is.null(by)) {
    by <- intersect(names(first_df), names(second_df))
    if (length(by) == 0) {
      stop("No common variables found between the datasets to join on.")
    }
  }

  if (isTRUE(merge_var)) {
    merge_var <- "merge_var"
  } else if (!is.character(merge_var) && !identical(merge_var, FALSE)) {
    stop("merge_var must be a single string, TRUE, or FALSE.")
  }

  check_uniqueness <- function(data, keys) {
    any(duplicated(data[keys]))
  }

  if (join_type == "1:1" && (check_uniqueness(first_df, by) || check_uniqueness(second_df, by))) {
    stop("Error: The `by` variable(s) must be unique in both datasets for a 1:1 join.")
  }

  if (join_type == "1:m" && check_uniqueness(first_df, by)) {
    stop("Error: The `by` variable(s) must be unique in the first dataset for a 1:m join.")
  }

  if (join_type == "m:1" && check_uniqueness(second_df, by)) {
    stop("Error: The `by` variable(s) must be unique in the second dataset for an m:1 join.")
  }

  if (join_type == "m:m" && check_uniqueness(first_df, by) && check_uniqueness(second_df, by)) {
    warning("Detected a many-to-many relationship. Ensure this is intended.")
  }

  first_df <- dplyr::mutate(first_df, first_df_marker = 1)
  second_df <- dplyr::mutate(second_df, second_df_marker = 1)

  joined_data <- join_function(first_df, second_df, by = by, suffix = suffix)

  if (!identical(column_update, FALSE)) {
    common_vars <- setdiff(intersect(names(first_df), names(second_df)), by)

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

    for (var in update_vars) {
      x_var <- paste0(var, suffix[1])
      y_var <- paste0(var, suffix[2])

      if (x_var %in% names(joined_data) && y_var %in% names(joined_data)) {
        joined_data <- joined_data %>%
          dplyr::mutate(
            !!var := dplyr::coalesce(.data[[x_var]], .data[[y_var]])
          ) %>%
          dplyr::select(-!!x_var, -!!y_var)
      }
    }
  }

  joined_data <- joined_data %>%
    dplyr::mutate(
      .merge_var = dplyr::case_when(
        !is.na(.data$first_df_marker) & !is.na(.data$second_df_marker) ~ 1,
        !is.na(.data$first_df_marker) &  is.na(.data$second_df_marker) ~ 2,
        is.na(.data$first_df_marker) & !is.na(.data$second_df_marker) ~ 3
      )
    ) %>%
    dplyr::mutate(
      .merge_var = labelled::labelled(
        .data[[".merge_var"]],
        labels = c("matched" = 1, "first_only" = 2, "second_only" = 3)
      )
    ) %>%
    dplyr::select(-first_df_marker, -second_df_marker)

  if (!identical(drop_summaries, FALSE)) {
    unmatched_first <- base::sum(joined_data[[".merge_var"]] == 2)
    unmatched_second <- base::sum(joined_data[[".merge_var"]] == 3)

    match_summary <- dplyr::tibble(
      category = c("matched", "first_only", "second_only"),
      count = c(
        base::sum(joined_data[[".merge_var"]] == 1),
        unmatched_first,
        unmatched_second
      )
    )

    totals_summary <- dplyr::tibble(
      category = c("total_first_df", "total_second_df", "total_joined_data"),
      count = c(
        base::nrow(first_df),
        base::nrow(second_df),
        base::nrow(joined_data)
      )
    )

    base::print(match_summary)
    base::print(totals_summary)
  }

  if (is.character(merge_var)) {
    joined_data <- dplyr::rename(joined_data, !!merge_var := .merge_var)
  } else if (identical(merge_var, FALSE)) {
    joined_data <- dplyr::select(joined_data, - .merge_var)
  }

  joined_data
}
