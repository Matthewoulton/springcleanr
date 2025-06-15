#' Stata-style Join Function
#'
#' This function mimics Stata’s merging functionality while retaining R’s flexibility
#' and adding features like labelled merge indicators, join diagnostics, and column coalescing.
#' It separates the *type of SQL join* from the *relationship between the datasets*, offering explicit
#' checks for one-to-one, one-to-many, and many-to-many merges.
#'
#' @param first_df A data frame or tibble. The primary dataset to be merged.
#' @param second_df A data frame or tibble. The secondary dataset to be merged.
#' @param by A character vector of column names to join by. If `NULL`, common variable names are used.
#' @param join_type A character string specifying the type of join to perform.
#'   Options: `"left"`, `"inner"`, `"full"`. Passed to the appropriate `dplyr` join function.
#' @param relationship A character string specifying the expected observation-level relationship
#'   between `first_df` and `second_df`. This is a **required** argument.
#'   Options: `"1:1"`, `"1:m"`, `"m:1"`, `"m:m"`. Triggers validation checks for uniqueness.
#' @param merge_var A character string or `NULL`. If `NULL`, no merge indicator is added.
#' #' @param assert Integer vector or NULL. If not NULL, asserts that all observations fall into the
#'   specified merge categories:
#'   `1 = matched`, `2 = first only`, `3 = second only`.
#'   For example, `assert = 1` checks that all rows matched. Default is `NULL`.
#' @param drop_summaries Logical. If `FALSE`, prints a summary of matched and unmatched row counts and dataset sizes.
#' @param column_update Logical or character vector. If `TRUE`, overlapping columns (excluding `by`) are updated by coalescing suffix variants.
#'   If a character vector, only those specific variables will be updated.
#' @param suffix A character vector of length 2 specifying suffixes for overlapping column names
#'   from `first_df` and `second_df`. Defaults to `c(".x", ".y")`. Passed to `*_join()` methods.
#'
#' @return A data frame with the merged result. May include:
#' - A labelled merge indicator column (if `merge_var` is set).
#' - Coalesced columns (if `column_update` is used).
#' - Optional printed summaries of match status and row counts.
#'
#' @examples
#' stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id")
#'
#' @export
stata_style_join <- function(first_df,
                             second_df,
                             relationship, # this argument is required.
                             join_type = "left",
                             by = NULL,
                             merge_var = "merge_var",
                             assert = NULL,
                             drop_summaries = FALSE,
                             column_update = FALSE,
                             suffix = c(".x", ".y")) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  if (!requireNamespace("labelled", quietly = TRUE)) {
    stop("Package 'labelled' is required but not installed.")
  }

  if (any(is.na(first_df[by])) || any(is.na(second_df[by]))) {
    warning("Missing values detected in `by` columns. Join results may be incomplete.")
  }


    # Canonicalize the relationship input
    relationship_map <- c(
      "1:1" = "1:1",
      "one-to-one" = "1:1",
      "1:m" = "1:m",
      "one-to-many" = "1:m",
      "m:1" = "m:1",
      "many-to-one" = "m:1",
      "m:m" = "m:m",
      "many-to-many" = "m:m"
    )

    if (missing(relationship)) {
      stop("The `relationship` argument is required and must be one of: ",
           paste(names(relationship_map), collapse = ", "))
    }

    if (!relationship %in% names(relationship_map)) {
      stop("Invalid `relationship` value. Must be one of: ",
           paste(names(relationship_map), collapse = ", "))
    }

    normalised_relationship <- relationship_map[[relationship]]

  # Define the join function based on the simplified join_type
  join_function <- switch(
    join_type,
    "left"  = dplyr::left_join,
    "inner" = dplyr::inner_join,
    "full"  = dplyr::full_join,
    stop("Invalid join_type specified. Must be one of 'left', 'inner', 'full'.")
  )

  if (is.null(by)) {
    by <- intersect(names(first_df), names(second_df))
    if (length(by) == 0) stop("No common variables found between the datasets to join on.")
  }

  if (!is.null(merge_var) && !is.character(merge_var)) {
    stop("merge_var must be either NULL or a character string.")
  }

  check_uniqueness <- function(data, keys) {
    any(duplicated(data[keys]))
  }

  # Check relationships
  switch(
    normalised_relationship,
    "1:1" = {
      if (check_uniqueness(first_df, by) || check_uniqueness(second_df, by)) {
        stop("For a 1:1 join, the `by` keys must be unique in both datasets.")
      }
    },
    "1:m" = {
      if (check_uniqueness(first_df, by)) {
        stop("For a 1:m join, `by` keys must be unique in the first dataset.")
      }
    },
    "m:1" = {
      if (check_uniqueness(second_df, by)) {
        stop("For a m:1 join, `by` keys must be unique in the second dataset.")
      }
    },
    "m:m" = {
      if (check_uniqueness(first_df, by) && check_uniqueness(second_df, by)) {
        warning("Many-to-many join detected. Ensure this is intended.")
      }
    },
    stop("Invalid relationship specified. Must be one of '1:1', '1:m', 'm:1', 'm:m'.")
  )

  first_df <- dplyr::mutate(first_df, first_df_marker = 1)
  second_df <- dplyr::mutate(second_df, second_df_marker = 1)

  if (normalised_relationship == "m:m") {
    joined_data <- join_function(
      first_df,
      second_df,
      by = by,
      suffix = suffix,
      relationship = "many-to-many"
    )
  } else {
    joined_data <- join_function(
      first_df,
      second_df,
      by = by,
      suffix = suffix
    )
  }


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

  if (!is.null(assert)) {
    if (!all(assert %in% c(1, 2, 3))) {
      stop("`assert` must be NULL or a vector containing only 1 (first only), 2 (second only), and/or 3 (matched).")
    }

    present <- unique(as.numeric(joined_data$.merge_var))
    unexpected <- setdiff(present, assert)

    if (length(unexpected) > 0) {
      label_map <- c("1" = "matched", "2" = "first only", "3" = "second only")
      stop(
        "Assertion failed: unexpected merge categories found. ",
        "Expected only: ", paste(assert, collapse = ", "), ". ",
        "Found also: ", paste(unexpected, collapse = ", "), "."
      )
    }
  }

  if (!identical(drop_summaries, TRUE)) {
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

  if (!is.null(merge_var)) {
    joined_data <- dplyr::rename(joined_data, !!merge_var := .merge_var)
  } else {
    joined_data <- dplyr::select(joined_data, - .merge_var)
  }

  joined_data
}
