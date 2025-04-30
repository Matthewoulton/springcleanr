#' Validate Variable Presence, Type, and Variation in a Dataset
#'
#' Checks that specified variables are present in a data frame, have variation, match expected types,
#' and (optionally) only include values from a predefined list. You can choose whether to `stop()` on failures
#' or issue warnings instead.
#'
#' @param data A data frame to validate.
#' @param variables A character vector of column names to check.
#' @param outcome_list Optional. A vector of valid values. If provided, the function checks that all values
#'   in the variables are included in this list (after removing `NA`s).
#' @param hardstop Logical. If `TRUE`, the function stops on failure. If `FALSE` (default), it issues a warning.
#' @param variable_type Either `"numeric"`, `"string"`, or `FALSE`. If `"numeric"` or `"string"`, the function checks
#'   that each column has the corresponding type. If `FALSE`, skips type checking.
#'
#' @return Invisibly returns `NULL`, but throws an error or warning if any checks fail.
#'
#' @details The function performs the following checks on each variable:
#' - It is present in the dataset.
#' - If `variable_type` is not `FALSE`, the variable must match the specified type.
#' - It contains non-`NA` values.
#' - It has variation (i.e., not all values are the same).
#' - If `outcome_list` is provided, all non-`NA` values must be found in this list.
#'
#' If `hardstop = TRUE`, any failure results in an error. Otherwise, failures issue warnings and the function continues.
#'
#' @examples
#' df <- data.frame(score = c(1, 2, 2, NA), category = c("A", "A", "A", "A"))
#' validate_presence_and_variation(df, variables = c("score", "category"), variable_type = "numeric")
#' validate_presence_and_variation(df, variables = c("category"), variable_type = "string", outcome_list = c("A", "B"))
#'
#' @export


validate_presence_and_variation <- function(
    data,
    variables,
    outcome_list   = NULL,
    hardstop           = FALSE,
    variable_type  = "numeric"  # "numeric", "string", or FALSE
) {
  # Ensure 'variables' is a character vector
  if (!is.character(variables)) {
    stop("variables must be a character vector of column names.")
  }

  # Helper to conditionally throw error or warning
  message_fun <- function(msg) {
    if (hardstop) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  # Helper: checks if a value x matches the required type
  is_expected_type <- function(x, type) {
    switch(
      type,
      "numeric" = is.numeric(x),
      "string"  = is.character(x),
      # If type is something else (including FALSE), skip the check
      # because we only support numeric/string or skipping
      TRUE
    )
  }

  # 1) If variable_type != FALSE and outcome_list is not NULL,
  #    check that outcome_list matches variable_type
  if (!is.null(outcome_list) && variable_type != FALSE) {

    # If we're expecting numeric, check outcome_list is numeric
    if (variable_type == "numeric" && !is.numeric(outcome_list)) {
      stop(
        "Provided outcome_list is not numeric, but variable_type = numeric. ",
        "Check the types or set variable_type = FALSE to skip type checks."
      )
    }

    # If we're expecting string, check outcome_list is character
    if (variable_type == "string" && !is.character(outcome_list)) {
      stop(
        "Provided outcome_list is not a character vector, but variable_type = string. ",
        "Check the types or set variable_type = FALSE to skip type checks."
      )
    }
  }

  for (var_name in variables) {

    # 2) Check presence
    if (!var_name %in% names(data)) {
      stop("No column named '", var_name, "' was found in data.")
    }

    # 3) Check type of the column (only if variable_type != FALSE)
    if (variable_type != FALSE) {
      column_vals <- data[[var_name]]

      if (!is_expected_type(column_vals, variable_type)) {
        stop(
          "Column '", var_name, "' is not of type '", variable_type,
          "'. Consider setting variable_type = FALSE if you want to skip type checks."
        )
      }
    }

    # 4) Filter out NA
    non_na_vals <- data[[var_name]][!is.na(data[[var_name]])]

    if (length(non_na_vals) == 0) {
      warning(
        "Column '", var_name, "' has no non-NA values. ",
        "Skipping variation and membership checks.",
        call. = FALSE
      )
      next
    }

    # 5) Check if constant
    unique_vals <- unique(non_na_vals)
    if (length(unique_vals) == 1) {
      msg <- paste0(
        "Column '", var_name,
        "' is constant (all values = ", unique_vals[1], ")."
      )
      message_fun(msg)
    }

    # 6) If 'outcome_list' is specified, check membership
    if (!is.null(outcome_list)) {
      not_in_list <- non_na_vals[!(non_na_vals %in% outcome_list)]
      if (length(not_in_list) > 0) {
        msg <- paste0(
          "Column '", var_name, "' has values not in outcome_list: ",
          paste0(unique(not_in_list), collapse = ", ")
        )
        message_fun(msg)
      }
    }
  }

  invisible(NULL)
}
