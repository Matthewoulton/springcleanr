#' Validate Variable Presence, Type, and Variation in a Dataset
#'
#' Checks that specified variables are present in a data frame, match expected types, contain non-missing and varying values,
#' and (optionally) only include values from a predefined list. You can choose whether to `stop()` on failures
#' or issue warnings instead. All checks are run, and a summary of issues is returned.
#'
#' @param data A data frame to validate.
#' @param variables A character vector of column names to check.
#' @param outcome_list Optional. A vector of valid values. If provided, the function checks that all non-NA values
#'   in the variables are included in this list. Only applied if `variable_type` is not `FALSE`.
#' @param hardstop Logical. If `TRUE`, the function collects all failing checks and throws a single error.
#'   If `FALSE` (default), warnings are issued for each problem but the function does not stop execution.
#' @param variable_type Either `"numeric"`, `"string"`, or `FALSE`; or a character vector of these types with one per variable.
#'   If `"numeric"` or `"string"`, the function checks that each column has the corresponding type.
#'   If `FALSE`, skips type checking. If a vector is supplied, it must match the length of `variables`.
#' @param check_constant Logical. If `TRUE` (default), the function checks whether each variable has only a single unique
#'   non-NA value and flags it as constant. Set to `FALSE` to skip this check.
#'
#' @return Invisibly returns `NULL`, but throws an error or warning if any checks fail.
#'
#' @details The function performs the following checks on each variable:
#' - Presence in the dataset.
#' - If `variable_type` is not `FALSE`, the column must match the expected type.
#' - It must contain at least one non-`NA` value.
#' - (Optional) It must not be constant if `check_constant = TRUE`.
#' - If `outcome_list` is provided, all non-`NA` values must be included in that list (only when `variable_type` is not `FALSE`).
#'
#' When `hardstop = TRUE`, any issues are collected and reported together in a single error message.
#' When `hardstop = FALSE`, issues are issued as warnings.
#'
#' @examples
#' df <- data.frame(score = c(1, 2, 2, NA), category = c("A", "A", "A", "A"))
#' column_validation(df, variables = c("score", "category"), variable_type = "numeric")
#' column_validation(df, variables = c("category"), variable_type = "string", outcome_list = c("A", "B"))
#'
#' # Skip constant check:
#' column_validation(df, variables = "category", variable_type = "string", check_constant = FALSE)
#'
#' # Mixed type validation:
#' df2 <- data.frame(ID = 1:3, Name = c("Alice", "Bob", "Charlie"))
#' column_validation(df2, variables = c("ID", "Name"), variable_type = c("numeric", "string"))
#'
#' @export



column_validation <- function(
    data,
    variables,
    outcome_list   = NULL,
    hardstop       = FALSE,
    variable_type  = "numeric",  # Can be single value or vector
    check_constant = TRUE        # New: check constant-valued columns
) {
  if (!is.character(variables)) {
    stop("`variables` must be a character vector of column names.")
  }

  if (length(variable_type) > 1 && length(variable_type) != length(variables)) {
    stop("`variable_type` must be length 1 or the same length as `variables`.")
  }

  issues_found <- character(0)

  message_fun <- function(msg) {
    issues_found <<- c(issues_found, msg)
  }

  is_expected_type <- function(x, type) {
    switch(
      type,
      "numeric"   = is.numeric(x),
      "string"    = is.character(x),
      "character" = is.character(x),
      TRUE
    )
  }

  if (!is.null(outcome_list) && length(variable_type) == 1 && variable_type != FALSE) {
    if (variable_type == "numeric" && !is.numeric(outcome_list)) {
      stop("Provided `outcome_list` is not numeric, but `variable_type = numeric`.")
    }
    if (variable_type == "string" && !is.character(outcome_list)) {
      stop("Provided `outcome_list` is not a character vector, but `variable_type = string`.")
    }
  }

  for (i in seq_along(variables)) {
    var_name <- variables[i]
    this_type <- if (length(variable_type) == 1) variable_type else variable_type[i]

    if (!var_name %in% names(data)) {
      stop(paste0("No column named '", var_name, "' was found in data."))
      next
    }

    column_vals <- data[[var_name]]

    if (this_type != FALSE && !is_expected_type(column_vals, this_type)) {
      message_fun(paste0(
        "Column '", var_name, "' is not of type '", this_type,
        "'. Consider setting `variable_type = FALSE` to skip type checks."
      ))
      next
    }

    non_na_vals <- column_vals[!is.na(column_vals)]

    if (length(non_na_vals) == 0) {
      message_fun(paste0(
        "Column '", var_name, "' has no non-NA values. Skipping variation and membership checks."
      ))
      next
    }

    if (check_constant) {
      unique_vals <- unique(non_na_vals)
      if (length(unique_vals) == 1) {
        message_fun(paste0(
          "Column '", var_name, "' is constant (all values = ", unique_vals[1], ")."
        ))
      }
    }

    if (!is.null(outcome_list)) {
      not_in_list <- non_na_vals[!(non_na_vals %in% outcome_list)]
      if (length(not_in_list) > 0) {
        message_fun(paste0(
          "Column '", var_name, "' has values not in outcome_list: ",
          paste0(unique(not_in_list), collapse = ", ")
        ))
      }
    }
  }

  if (length(issues_found) > 0) {
    full_msg <- paste(issues_found, collapse = "\n")
    if (hardstop) {
      stop(full_msg, call. = FALSE)
    } else {
      warning(full_msg, call. = FALSE)
    }
  } else {
    message("Validation: Success.")
  }

  invisible(NULL)
}


