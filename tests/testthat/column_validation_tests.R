### tests for column_validation

### column_validation function

test_that("column_validation handles normal numeric usage correctly", {
  df <- data.frame(
    treatment = c(0, 1, 0, 1),
    tmt_post  = c(1, 1, 0, 0)
  )

  expect_message(
    column_validation(
      data          = df,
      variables     = c("treatment", "tmt_post"),
      outcome_list  = c(0, 1),
      hardstop      = FALSE,
      variable_type = "numeric"
    ),
    regexp = "Validation: Success\\."
  )
})

test_that("column_validation throws error if column is missing", {
  df <- data.frame(
    treatment = c(0, 0, 1)
  )

  expect_error(
    column_validation(
      data         = df,
      variables    = c("treatment", "tmt_post"),
      outcome_list = c(0, 1)
    ),
    regexp = "No column named 'tmt_post'"
  )
})

test_that("column_validation throws error if column is not of expected type (numeric)", {
  df <- data.frame(
    treatment = c("treated", "control", "treated")
  )

  expect_error(
    column_validation(
      data          = df,
      variables     = "treatment",
      variable_type = "numeric",
      hardstop = TRUE
    ),
    regexp = "Column 'treatment' is not of type 'numeric'"
  )
})

test_that("column_validation warns if column is not of expected type (numeric) with hardstop = FALSE", {
  df <- data.frame(
    treatment = c("treated", "control", "treated")
  )

  expect_warning(
    column_validation(
      data          = df,
      variables     = "treatment",
      variable_type = "numeric",
      hardstop      = FALSE
    ),
    regexp = "Column 'treatment' is not of type 'numeric'"
  )
})


test_that("column_validation throws error if column is not of expected type (string)", {
  df <- data.frame(
    status = factor(c("A", "B", "B", "A"))
  )

  expect_error(
    column_validation(
      data          = df,
      variables     = "status",
      variable_type = "string",
      hardstop = TRUE
    ),
    regexp = "Column 'status' is not of type 'string'"
  )
})

test_that("column_validation warns when column is constant with hardstop = FALSE", {
  df <- data.frame(
    treatment = c(1, 1, 1, 1)
  )

  expect_warning(
    column_validation(
      data         = df,
      variables    = "treatment",
      outcome_list = c(0, 1),
      hardstop     = FALSE,
      variable_type = "numeric"
    ),
    regexp = "is constant \\(all values = 1\\)"
  )
})

test_that("column_validation errors when column is constant with hardstop = TRUE", {
  df <- data.frame(
    treatment = c(0, 0, 0, 0)
  )

  expect_error(
    column_validation(
      data      = df,
      variables = "treatment",
      hardstop  = TRUE,
      variable_type = "numeric"
    ),
    regexp = "is constant \\(all values = 0\\)"
  )
})

test_that("column_validation warns if values are not in outcome_list (hardstop = FALSE)", {
  df <- data.frame(
    treatment = c(0, 2, 1)
  )

  expect_warning(
    column_validation(
      data          = df,
      variables     = "treatment",
      outcome_list  = c(0, 1),
      variable_type = "numeric",
      hardstop      = FALSE
    ),
    regexp = "has values not in outcome_list: 2"
  )
})

test_that("column_validation errors if values are not in outcome_list (hardstop = TRUE)", {
  df <- data.frame(
    treatment = c(0, 2, 1)
  )

  expect_error(
    column_validation(
      data          = df,
      variables     = "treatment",
      outcome_list  = c(0, 1),
      variable_type = "numeric",
      hardstop      = TRUE
    ),
    regexp = "has values not in outcome_list: 2"
  )
})

test_that("column_validation warns if a column has only NAs", {
  df <- data.frame(
    treatment = c(NaN, NaN, NaN)
  )

  expect_warning(
    column_validation(
      data      = df,
      variables = "treatment"
    ),
    regexp = "has no non-NA values"
  )
})

test_that("column_validation allows skipping type checks (variable_type = FALSE)", {
  df <- data.frame(
    mixed_col = factor(c("A", "B", "C")),
    x         = c(0, 1, 0)
  )

  expect_message(
    column_validation(
      data          = df,
      variables     = c("mixed_col", "x"),
      variable_type = FALSE
    ),
    regexp = "Validation: Success\\."
  )
})

test_that("column_validation checks outcome_list type if variable_type = 'string'", {
  df <- data.frame(status = c("A", "B", "C"))

  expect_error(
    column_validation(
      data          = df,
      variables     = "status",
      outcome_list  = c(1, 2),
      variable_type = "string"
    ),
    regexp = "Provided `outcome_list` is not a character vector, but `variable_type = string`\\."
  )
})

test_that("column_validation checks outcome_list type if variable_type = 'numeric'", {
  df <- data.frame(treatment = c(1, 0, 1))

  expect_error(
    column_validation(
      data          = df,
      variables     = "treatment",
      outcome_list  = c("A", "B"),
      variable_type = "numeric"
    ),
    regexp = "Provided `outcome_list` is not numeric, but `variable_type = numeric`\\."
  )
})

test_that("column_validation handles mixed variable types with vector input", {
  df <- data.frame(
    ID     = c(1, 2, 3),
    Height = c(180, 175, 170),
    Name   = c("Alice", "Bob", "Charlie"),
    Age    = c(30, 25, 35),
    City   = c("London", "Paris", "Berlin")
  )

  expect_message(
    column_validation(
      data          = df,
      variables     = c("ID", "Height", "Name", "Age", "City"),
      variable_type = c("numeric", "numeric", "string", "numeric", "string")
    ),
    regexp = "Validation: Success\\."
  )
})

test_that("column_validation errors when variable_type length mismatches variables", {
  df <- data.frame(
    A = c(1, 2), B = c(3, 4), C = c("x", "y")
  )

  expect_error(
    column_validation(
      data          = df,
      variables     = c("A", "B", "C"),
      variable_type = c("numeric", "string")  # too short
    ),
    regexp = "`variable_type` must be length 1 or the same length as `variables`"
  )
})

test_that("column_validation errors when one variable is wrong type with vector input", {
  df <- data.frame(
    A = c(1, 2),
    B = c("x", "y")
  )

  expect_error(
    column_validation(
      data          = df,
      variables     = c("A", "B"),
      variable_type = c("numeric", "numeric"),  # B is not numeric
      hardstop = TRUE),

    regexp = "Column 'B' is not of type 'numeric'"
  )
})

test_that("column_validation skips all type checks when variable_type is FALSE", {
  df <- data.frame(
    X = c(1, 2, 3),
    Y = c("a", "b", "c")
  )

  expect_message(
    column_validation(
      data          = df,
      variables     = c("X", "Y"),
      variable_type = FALSE
    ),
    regexp = "Validation: Success\\."
  )
})


test_that("column_validation reports NA counts correctly when NA_count = TRUE", {
  df <- data.frame(
    var1 = c(1, NA, 3, NA, 5),
    var2 = c("a", "b", NA, "d", "e"),
    var3 = c(NA, NA, NA, NA, NA)
  )

  # Case 1: NA_count = TRUE should issue a warning for NA counts
  expect_warning(
    column_validation(
      data = df,
      variables = c("var1", "var2", "var3"),
      variable_type = FALSE,
      hardstop = FALSE,
      check_constant = FALSE,
      NA_count = TRUE
    ),
    regexp = "NA value.*\\("
  )

  # Case 2: NA_count = FALSE should produce the success message only
  expect_message(
    column_validation(
      data = df,
      variables = c("var1", "var2"),
      variable_type = FALSE,
      hardstop = FALSE,
      check_constant = FALSE,
      NA_count = FALSE
    ),
    regexp = "Validation: Success"
  )
})

