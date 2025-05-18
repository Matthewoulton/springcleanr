### find_best_column_match function

test_that("find_best_column_match returns the best match for a given input (case-insensitive)", {
  candidate_names <- c("ColumnA", "ColumnB", "TestColumn")

  # When the input string is close to "ColumnA" (case doesn't matter)
  best <- find_best_column_match("columna", candidate_names)
  expect_equal(best, "ColumnA")

  # Another example: if input is "Test", then TestColumn should be the best match if no closer one exists
  best2 <- find_best_column_match("test", candidate_names)
  expect_equal(best2, "TestColumn")
})

test_that("find_best_column_match returns a match regardless of input case", {
  candidate_names <- c("columnA", "COLUMNB", "TestColumn")

  # Lowercase input matches the candidate in its original case
  best <- find_best_column_match("columna", candidate_names)
  expect_equal(best, "columnA")

  # Mixed case input should produce the same result as lower case
  best_mixed <- find_best_column_match("CoLuMbA", candidate_names)
  expect_equal(best_mixed, "columnA")
})

test_that("find_best_column_match errors when no close match is found", {
  candidate_names <- c("apple", "banana", "cherry")
  expect_error(
    find_best_column_match("orange", candidate_names),
    regexp = "No close match found for 'orange'"
  )
})

test_that("find_best_column_match returns a single best match", {
  candidate_names <- c("Alpha", "Beta", "Gamma")
  best <- find_best_column_match("alp", candidate_names)
  expect_type(best, "character")
  expect_length(best, 1)
})


### --------------------------------------------------
