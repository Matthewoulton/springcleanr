#### duplicates_function tests

### Duplicates function

test_that("duplicates_function with method='select' returns only duplicate rows", {

  # Create a small data frame with intentional duplicates
  test_data <- dplyr::tibble(
    group = c("A", "A", "B", "B", "C", "D"),
    value = c(1, 1, 2, 3, 4, 5)
  )

  # Call the function with method = "select"
  duplicates <- duplicates_function(test_data, variables = c("group", "value"), method = "select")

  # We expect only rows that appear more than once in ("group", "value")
  # In this case, "A", 1 appears twice
  expected_duplicates <- dplyr::tibble(
    group = c("A", "A"),
    value = c(1, 1)
  )

  # Check that the returned rows match the expected duplicates
  expect_equal(nrow(duplicates), nrow(expected_duplicates))
  expect_equal(duplicates, expected_duplicates)
})

test_that("duplicates_function with method = 'assert' returns original data when no duplicates exist", {
  # Create a small data frame with NO duplicates
  data_no_dups <- dplyr::tibble(
    group = c("A", "B", "C", "D"),
    value = c(1, 2, 3, 4)
  )

  # Expect no error when calling 'assert' on a dataset with no duplicates
  expect_no_error({
    result <- duplicates_function(data_no_dups, variables = c("group", "value"), method = "assert")
    # The function should return the original dataset
    expect_equal(result, data_no_dups)
  })
})

test_that("duplicates_function with method = 'assert' throws an error if duplicates exist", {
  # Create a small data frame WITH duplicates
  data_with_dups <- dplyr::tibble(
    group = c("A", "A", "B"),
    value = c(1, 1, 2)
  )

  # We expect an error when calling 'assert' on a dataset with duplicates
  # By default, testthat checks that ANY error is thrown.
  # We can also validate that the error message contains specific text.
  expect_error(
    duplicates_function(data_with_dups, variables = c("group", "value"), method = "assert"),
    regexp = "Duplicates were found" # or any substring you expect in your error
  )

})

test_that("duplicates_function works with unquoted variable names via tidyselect", {

  test_data <- dplyr::tibble(
    group = c("A", "A", "B", "B", "C"),
    value = c(1, 1, 2, 3, 4)
  )

  result <- duplicates_function(test_data, variables = c(group, value), method = "select")

  expected <- dplyr::tibble(
    group = c("A", "A"),
    value = c(1, 1)
  )

  expect_equal(result, expected)
})


test_that("duplicates_function works with tidyselect helpers like starts_with()", {

  test_data <- dplyr::tibble(
    grp = c("A", "A", "B", "C", "C"),
    val = c(1, 1, 2, 3, 3)
  )

  # Select based on the `grp` column only (using starts_with)
  result <- duplicates_function(test_data, variables = starts_with("g"), method = "select")

  expected <- dplyr::tibble(
    grp = c("A", "A", "C", "C"),
    val = c(1, 1, 3, 3)
  )

  expect_equal(result, expected)
})

test_that("duplicates_function throws an error when selected column doesn't exist", {

  test_data <- dplyr::tibble(
    group = c("A", "B", "C"),
    value = c(1, 2, 3)
  )

  # Using a non-existent column name
  expect_error(
    duplicates_function(test_data, variables = c("nonexistent"), method = "report"),
    regexp = "Column `nonexistent` doesn't exist"
  )


  # Using a tidyselect helper that matches nothing
  expect_error(
    duplicates_function(test_data, variables = starts_with("z"), method = "drop"),
    regexp = "No variables were selected."
  )
})
