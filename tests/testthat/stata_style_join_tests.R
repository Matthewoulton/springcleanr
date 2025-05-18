##### Tests for stata_style_join

library(testthat)

## Tests for stata_style_join

# Sample datasets for testing
first_df <- dplyr::tibble(id = c(1, 2, 3), value = c("A", "B", "C"))
second_df <- dplyr::tibble(id = c(2, 3, 4), value = c("X", "Y", "Z"))

# Test 1: Basic left join
test_that("Left join works correctly", {
  result <- stata_style_join(first_df, second_df, join_type = "left", by = "id")
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$merge_var == 1), 2)  # matched
  expect_equal(sum(result$merge_var == 2), 1)  # first_only
})

test_that("Left join works correctly", {
  result <- stata_style_join(first_df, second_df, join_type = "left", by = c("id" = "id"))
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$merge_var == 1), 2)  # matched
  expect_equal(sum(result$merge_var == 2), 1)  # first_only
})

# Test 2: Left join with merge_var = FALSE
test_that("Left join works correctly with merge_var = FALSE", {
  result <- stata_style_join(first_df, second_df, join_type = "left", by = "id", merge_var = FALSE)
  expect_equal(nrow(result), 3)
  expect_false("merge_var" %in% colnames(result))
})

# Test 3: 1:1 join with unique keys in both datasets
test_that("1:1 join works with unique keys", {
  unique_first <- dplyr::tibble(id = c(1, 2, 3), value = c("A", "B", "C"))
  unique_second <- dplyr::tibble(id = c(1, 2, 3), value = c("X", "Y", "Z"))
  result <- stata_style_join(unique_first, unique_second, join_type = "1:1", by = "id")
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$merge_var == 1), 3)  # matched
})

# Test 4: 1:1 join with duplicate keys should throw an error
test_that("1:1 join throws error with duplicate keys", {
  duplicate_first <- dplyr::tibble(id = c(1, 1, 2), value = c("A", "B", "C"))
  expect_error(stata_style_join(second_df, duplicate_first, join_type = "1:1", by = "id"))
})

# Test 5: 1:m join with unique keys in the first dataset
test_that("1:m join works with unique keys in first dataset", {
  first_df_1m <- dplyr::tibble(id = c(1, 2, 3), value = c("A", "B", "C"))
  second_df_1m <- dplyr::tibble(id = c(2, 2, 3, 4), value = c("X", "Y", "Z", "W"))
  result <- stata_style_join(first_df_1m, second_df_1m, join_type = "1:m", by = "id")
  expect_equal(nrow(result), 4)
  expect_equal(sum(result$merge_var == 1), 3)  # matched
})

# Test 6: m:1 join with unique keys in the second dataset
test_that("m:1 join works with unique keys in second dataset", {
  first_df_m1 <- dplyr::tibble(id = c(2, 2, 3, 4), value = c("A", "B", "C", "D"))
  second_df_m1 <- dplyr::tibble(id = c(2, 3, 4), value = c("X", "Y", "Z"))
  result <- stata_style_join(first_df_m1, second_df_m1, join_type = "m:1", by = "id")
  expect_equal(nrow(result), 4)
  expect_equal(sum(result$merge_var == 1), 4)  # matched
})

# Test 7: m:m join with non-unique keys

test_that("stata_style_join handles m:m join with non-unique keys and triggers a warning", {
  first_df <- dplyr::tibble(
    id = c(1, 2, 2, 3),
    value_first = c("A", "B", "C", "D")
  )

  second_df <- dplyr::tibble(
    id = c(2, 2, 4),
    value_second = c("X", "Y", "Z")
  )

  # Expect a warning when performing an m:m join
  expect_warning(
    {
      joined_data <- stata_style_join(
        first_df = first_df,
        second_df = second_df,
        join_type = "m:m",
        by = "id",
        merge_var = TRUE
      )
    },
    regexp = "Detected a many-to-many relationship. Ensure this is intended.",
    fixed = TRUE
  )

  expect_true("merge_var" %in% colnames(joined_data))
  expect_true("value_first" %in% colnames(joined_data))
  expect_true("value_second" %in% colnames(joined_data))

  expect_equal(nrow(joined_data), 7)
})

# Test 8 : merge_var parameter

test_that("stata_style_join correctly handles merge_var parameter", {
  # Sample datasets
  df1 <- dplyr::tibble(
    id = c(1, 2, 3),
    value1 = c("A", "B", "C")
  )

  df2 <- dplyr::tibble(
    id = c(2, 3, 4),
    value2 = c("X", "Y", "Z")
  )

  # Test 1: Default merge_var (merge_var = "merge_var")
  result <- stata_style_join(df1, df2, join_type = "left", by = "id")
  expect_true("merge_var" %in% colnames(result))
  expect_equal(as.numeric(result$merge_var), c(2, 1, 1)) # Convert haven_labelled to numeric
  expect_equal(attr(result$merge_var, "labels"), c("matched" = 1, "first_only" = 2, "second_only" = 3)) # Check labels

  # Test 2: Custom merge_var name
  result <- stata_style_join(df1, df2, join_type = "left", by = "id", merge_var = "custom_merge")
  expect_true("custom_merge" %in% colnames(result))
  expect_equal(as.numeric(result$custom_merge), c(2, 1, 1)) # Convert haven_labelled to numeric
  expect_equal(attr(result$custom_merge, "labels"), c("matched" = 1, "first_only" = 2, "second_only" = 3)) # Check labels
  expect_false("merge_var" %in% colnames(result)) # Original "merge_var" should not exist

  # Test 3: merge_var = FALSE
  result <- stata_style_join(df1, df2, join_type = "left", by = "id", merge_var = FALSE)
  expect_false("merge_var" %in% colnames(result))
  expect_false("custom_merge" %in% colnames(result)) # No merge_var should exist
})

# Test 9: column_update parameter

test_that("stata_style_join handles column_update correctly", {
  # Sample datasets
  df1 <- dplyr::tibble(
    id = c(1, 2, 3),
    col1 = c("A", "B", "C"),
    shared_col = c(NA, "keep1", "keep2")
  )

  df2 <- dplyr::tibble(
    id = c(2, 3, 4),
    col2 = c("X", "Y", "Z"),
    shared_col = c("update1", NA, "update2")
  )

  # Test 1: column_update = TRUE (all overlapping columns)
  result <- stata_style_join(df1, df2, join_type = "full", by = "id", column_update = TRUE)
  expect_equal(result$shared_col, c(NA, "keep1", "keep2", "update2"))
  expect_false("shared_col.x" %in% colnames(result))
  expect_false("shared_col.y" %in% colnames(result))

  # Test 2: column_update = "shared_col" (specific column)
  result <- stata_style_join(df1, df2, join_type = "full", by = "id", column_update = "shared_col")
  expect_equal(result$shared_col, c(NA, "keep1", "keep2", "update2"))
  expect_false("shared_col.x" %in% colnames(result))
  expect_false("shared_col.y" %in% colnames(result))

  # Test 3: column_update = FALSE (default R behavior)
  result <- stata_style_join(df1, df2, join_type = "full", by = "id", column_update = FALSE)
  expect_true("shared_col.x" %in% colnames(result))
  expect_true("shared_col.y" %in% colnames(result))
  expect_equal(result$shared_col.x, c(NA, "keep1", "keep2", NA))
  expect_equal(result$shared_col.y, c(NA, "update1", NA, "update2"))

  # Test 4: column_update = character vector with a typo
  expect_error(
    stata_style_join(df1, df2, join_type = "full", by = "id", column_update = "wrong_col"),
    "Some specified column_update variables are not in both datasets."
  )

})

# Test 10: column updates

test_that("stata_style_join correctly updates columns when column_update is TRUE or a subset", {
  # Create example datasets
  first_df <- dplyr::tibble(
    id = c(1, 2, 3),
    value = c(10, NA, 30),
    category = c("A", "B", "C")
  )

  second_df <- dplyr::tibble(
    id = c(2, 3, 4),
    value = c(20, 40, 50),
    category = c("X", "Y", "Z")
  )

  # Test with column_update = TRUE (all common variables updated)
  joined_data <- stata_style_join(
    first_df = first_df,
    second_df = second_df,
    join_type = "left",
    by = "id",
    column_update = TRUE
  )

  # Check that column names exist and are updated correctly
  expect_true("value" %in% colnames(joined_data))
  expect_true("category" %in% colnames(joined_data))
  expect_false("value.x" %in% colnames(joined_data))
  expect_false("value.y" %in% colnames(joined_data))

  # Verify value column is updated (x unless NA, then y)
  expect_equal(joined_data$value, c(10, 20, 30))

  # Test with column_update = c("value") (only update specific variables)
  joined_data_subset <- stata_style_join(
    first_df = first_df,
    second_df = second_df,
    join_type = "left",
    by = "id",
    column_update = c("value")
  )

  # Check that value is updated, but category remains with .x and .y suffixes
  expect_true("value" %in% colnames(joined_data_subset))
  expect_false("value.x" %in% colnames(joined_data_subset))
  expect_false("value.y" %in% colnames(joined_data_subset))
  expect_true("category.x" %in% colnames(joined_data_subset))
  expect_true("category.y" %in% colnames(joined_data_subset))

  # Verify the updated value column
  expect_equal(joined_data_subset$value, c(10, 20, 30))
})

# Test 11: column updates

test_that("stata_style_join respects custom suffix and updates columns correctly", {
  df1 <- data.frame(
    id = c(1, 2, 3),
    value = c(10, NA, 30)
  )

  df2 <- data.frame(
    id = c(2, 3, 4),
    value = c(20, 40, 50)
  )

  result <- stata_style_join(
    df1, df2,
    join_type = "left",
    by = "id",
    column_update = TRUE,
    suffix = c("_from_first", "_from_second"),
    merge_var = FALSE
  )

  # Ensure the output has the 'value' column coalesced
  expect_true("value" %in% names(result))

  # Ensure neither suffix column remains
  expect_false("value_from_first" %in% names(result))
  expect_false("value_from_second" %in% names(result))

  # Ensure correct coalescing behavior
  expected_values <- c(10, 20, 30)  # NA in df1[2] replaced by df2[2]
  expect_equal(result$value, expected_values)
})

test_that("stata_style_join applies suffixes when common column names exist", {
  df1 <- data.frame(
    id = c(1, 2, 3),
    score = c(10, 20, 30)
  )

  df2 <- data.frame(
    id = c(2, 3, 4),
    score = c(200, 300, 400)
  )

  result <- stata_style_join(
    df1,
    df2,
    join_type = "left",
    by = "id",
    column_update = FALSE,
    suffix = c("_first", "_second"),
    merge_var = FALSE
  )

  # Should have both suffixed versions of "score"
  expect_true("score_first" %in% names(result))
  expect_true("score_second" %in% names(result))

  # Should not have an unsuffixed "score" column
  expect_false("score" %in% names(result))

  # Row count should match left join of df1
  expect_equal(nrow(result), nrow(df1))

  # Check suffix values for first row (id = 1): only in df1
  expect_equal(result$score_first[1], 10)
  expect_true(is.na(result$score_second[1]))

  # Check suffix values for second row (id = 2): matched
  expect_equal(result$score_first[2], 20)
  expect_equal(result$score_second[2], 200)
})
