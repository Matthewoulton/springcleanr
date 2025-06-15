library(testthat)

# Sample datasets for testing
first_df <- dplyr::tibble(id = c(1, 2, 3), value = c("A", "B", "C"))
second_df <- dplyr::tibble(id = c(2, 3, 4), value = c("X", "Y", "Z"))

# Test 1: Basic left join
test_that("Left join works correctly", {
  result <- stata_style_join(first_df, second_df, join_type = "left", relationship = "m:1", by = "id")
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$merge_var == 1), 2)
  expect_equal(sum(result$merge_var == 2), 1)
})

test_that("Left join works correctly with named by", {
  result <- stata_style_join(first_df, second_df, join_type = "left", relationship = "m:1", by = c("id" = "id"))
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$merge_var == 1), 2)
  expect_equal(sum(result$merge_var == 2), 1)
})

# Test 2: Left join with merge_var = NULL
test_that("Left join works correctly with merge_var = NULL", {
  result <- stata_style_join(first_df, second_df, join_type = "left", relationship = "m:1", by = "id", merge_var = NULL)
  expect_equal(nrow(result), 3)
  expect_false("merge_var" %in% colnames(result))
})

# Test 3: 1:1 join with unique keys in both datasets
test_that("1:1 relationship works with unique keys", {
  unique_first <- dplyr::tibble(id = c(1, 2, 3), value = c("A", "B", "C"))
  unique_second <- dplyr::tibble(id = c(1, 2, 3), value = c("X", "Y", "Z"))
  result <- stata_style_join(unique_first, unique_second, join_type = "inner", relationship = "1:1", by = "id")
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$merge_var == 1), 3)
})

# Test 4: 1:1 join with duplicate keys should throw an error
test_that("1:1 relationship throws error with duplicate keys", {
  duplicate_first <- dplyr::tibble(id = c(1, 1, 2), value = c("A", "B", "C"))
  expect_error(
    stata_style_join(second_df, duplicate_first, join_type = "inner", relationship = "1:1", by = "id")
  )
})

# Test 5: 1:m join with unique keys in the first dataset
test_that("1:m relationship works with unique keys in first dataset", {
  first_df_1m <- dplyr::tibble(id = c(1, 2, 3), value = c("A", "B", "C"))
  second_df_1m <- dplyr::tibble(id = c(2, 2, 3, 4), value = c("X", "Y", "Z", "W"))
  result <- stata_style_join(first_df_1m, second_df_1m, join_type = "left", relationship = "1:m", by = "id")
  expect_equal(nrow(result), 4)
  expect_equal(sum(result$merge_var == 1), 3)
})

# Test 6: m:1 join with unique keys in the second dataset
test_that("m:1 relationship works with unique keys in second dataset", {
  first_df_m1 <- dplyr::tibble(id = c(2, 2, 3, 4), value = c("A", "B", "C", "D"))
  second_df_m1 <- dplyr::tibble(id = c(2, 3, 4), value = c("X", "Y", "Z"))
  result <- stata_style_join(first_df_m1, second_df_m1, join_type = "left", relationship = "m:1", by = "id")
  expect_equal(nrow(result), 4)
  expect_equal(sum(result$merge_var == 1), 4)
})

# Test 7: m:m join with non-unique keys
test_that("stata_style_join handles m:m join with non-unique keys and triggers a warning", {
  first_df <- dplyr::tibble(id = c(1, 2, 2, 3), value_first = c("A", "B", "C", "D"))
  second_df <- dplyr::tibble(id = c(2, 2, 4), value_second = c("X", "Y", "Z"))

  expect_warning(
    {
      joined_data <- stata_style_join(
        first_df = first_df,
        second_df = second_df,
        join_type = "full",
        relationship = "m:m",
        by = "id"
      )
    },
    regexp = "Many-to-many join detected. Ensure this is intended.",
    fixed = TRUE
  )

  expect_true("merge_var" %in% colnames(joined_data))
  expect_equal(nrow(joined_data), 7)
})

# Test 8: merge_var parameter
test_that("stata_style_join correctly handles merge_var parameter", {
  df1 <- dplyr::tibble(id = c(1, 2, 3), value1 = c("A", "B", "C"))
  df2 <- dplyr::tibble(id = c(2, 3, 4), value2 = c("X", "Y", "Z"))

  result <- stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id")
  expect_true("merge_var" %in% colnames(result))
  expect_equal(as.numeric(result$merge_var), c(2, 1, 1))
  expect_equal(attr(result$merge_var, "labels"), c("matched" = 1, "first_only" = 2, "second_only" = 3))

  result <- stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id", merge_var = "custom_merge")
  expect_true("custom_merge" %in% colnames(result))
  expect_equal(as.numeric(result$custom_merge), c(2, 1, 1))
  expect_equal(attr(result$custom_merge, "labels"), c("matched" = 1, "first_only" = 2, "second_only" = 3))
  expect_false("merge_var" %in% colnames(result))

  result <- stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id", merge_var = NULL)
  expect_false("merge_var" %in% colnames(result))
  expect_false("custom_merge" %in% colnames(result))
})

# Test 9: column_update parameter
test_that("stata_style_join handles column_update correctly", {
  df1 <- dplyr::tibble(id = c(1, 2, 3), col1 = c("A", "B", "C"), shared_col = c(NA, "keep1", "keep2"))
  df2 <- dplyr::tibble(id = c(2, 3, 4), col2 = c("X", "Y", "Z"), shared_col = c("update1", NA, "update2"))

  result <- stata_style_join(df1, df2, join_type = "full", relationship = "m:m", by = "id", column_update = TRUE)
  expect_equal(result$shared_col, c(NA, "keep1", "keep2", "update2"))

  result <- stata_style_join(df1, df2, join_type = "full", relationship = "m:m", by = "id", column_update = "shared_col")
  expect_equal(result$shared_col, c(NA, "keep1", "keep2", "update2"))

  result <- stata_style_join(df1, df2, join_type = "full", relationship = "m:m", by = "id", column_update = FALSE)
  expect_true("shared_col.x" %in% colnames(result))
  expect_true("shared_col.y" %in% colnames(result))

  expect_error(
    stata_style_join(df1, df2, join_type = "full", relationship = "m:m", by = "id", column_update = "wrong_col"),
    "Some specified column_update variables are not in both datasets."
  )
})

# Test 10: selective column updates
test_that("stata_style_join correctly updates columns when column_update is TRUE or a subset", {
  first_df <- dplyr::tibble(id = c(1, 2, 3), value = c(10, NA, 30), category = c("A", "B", "C"))
  second_df <- dplyr::tibble(id = c(2, 3, 4), value = c(20, 40, 50), category = c("X", "Y", "Z"))

  joined_data <- stata_style_join(first_df, second_df, join_type = "left", relationship = "m:1", by = "id", column_update = TRUE)
  expect_equal(joined_data$value, c(10, 20, 30))

  joined_data_subset <- stata_style_join(first_df, second_df, join_type = "left", relationship = "m:1", by = "id", column_update = c("value"))
  expect_equal(joined_data_subset$value, c(10, 20, 30))
  expect_true("category.x" %in% colnames(joined_data_subset))
  expect_true("category.y" %in% colnames(joined_data_subset))
})

# Test 11: suffix behavior
test_that("stata_style_join respects custom suffix and updates columns correctly", {
  df1 <- data.frame(id = c(1, 2, 3), value = c(10, NA, 30))
  df2 <- data.frame(id = c(2, 3, 4), value = c(20, 40, 50))

  result <- stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id", column_update = TRUE,
                             suffix = c("_from_first", "_from_second"), merge_var = NULL)

  expect_true("value" %in% names(result))
  expect_false("value_from_first" %in% names(result))
  expect_false("value_from_second" %in% names(result))
  expect_equal(result$value, c(10, 20, 30))
})

test_that("stata_style_join applies suffixes when common column names exist", {
  df1 <- data.frame(id = c(1, 2, 3), score = c(10, 20, 30))
  df2 <- data.frame(id = c(2, 3, 4), score = c(200, 300, 400))

  result <- stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id",
                             column_update = FALSE, suffix = c("_first", "_second"),
                             merge_var = NULL)

  expect_true("score_first" %in% names(result))
  expect_true("score_second" %in% names(result))
  expect_false("score" %in% names(result))
  expect_equal(nrow(result), nrow(df1))
  expect_equal(result$score_first[1], 10)
  expect_true(is.na(result$score_second[1]))
  expect_equal(result$score_first[2], 20)
  expect_equal(result$score_second[2], 200)
})

test_that("stata_style_join assert parameter enforces allowed merge statuses", {
  df1 <- dplyr::tibble(id = c(1, 2, 3))
  df2 <- dplyr::tibble(id = c(2, 3, 4))

  # Case 1: assert = 1 (matched only) — should fail (id = 1 unmatched in df1)
  expect_error(
    stata_style_join(df1, df2, join_type = "full", relationship = "m:1", by = "id", assert = 1),
    "Assertion failed: unexpected merge categories found"
  )

  # Case 2: assert = c(1, 2) — should pass with left join
  expect_silent(
    result <- stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id",
                               assert = c(1, 2), drop_summaries = TRUE)
  )

  # Case 3: assert = c(1, 3) — should pass with right join
  expect_silent(
    stata_style_join(df1, df2, join_type = "full", relationship = "m:m",
                     by = "id", assert = c(1, 2, 3), drop_summaries = TRUE)
  )

  # Case 4: assert = NULL — default behavior, should not error
  expect_silent(
    stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id",
                     drop_summaries = TRUE)
  )

  # Case 5: invalid assert value
  expect_error(
    stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id",
                     assert = 5, drop_summaries = TRUE),
    "`assert` must be NULL or a vector containing only 1 \\(first only\\), 2 \\(second only\\), and/or 3 \\(matched\\)"
  )
})

