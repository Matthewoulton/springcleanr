# springcleanr Package News

## [Unreleased]

### 🔧 `duplicates_function()`

-   **Tidyselect support added**\
    `variables` now accepts tidyselect-style expressions (e.g., `c(id, name)`, `starts_with("id")`) for flexible column selection.

-   **All columns by default**\
    If no `variables` are specified, the function defaults to `everything()` (i.e., all columns).
    This fixed bug when supplying NULL to variables.

-   **Improved error feedback**\
    Errors are now thrown when non-existent columns are referenced or tidyselect expressions match nothing.

-   **Tests added**:

    -   Tidyselect helper support
    -   Errors for non-existent or unmatched columns
    -   Compatibility with both quoted and unquoted column names

------------------------------------------------------------------------

### 🔧 `column_validation` Renamed from `validate_presence_and_variation()`

-   **Vectorised `variable_type`**\
    Accepts either a single value (`"numeric"`, `"string"`, `FALSE`) or a vector matching `variables`, enabling per-column type checking.

-   **New `check_constant` flag**\
    Optional argument (`TRUE` by default) to enable/disable the constant-value check (e.g., if a column has only one unique non-NA value).

-   **Deferred validation results**\
    All checks are now evaluated for all variables:

    -   If `hardstop = TRUE`, errors are thrown with a summary of all issues.
    -   If `hardstop = FALSE`, all issues are issued as warnings.
    -   This applies to type mismatches, NA-only columns

------------------------------------------------------------------------

### 🔧 `stata_style_join()`

-   **Arguments renamed for clarity**\
    `first_data` and `second_data` have been renamed to `first_df` and `second_df`.

-   **Default summary output enabled**\
    The `drop_summaries` argument now defaults to `FALSE`, and TRUE/FALSE redefined, meaning a summary of matched and unmatched rows will be printed by default after joins.

-   **Always computes internal merge indicator**\
    The merge status is now always computed internally, even if `merge_var = FALSE`. The column is dropped from the output if not requested, but used for summary statistics regardless.

-   **Dependency checks added**\
    Now checks for required packages (`dplyr`, `labelled`) and throws informative errors if they are not installed.

-   **Improved consistency and robustness**

    -   Join summaries no longer fail if `merge_var = FALSE`
    -   Conditional renaming or exclusion of `merge_var` is handled cleanly
    -   Many-to-many join warnings are issued when appropriate
    
 - **New `suffix` argument**  
  `stata_style_join()` now accepts a `suffix` argument (default `c(".x", ".y")`), which is passed to the underlying join operation. This allows users to control the names of overlapping columns.  
  Column update logic now uses these suffixes when identifying `.x` and `.y` columns for coalescing.

- **Separated out relationship argument (as in dplyr)**
  `stata_style_join()` now accepts "1:1", "m:1", "m:1", "m:m" in the relationship argument, instead of the join_type one.
  
- **Added assert argument to `stata_style_join()`**

  ------------------------------------------------------------------------

### 🔧 `graph_titler()`

I dropped this function.

### 🔧 `find_best_column_match()`

I dropped this function.
