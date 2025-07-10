# 📦 R Data Cleaning Utilities (STATA-style)

This package provides a set of functions to streamline and safeguard common data cleaning operations in R, inspired by workflows in STATA. It is designed for data cleaning, particularly when working with administrative datasets or structured survey data.

---

## ✨ Included Functions

### `stata_style_join()`

A robust merging function that mirrors STATA's `merge` behavior while retaining the flexibility of R.

**Key Features:**
- Explicit separation of SQL join type (`left`, `inner`, `full`) from observation-level relationship (`"1:1"`, `"1:m"`, `"m:1"`, `"m:m"`).
- Triggers validation errors or warnings based on join relationships, guarding against unintentional many-to-many joins.
- Optionally adds a labelled merge indicator (`merge_var`) similar to STATA’s `_merge`.
- Optional summary of matched/unmatched observations.
- Optional coalescing of overlapping columns (`column_update`).

**Example:**
```r
stata_style_join(df1, df2, join_type = "left", relationship = "m:1", by = "id")
```

---

### `duplicates_function()`

Flexible handling of duplicate rows, modeled after STATA’s `duplicates` and `isid`.

**Modes:**
- `"report"`: print duplicates by key.
- `"drop"`: remove duplicates.
- `"assert"`: raise an error if any duplicates exist (like `isid` in STATA).

**Example:**
```r
duplicates_function(data, by = c("id", "year"), mode = "assert")
```

---

### `validate_presence_and_variation()`

Performs structured checks on input columns, ensuring that key variables meet type, range, and value expectations.

**Checks include:**
- Column presence.
- Type validation (`"numeric"`, `"character"`).
- Membership in a user-specified value list (`outcome_list`).
- Constant value detection.
- NA diagnostics (with `NA_count = TRUE`).

**Example:**
```r
validate_presence_and_variation(
  data = df,
  variables = c("eligible", "takeup"),
  variable_type = "numeric",
  outcome_list = c(0, 1),
  NA_count = TRUE
)
```

---

## 📋 Installation

This package is not on CRAN. You can install it from source:

```r
# From local directory
devtools::load_all("path/to/your/package")

# Or install from GitHub (if hosted there)
# devtools::install_github("yourusername/datacleanr")
```