# springcleanr
Data cleaning functions for R.

These carry out some common steps in cleaning data. Many imitate or build on functions available in STATA.

The functions are:

duplicates_function: this mirrors STATA functionality (duplicates), allowing you to report or drop duplicates. It also has an 'assert' method, which contains a stopifnot if there are any duplicates (like isid in stata).

find_best_column_match: this function takes a string or an enquo object and a dataset, and looks in the dataset to find the best match among the columns. This makes nesting enquos much much easier, and avoids quotation problems when calling enquo (or similar) within functions. I use this a lot, and have found it much easier.

validate_presence_and_variation: this function has quite a few (relatively simple) capabilities, but the main is to check that a column exists in the dataset, that it is of a given type, and that its values are contained within a given set (e.g that they are 0 or 1).
graph_titler: this function works similarly to clean_names, but in reverse. It is intended to take nice tidyverse-style variable names (e.g gdp_growth) and render them in title case (e.g GDP Growth), so that when dynamically producing titles, it is straightforward to change the title of graphs dynamically  - you can just make the xtitle graph_titler(your_var_name). It is configured to work with enquo objects as well as strings.

stata_style_join: this function imitates STATA's merges, allowing more confidence that merges have not introduced errors. 
