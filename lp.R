__lp_df_summary <- function(df) {
  # Validate
  if (!is.data.frame(df) && !inherits(df, "tbl_df")) {
    stop("The given df must be a data frame or tibble.")
  }
  
  # Duplicate
  has_duplicates <- any(duplicated(df))
  
  # unique
  unique_cols <- names(df)[sapply(df, function(col) length(unique(col)) == nrow(df))]
  non_unique_cols <- setdiff(names(df), unique_cols)
  
  # NA
  na_cols <- names(df)[sapply(df, function(col) any(is.na(col)))]
  non_na_cols <- setdiff(names(df), na_cols)
  
  # Column Name & Types
  not_snake_case_cols <- names(df)[!grepl("^[a-z_][a-z0-9_]*$", names(df))]
  
  col_info <- sapply(names(df), function(col) paste(col, ":", typeof(df[[col]])))
  df_desc <- paste(col_info, collapse = "|")
  
  summary_string <- paste(
    "\n\n|----------------------Data Frame/Tibble Summary------------------------",
    "\n", df_desc,
    "\n",
    "\n1. Duplicate rows? ", ifelse(has_duplicates, "Yes", "No"),
    "\n2. Unique: ", paste(unique_cols, collapse = ", "),
    "\n3. NOT unique: ", paste(non_unique_cols, collapse = ", "),
    "\n4. NA: ", paste(na_cols, collapse = ", "),
    "\n5. NOT NA: ", paste(non_na_cols, collapse = ", "),
    "\n6. NOT snake_case: ", paste(not_snake_case_cols, collapse = ", "),
    "\n------------------------------------------------------------------------|",
    "\n\n"
  )
  return(summary_string)
}

lp_df_summary <- function(df) {
  summary <- __lp_df_summary(df)
  cat(summary)
}
