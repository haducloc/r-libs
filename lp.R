lp_df_sum_ <- function(df) {
  
  get_distinct_values <- function(column_values) {
    unique_values <- sort(unique(column_values))
    output <- character(0)
  
    total_values <- length(unique_values)
  
    for (i in seq_along(unique_values)) {
      current_value <- unique_values[i]
      current_value_str <- as.character(current_value)
  
      if (length(output) == 0) {
        output <- current_value_str
      } else {
        current_output <- paste(output, current_value_str, sep = ",")
        if (nchar(current_output) <= 64) {
          output <- current_output
        } else {
          output <- paste(output, "...", sep = "")
          break
        }
      }
    }
  
    return (paste(output, "(", total_values, " values)"))
  }

  # List of Columns
  
  result <- paste("***** Data Frame:", paste(ncol(df), "columns x", nrow(df), "rows\n\n"))
                  
  for (i in 1:ncol(df)) {
    col_name <- names(df)[i]
    col_type <- class(df[[col_name]])
    has_na <- any(is.na(df[[col_name]]))
    is_unique <- length(unique(df[[col_name]])) == nrow(df)
    
    result <- paste(result, i, "-", col_name, ":", col_type,
                    if (has_na) ", has NA(s)" else "",
                    if (is_unique) ", is UNIQUE\n" else "\n")
  }
  result <- paste(result, "\n")
  
  # Column Distinct Values
  result <- paste(result, "***** Column distinct values:\n\n")
  for (i in 1:ncol(df)) {
    col_name <- names(df)[i]
    distinct_values <- get_distinct_values(df[[col_name]])
    result <- paste(result, i, "-", col_name, ":", distinct_values, "\n")
  }
  
  return(paste(result, "\n"))
}

lp_df_sum <- function(df) {
  info <- lp_df_sum_(df)
  cat(info)
}
