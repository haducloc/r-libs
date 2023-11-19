lp_df_sum_ <- function(df, type, line_width, columns) {

  get_distinct_values <- function(column_values) {
    unique_values <- sort(unique(column_values), na.last = FALSE)
    output <- character(0)

    total_values <- length(unique_values)

    for (i in seq_along(unique_values)) {
      current_value <- unique_values[i]
      current_value_str <- as.character(current_value)

      if (length(output) == 0) {
        output <- current_value_str
      } else {
        current_output <- paste(output, current_value_str, sep = ",")
        if (nchar(current_output) <= line_width) {
          output <- current_output
        } else {
          output <- paste(output, ",...", sep = "")
          break
        }
      }
    }

    return(paste(output, " (", total_values, ")", sep=""))
  }

  # Spec
  if (type == "spec") {
    result <- paste("***** Data Frame: ", paste(ncol(df), " columns x ", nrow(df), " rows\n\n", sep=""))

    for (i in 1:ncol(df)) {
      col_name <- names(df)[i]
      col_type <- class(df[[col_name]])
      has_na <- any(is.na(df[[col_name]]))
      is_unique <- length(unique(df[[col_name]])) == nrow(df)
      
      result <- paste(result, i, ". ", col_name, ": ", col_type,
                      if (has_na) ", NA: yes" else ", NA: no",
                      if (is_unique) ", Unique: yes\n" else ", Unique: no\n", sep="")
    }

    result <- paste(result, "\n", sep="")
    return(result)
  }

  # Distinct Values
  if (type == "data") {
    result <- paste("***** Sorted Distinct Values:\n\n", sep="")

    if (is.null(columns) || length(columns) == 0) {
      columns <- names(df)
    }

    for (col_name in columns) {
      if (col_name %in% names(df)) {
        distinct_values <- get_distinct_values(df[[col_name]])
        result <- paste(result, col_name, ": ", distinct_values, "\n", sep="")
      } else {
        warning(paste("Column '", col_name, "' not found in the data frame.", sep=""))
      }
    }
    result <- paste(result, "\n", sep="")
    return(result)
  }

  stop("Error: The given type is invalid. It must be 'spec' or 'data'.")
}

lp_df_sum <- function(df, type="spec", line_width=100, columns=NULL) {
  if (!is.character(type)) {
    stop("Error: type must be a character.")
  }
  if (!is.numeric(line_width) | line_width <= 0) {
    stop("Error: line_width must be a positive integer.")
  }
  
  types <- c('spec', 'data')

  if (!(type %in% types)) {
    stop("Error: type is invalid. It must be 'spec' or 'data'.")
  }
  
  info <- lp_df_sum_(df, type, line_width, columns)
  cat(info)
}
