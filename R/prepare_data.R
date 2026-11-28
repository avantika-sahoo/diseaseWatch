#' @name prepare_data
#' @title Prepares dataset for analysis
#' @param df A data frame
#' @description  This function prepares a data frame containing infectious
#'   data for analysis. This function will clean the data by removing
#'   duplicates, standardize formats, and handle missing values.
#' @returns A cleaned and standardized data frame
#' @export
#' @examples
#' df <- data.frame(
#'   region = c("Region A", "Region A"),
#'   cases = c(25, 32),
#'   population = c(100000, 100000)
#' )
#'
#' prepare_data(df)

prepare_data <- function(df) {

  # Check that the input is a nonempty data frame
  if (!is.data.frame(df) || nrow(df) == 0) {

    stop("Input must be a nonempty data frame")

  }

  # Standardize column names
  names(df) <- tolower(trimws(names(df)))
  names(df) <- gsub("\\s+", "_", names(df))

  # Common aliases for missing values
  missing_values <- c("", "NA", "N/A", "NULL")

  # Clean character columns
  for (col in names(df)) {

    if (is.character(df[[col]])) {

      # Remove leading and trailing whitespace
      df[[col]] <- trimws(df[[col]])

      # Standardize missing values
      df[[col]][toupper(df[[col]]) %in% missing_values] <- NA_character_

    }

  }

  # Remove duplicates
  df <- unique(df)

  # Reset row names
  rownames(df) <- NULL

  return(df)

}

