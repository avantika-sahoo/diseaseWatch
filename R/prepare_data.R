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
#'   date = c("2026-08-15", "2026-12-12"),
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

  # Remove leading and trailing whitespace in text
  for (col in names(df)) {

    if (is.character(df[[col]])) {

      df[[col]] <- trimws(df[[col]])

    }

  }

  # Remove duplicates
  df <- unique(df)

  # Reset row names
  rownames(df) <- NULL

  return(df)

}

