#' @name calculate_metrics
#' @title Calculate metrics of data
#' @param df A data frame
#' @description  This function calculates metrics from the infectious disease
#'   data. Metrics of case counts are calculated, including the mean, median,
#'   minumum, and maximum.

#' @examples
#' df <- data.frame(
#'   date = c("2026-08-15", "2026-12-12"),
#'   region = c("Region A", "Region A"),
#'   cases = c(25, 32),
#'   population = c(100000, 100000)
#' )
#'
#' calculate_metrics(df)

calculate_metrics <- function(df) {

  # Check that the cases column exists
  if (!"cases" %in% names(df)) {
    stop("The dataset must contain a cases column")
  }

  # Calculate metrics of cases and store in data frame
  metrics <- data.frame(
    total_cases = sum(df$cases, na.rm = TRUE),
    mean_cases = mean(df$cases, na.rm = TRUE),
    min_cases = min(df$cases, na.rm = TRUE),
    max_cases = max(df$cases, na.rm = TRUE)
  )

  return(metrics)

}
