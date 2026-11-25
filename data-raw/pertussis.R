## code to prepare `pertussis` dataset goes here

# Import the raw pertussis dataset
pertussis_raw <- read.csv(
  "data-raw/NNDSSPertussisYearComparisonWEEKLY.csv",
  check.names = FALSE
)

# Inspect dataset
head(pertussis_raw)
str(pertussis_raw)
summary(pertussis_raw)
