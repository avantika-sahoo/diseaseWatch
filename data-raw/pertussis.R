## code to prepare `pertussis` dataset goes here

# Import the raw pertussis dataset
pertussis_raw <- read.csv(
  "data-raw/NNDSSPertussisYearComparisonWEEKLY.csv",
  check.names = FALSE
)

# Inspect dataset
print(head(pertussis_raw))
str(pertussis_raw)
print(summary(pertussis_raw))
print(names(pertussis_raw))
