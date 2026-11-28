
# Tests for prepare_data

test_that("Function checks that input is a nonempty data frame", {

  expect_error(prepare_data("invalid input"))

  expect_error(prepare_data(c(1, 2, 3)))

  expect_error(prepare_data(data.frame()))

})

test_that("Column names are standardized", {

  df <- data.frame(
    " Country " = c("USA", "India"),
    "Total Cases" = c(100, 200),
    "TOTAL DEATHS" = c(5, 10),
    check.names = FALSE
  )

  result <- prepare_data(df)

  expect_equal(
    names(result),
    c("country", "total_cases", "total_deaths")
  )

})

test_that("Leading and trailing whitespace is removed", {

  df <- data.frame(
    region = c(" USA ", "India ", " South Korea"),
    cases = c(100, 200, 300)
  )

  result <- prepare_data(df)

  expect_equal(
    result$region,
    c("USA", "India", "South Korea")
  )

  expect_equal(result$cases, df$cases)

})

test_that("Duplicate rows are removed after cleaning", {

  df <- data.frame(
    region = c("USA", "India", " USA "),
    cases = c(100, 200, 100)
  )

  result <- prepare_data(df)

  expect_equal(nrow(result), 2)

  expect_equal(result$region, c("USA", "India"))

  expect_equal(result$cases, c(100, 200))

})

test_that("Row names are reset after duplicate removal", {

  df <- data.frame(
    region = c("USA", "USA", "India"),
    cases = c(100, 100, 200)
  )

  result <- prepare_data(df)

  expect_equal(rownames(result), c("1", "2"))

})

test_that("Missing value aliases are standardized", {

  df <- data.frame(
    region = c("A", "B", "C", "D", "E", "F", "G", "H"),
    cases = c("", " ", "NA", "N/A", "null", NA, "100", "0")
  )

  result <- prepare_data(df)

  expect_equal(
    result$cases,
    c(NA, NA, NA, NA, NA, NA, 100, 0)
  )

})

test_that("Numeric and missing values are preserved", {

  df <- data.frame(
    cases = c(100, NA, 0, 200)
  )

  result <- prepare_data(df)

  expect_identical(result$cases, df$cases)

})

test_that("Numeric text is converted to numeric values", {

  df <- data.frame(
    cases = c("100", "2,003", NA)
  )

  result <- prepare_data(df)

  expect_equal(result$cases, c(100, 2003, NA))

})

test_that("Nonnumeric character columns are preserved", {

  df <- data.frame(
    region = c("USA", "India", "South Korea")
  )

  result <- prepare_data(df)

  expect_identical(result$region, df$region)

})

test_that("Mixed character columns are not converted", {

  df <- data.frame(
    values = c("100", "Unknown", "200")
  )

  result <- prepare_data(df)

  expect_identical(result$values, df$values)

})
