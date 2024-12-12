# sample data for testing
sample_data <- tibble::tibble(
  ID = c("A1", "A2", "A3", "A4"),
  Numeric_Column = c("12", "3.5", "NaN", "abc"),
  Non_Numeric_Column = c("cat", "dog", "fish", "bird")
)

test_that("Flags non-numeric values for a chosen column", {
  result <- quantitative_check(sample_data, "Numeric_Column")

  # extract the data from the datatable for testing
  result_data <- result$x$data

  # remove the extra row number column added by DT::datatable
  result_data <- result_data[, -1, drop = FALSE]

  expected_result <- tibble::tibble(
    ID = c("A3", "A4"),
    Observed_Value = c("NaN", "abc"),
    Error_Message = c("Value is not numeric", "Value is not numeric")
  )

  testthat::expect_equal(result_data, as.data.frame(expected_result))
})

test_that("Indicates no problematic rows for a valid numeric column", {
  new_data <- tibble::tibble(
    ID = c("B1", "B2"),
    Numeric_Column = c("12", "3.5")
  )

  result <- quantitative_check(new_data, "Numeric_Column")
  testthat::expect_null(result)
})

test_that("Warning is issued when 'ID' column is missing", {
  # sample data without an 'ID' column
  no_id_data <- tibble::tibble(
    Numeric_Column = c("12", "3.5", "NaN", "abc")
  )

  expect_warning(
    result <- quantitative_check(no_id_data, "Numeric_Column"),
    "Warning: 'ID' column not found. Returning Observed_Value and Error_Message only."
  )

  # extract the data from the datatable for testing
  result_data <- result$x$data

  # remove the extra row number column added by DT::datatable
  result_data <- result_data[, -1, drop = FALSE]

  expected_result <- tibble::tibble(
    Observed_Value = c("NaN", "abc"),
    Error_Message = c("Value is not numeric", "Value is not numeric")
  )

  testthat::expect_equal(result_data, as.data.frame(expected_result))
})

test_that("Displays error when an inputted column does not exist", {
  testthat::expect_error(
    quantitative_check(sample_data, "Fake_Column"),
    "Error: The column Fake_Column does not exist in the dataset."
  )
})

test_that("Displays error for an inputted NULL dataset", {
  testthat::expect_error(
    quantitative_check(NULL, "Numeric_Column"),
    "Error: The dataset does not exist or is NULL."
  )
})
