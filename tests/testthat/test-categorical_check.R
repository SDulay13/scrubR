# sample data for testing
sample_data <- tibble::tibble(
  ID = c("A1", "A2", "A3", "A4"),
  Numeric_Column = c("12", "3.5", "NaN", "abc"),
  Non_Numeric_Column = c("cat", "dog", "fish", "bird")
)

test_that("Flags values that do not match the expected entries", {
  result <- categorical_check(sample_data, "Non_Numeric_Column", c("cat", "dog"))

  # extract the data from the datatable for testing
  result_data <- result$x$data

  # remove the extra row number column added by DT::datatable
  result_data <- result_data[, -1, drop = FALSE]

  expected_result <- tibble::tibble(
    ID = c("A3", "A4"),
    Observed_Value = c("fish", "bird"),
    Error_Message = c(
      "Value must be in the format: cat or dog",
      "Value must be in the format: cat or dog"
    )
  )

  testthat::expect_equal(result_data, as.data.frame(expected_result))
})

test_that("Indicates no problematic rows for expected values", {
  valid_data <- tibble::tibble(
    ID = c("B1", "B2"),
    Non_Numeric_Column = c("cat", "dog")
  )

  result <- categorical_check(valid_data, "Non_Numeric_Column", c("cat", "dog"))
  testthat::expect_null(result)
})

test_that("Displays error when an inputted column does not exist", {
  testthat::expect_error(
    categorical_check(sample_data, "Fake_Column", c("cat", "dog")),
    "Error: The column Fake_Column does not exist in the dataset."
  )
})

test_that("Displays error for an inputted NULL dataset", {
  testthat::expect_error(
    categorical_check(NULL, "Non_Numeric_Column", c("cat", "dog")),
    "Error: The dataset does not exist or is NULL."
  )
})

test_that("Displays error for non-data frame input", {
  non_data <- c("Not", "A", "Dataframe")

  testthat::expect_error(
    categorical_check(non_data, "Non_Numeric_Column", c("cat", "dog")),
    "Error: The dataset is not a valid data frame or tibble."
  )
})
