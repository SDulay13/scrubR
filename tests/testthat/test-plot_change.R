test_that("Function handles incorrect input types", {
  expect_error(plot_change(list(), "ID", c("Var1", "Var2"), "Color", "Shape"),
               "Error: 'data' must be a data frame.")

  dummy_data <- data.frame(ID = 1:3, Var1 = c(1, 2, 3), Var2 = c(4, 5, 6))
  expect_error(plot_change(dummy_data, "MissingID", c("Var1", "Var2"), "Color", "Shape"),
               "Error: The ID column 'MissingID' is missing in the dataset.")

  expect_error(plot_change(dummy_data, "ID", c("Var1", "MissingVar"), "Color", "Shape"),
               "Error: The following columns are missing in the dataset: MissingVar")
})

test_that("Warnings for non-numeric values", {
  dummy_data <- data.frame(
    ID = 1:3,
    Var1 = c(100, "non-numeric", 300),
    Var2 = c(150, 250, 350),
    Color = c("A", "B", "A"),
    Shape = c("X", "Y", "X")
  )

  expect_warning(
    plot_change(dummy_data, "ID", c("Var1", "Var2"), "Color", "Shape"),
    "Some values in the selected columns could not be converted to numeric and have been set to NA."
  )
})
