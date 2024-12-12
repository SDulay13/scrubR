create_mock_excel <- function(file_path) {
  openxlsx::write.xlsx(
    list(
      "Sheet1" = data.frame(ID = c(1, 2, 3), Value1 = c("A", "B", "C")),
      "Sheet2" = data.frame(ID = c(3, 4, 5), Value2 = c("D", "E", "F")),
      "Sheet3" = data.frame(ID = c(1, 2, 3), Value3 = c("G", "H", "I"))
    ),
    file = file_path
  )
}


test_that("Function handles missing selected sheets", {
  temp_file <- tempfile(fileext = ".xlsx")
  create_mock_excel(temp_file)

  expect_error(join_sheets(temp_file, "ID", selected_sheets = c("NonExistentSheet")),
               "Error: None of the selected sheets exist in the file.")

  unlink(temp_file)
})

test_that("Function performs successful joins", {
  temp_file <- tempfile(fileext = ".xlsx")
  create_mock_excel(temp_file)

  joined_data <- join_sheets(temp_file, "ID")
  expect_s3_class(joined_data, "data.frame")
  expect_equal(ncol(joined_data), 4) # ID + Value1 + Value2 + Value3
  expect_equal(nrow(joined_data), 1) # Only ID = 3 is common across all sheets

  unlink(temp_file)
})
