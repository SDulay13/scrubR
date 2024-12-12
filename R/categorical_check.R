
#' Categorical check
#'
#' @param data The dataset.
#' @param column_name The name of the column whose entries need to be checked.
#' @param valid_entries The expected entries for that column.
#'
#' @return a table of problematic rows that have values that do not match the expected values, displaying the problematic observation's ID, observed value in the given column, and error message.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "mousedata.xlsx", package = "scrubR")
#' joined_data <- join_sheets(path, "ID", c("Birth", "Body Weight", "Outcome"))
#' categorical_check(joined_data, "Treatment", c("Alive", "Dead"))
categorical_check <- function(data, column_name, valid_entries) {
  if (is.null(data)) {
    stop("Error: The dataset does not exist or is NULL. Please provide a valid dataset.")
  }

  if (!inherits(data, "data.frame") && !inherits(data, "tbl_df")) {
    stop("Error: The dataset is not a valid data frame or tibble.")
  }

  if (!column_name %in% colnames(data)) {
    stop(paste("Error: The column", column_name, "does not exist in the dataset."))
  }

  filtered_data <- dplyr::filter(data, !(data[[column_name]] %in% valid_entries))

  if (nrow(filtered_data) == 0) {
    message("No problematic rows found. All values are valid.")
    return(NULL)
  }

  filtered_data$Observed_Value <- filtered_data[[column_name]]
  filtered_data$Error_Message <- stringr::str_c(
    "Value must be in the format: ",
    stringr::str_c(valid_entries, collapse = " or ")
  )

  problematic_rows <- dplyr::select(filtered_data, ID, Observed_Value, Error_Message)

  return(DT::datatable(problematic_rows))
}
