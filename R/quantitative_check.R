#' Quantitative check
#' For quantitative variables, check and flag whether values for a specified column are not numerical
#' @param data The dataset.
#' @param column_name The column whose values need to be checked.
#'
#' Return a table of problematic rows that have non-numerical values for a specified column, displaying the problematic observation's ID, observed value in the given column, and error message
#' @export
#'
#' @examples
#' path <- system.file("extdata", "mousedata.xlsx", package = "scrubR")
#' joined_data <- join_sheets(path, "ID", c("Birth", "Body Weight", "Outcome"))
#' quantitative_check_table <- quantitative_check(joined_data, "Body Weight 3")

quantitative_check <- function(data, column_name) {
  if (is.null(data)) {
    stop("Error: The dataset does not exist or is NULL. Please provide a valid dataset.")
  }

  if (!inherits(data, "data.frame") && !inherits(data, "tbl_df")) {
    stop("Error: The dataset is not a valid data frame or tibble.")
  }

  if (!column_name %in% colnames(data)) {
    stop(paste("Error: The column", column_name, "does not exist in the dataset."))
  }

  numeric_pattern <- "^-?\\d*(\\.\\d+)?$"

  column_sym <- rlang::sym(column_name)

  problematic_rows <- data |>
    dplyr::filter(!is.na(!!column_sym) & !grepl(numeric_pattern, !!column_sym)) |>
    dplyr::mutate(
      Observed_Value = !!column_sym,
      Error_Message = "Value is not numeric"
    )

  if (nrow(problematic_rows) == 0) {
    message("No problematic rows found. All values are numeric.")
    return(NULL)
  }

  if ("ID" %in% colnames(problematic_rows)) {
    return(DT::datatable(problematic_rows |>
                           dplyr::select(ID, Observed_Value, Error_Message))
    )
  } else {
    warning("Warning: 'ID' column not found. Returning Observed_Value and Error_Message only.")
    return(DT::datatable(problematic_rows |>
                           dplyr::select(Observed_Value, Error_Message))
    )
  }
}
