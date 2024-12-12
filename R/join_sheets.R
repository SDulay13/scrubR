#' Join excel sheets
#' Join excel sheets and check for mismatches in the key and flag the mismatched entries
#' @param file_path The file path for the xlsx file.
#' @param shared_column The column to use as key for join.
#' @param selected_sheets The sheets to be joined.
#'
#' @return a joined data set and a if a mismatch is present, a table with the mismatched keys and the corresponding sheet name
#' @export
#'
#' @examples
#' path <- system.file("extdata", "mousedata.xlsx", package = "scrubR")
#' joined_data <- join_sheets(path, "ID", c("Birth", "Body Weight", "Outcome"))

join_sheets <- function(file_path, shared_column, selected_sheets = NULL) {
  sheets <- readxl::excel_sheets(file_path)

  if (!is.null(selected_sheets)) {
    sheets <- intersect(sheets, selected_sheets)
    if (length(sheets) == 0) {
      stop("Error: None of the selected sheets exist in the file.")
    }
  }

  data_list <- lapply(sheets, function(sheet) {
    data <- readxl::read_excel(file_path, sheet = sheet)
    list(sheet_name = sheet, data = data)
  })

  data_list <- Filter(function(item) shared_column %in% names(item$data), data_list)

  if (length(data_list) < 2) {
    stop("Error: Not enough sheets contain the shared column.")
  }

  mismatches <- list()

  joined_data <- data_list[[1]]$data
  for (i in 2:length(data_list)) {
    joined_data <- dplyr::inner_join(joined_data, data_list[[i]]$data, by = shared_column)

    new_data <- data_list[[i]]$data
    mismatch_ids <- setdiff(new_data[[shared_column]], joined_data[[shared_column]])

    if (length(mismatch_ids) > 0) {
      mismatch_rows <- dplyr::filter(new_data, !!rlang::sym(shared_column) %in% mismatch_ids)

      mismatch_rows$sheet_name <- data_list[[i]]$sheet_name

      mismatch_rows <- mismatch_rows |>
        dplyr::select(!!rlang::sym(shared_column), sheet_name, dplyr::everything())

      mismatches[[data_list[[i]]$sheet_name]] <- mismatch_rows
    }
  }

  if (length(mismatches) > 0) {
    message("Mismatches detected:")
    for (key in names(mismatches)) {
      print(DT::datatable(mismatches[[key]], options = list(pageLength = 5),
                          caption = paste("Mismatches in:", key)))
    }
  } else {
    message("No mismatches detected.")
  }

  return(joined_data)
}

