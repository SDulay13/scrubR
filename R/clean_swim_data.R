#' @name clean_swim_data
#' @title Clean and Transform Olympic Swimming Data
#'
#' @description
#' This function processes Olympic swimming event results, cleaning and transforming the data
#' to make it analysis-ready. It parses relay distances, converts performance times to seconds,
#' handles missing values, and provides options for renaming columns and verbose output.
#'
#' @importFrom rlang .data
#' @param data A data frame containing Olympic swimming results.
#' @param rename_columns Logical. If `TRUE`, renames columns to standardized names for consistency.
#' @param verbose Logical. If `TRUE`, outputs details about rows with problematic values during processing.
#'
#' @return A cleaned data frame with the following modifications:
#' \itemize{
#'   \item Relay distances are parsed and calculated.
#'   \item Results times are converted to seconds.
#'   \item Problematic rows can be flagged for review in verbose mode.
#' }
#' @export
#'
#' @examples
#' # Example usage of the clean_swim_data function
#' data <- read.csv("/cloud/project/data-raw/Olympic_Swimming_Results_1912to2020.csv")
#' swimming_data_cleaned <- clean_swim_data(data, rename_columns = TRUE, verbose = TRUE)

# Required library
library(dplyr)

# Helper function for time conversion
convert_time_to_seconds <- function(time) {
  if (grepl("^\\d+:\\d+\\.\\d+$", time)) {
    parts <- strsplit(time, ":")[[1]]
    as.numeric(parts[1]) * 60 + as.numeric(parts[2])
  } else if (grepl("^\\d+\\.\\d+$", time)) {
    as.numeric(time)
  } else if (grepl("^\\d+$", time)) {
    as.numeric(time)
  } else {
    NA_real_
  }
}

# Main cleaning function
clean_swim_data <- function(data, rename_columns = FALSE, verbose = FALSE) {
  # Rename columns if specified
  if (rename_columns) {
    data <- data |>
      dplyr::rename(
        location = Location,
        year = Year,
        distance_in_meters = Distance..in.meters.,
        stroke = Stroke,
        relay = Relay.,
        gender = Gender,
        team = Team,
        athlete = Athlete,
        results = Results,
        rank = Rank
      )
  }

  # Ensure required columns are present
  required_columns <- c("relay", "distance_in_meters", "results")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Parse relay distances
  parse_relay_distance <- function(distance_str) {
    if (grepl("\\d+x\\d+", distance_str)) {
      parts <- strsplit(distance_str, "x")[[1]]
      return(as.numeric(parts[1]) * as.numeric(parts[2]))
    }
    return(NA_real_)
  }

  # Cleaning
  swimming_data_cleaned <- data |>
    dplyr::mutate(
      parsed_relay_distance = dplyr::if_else(
        relay == 1 & !is.na(distance_in_meters),
        sapply(as.character(distance_in_meters), parse_relay_distance),
        NA_real_
      ),
      total_distance_in_meters = dplyr::if_else(
        !is.na(parsed_relay_distance),
        parsed_relay_distance,
        suppressWarnings(as.numeric(gsub("m", "", as.character(distance_in_meters), fixed = TRUE)))
      ),
      results_seconds = dplyr::if_else(
        !is.na(results),
        sapply(results, convert_time_to_seconds),
        NA_real_
      )
    ) |>
    dplyr::select(-parsed_relay_distance)

  # Debug if verbose
  if (verbose) {
    problematic_rows <- swimming_data_cleaned |>
      dplyr::filter(is.na(total_distance_in_meters) & !is.na(distance_in_meters))
    if (nrow(problematic_rows) > 0) {
      cat("Rows with problematic 'total_distance_in_meters' values:\n")
      print(problematic_rows |>
              dplyr::select(distance_in_meters, relay, total_distance_in_meters))
    }
  }

  return(swimming_data_cleaned)
}
