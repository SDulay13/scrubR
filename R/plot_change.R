#' Plot change over time
#' Create a line graph of different body weights recorded at different data points. Color and shape are mapped to the treatment or placebo group. If one entry (e.g. body weight) has dropped by more than 20% of its previous value, it is marked as "Flagged" when the user hovers over the data point. Interactive so that the user can hover over data points and see the ID, value drop percentage, and whether the entry was flagged.
#' @param data The dataset.
#' @param id_col The ID column has the individual observations.
#' @param var_cols The column containing the variable that is recorded over time and plotted.
#' @param color The variable mapped to the color of the graph.
#' @param cat The variable mapped to the shape of the geom.
#'
#' @return an interactive plot of the change of multiple discrete entries over time, color coded by category and flagged for significant (+20%) drops in value
#' @export
#'
#' @examples
#' path <- system.file("extdata", "mousedata.xlsx", package = "scrubR")
#' joined_data <- join_sheets(path, "ID", c("Birth", "Body Weight", "Outcome"))
#' plot_change(joined_data, "ID",
#' c("Body Weight 1", "Body Weight 2", "Body Weight 3"),
#' "Treatment", "Treatment")
plot_change <- function(data, id_col, var_cols, color, cat) {

  if (!base::is.data.frame(data)) {
    stop("Error: 'data' must be a data frame.")
  }

  if (!(id_col %in% base::names(data))) {
    stop(paste("Error: The ID column '", id_col, "' is missing in the dataset.", sep = ""))
  }

  missing_cols <- base::setdiff(var_cols, base::names(data))
  if (length(missing_cols) > 0) {
    stop(paste("Error: The following columns are missing in the dataset: ",
               base::paste(missing_cols, collapse = ", "), sep = ""))
  }

  for (col in var_cols) {
    data[[col]] <- base::as.numeric(data[[col]])
  }

  if (base::any(base::is.na(data[var_cols]))) {
    warning("Some values in the selected columns could not be converted to numeric and have been set to NA.")
  }

  long_data <- tidyr::pivot_longer(
    data = dplyr::select(data, dplyr::all_of(c(id_col, var_cols, color, cat))),
    cols = dplyr::all_of(var_cols),
    names_to = "Date",
    values_to = "Value"
  )

  long_data <- long_data |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::mutate(
      Value_Drop = (Value / dplyr::lag(Value)) - 1,
      Flag = dplyr::if_else(Value_Drop < -0.2, "Flagged", "Normal")
    )

  long_data[[id_col]] <- base::as.factor(long_data[[id_col]])
  long_data[[color]] <- base::as.factor(long_data[[color]])
  long_data[[cat]] <- base::as.factor(long_data[[cat]])

  plot <- plotly::plot_ly(
    long_data,
    x = ~Date,
    y = ~Value,
    color = ~.data[[color]],
    symbol = ~.data[[cat]],
    type = 'scatter',
    mode = 'lines+markers',
    text = ~base::paste(
      "ID:", .data[[id_col]], "<br>",
      "Value Drop:", base::round(Value_Drop * 100, 2), "%<br>",
      "Flag:", Flag
    ),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = "Change Over Time",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value"),
      legend = list(title = list(text = color))
    )

  return(plot)
}

