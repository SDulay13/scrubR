  test_that("Standardized column names are present", {
    data <- data.frame(
      Location = "Tokyo",
      Year = 2020,
      Distance..in.meters. = "100",
      Stroke = "Freestyle",
      Relay. = 0,
      Gender = "Men",
      Team = "USA",
      Athlete = "John Doe",
      Results = "1:30.5",
      Rank = 1,
      stringsAsFactors = FALSE
    )
    swimming_data_cleaned <- clean_swim_data(data, rename_columns = TRUE, verbose = FALSE)
    expect_true(all(c(
      "location", "year", "distance_in_meters", "stroke",
      "relay", "gender", "team", "athlete", "results", "rank"
    ) %in% colnames(swimming_data_cleaned)))
  })

  test_that("Relay distances are parsed correctly", {
    data <- data.frame(
      relay = c(1, 1),
      distance_in_meters = c("4x100", "4x200"),
      results = c("3:00.00", "6:00.00"),
      stringsAsFactors = FALSE
    )
    swimming_data_cleaned <- clean_swim_data(data, verbose = FALSE)
    expect_equal(unname(swimming_data_cleaned$total_distance_in_meters), c(400, 800))
  })

  test_that("Non-relay distances are parsed correctly", {
    data <- data.frame(
      relay = c(0, 0),
      distance_in_meters = c("100", "200"),
      results = c("1:00.00", "2:00.00"),
      stringsAsFactors = FALSE
    )
    swimming_data_cleaned <- clean_swim_data(data, verbose = FALSE)
    expect_equal(unname(swimming_data_cleaned$total_distance_in_meters), c(100, 200))
  })

  test_that("Times are converted to seconds correctly", {
    data <- data.frame(
      relay = c(0, 0, 0, 1),
      distance_in_meters = c("100", "200", "400", "4x100"),
      results = c("1:30.5", "45.2", "30", NA),
      stringsAsFactors = FALSE
    )
    swimming_data_cleaned <- clean_swim_data(data, verbose = FALSE)
    expect_equal(unname(swimming_data_cleaned$results_seconds), c(90.5, 45.2, 30, NA_real_))
  })

  test_that("Verbose mode reports problematic rows", {
    data <- data.frame(
      relay = c(0, 1),
      distance_in_meters = c("N/A", "4x100"),
      results = c("N/A", "3:00.00"),
      stringsAsFactors = FALSE
    )
    expect_output(
      clean_swim_data(data, verbose = TRUE),
      "Rows with problematic 'total_distance_in_meters' values:"
    )
  })

  test_that("Function handles missing required columns gracefully", {
    data <- data.frame(
      relay = c(0, 1),
      distance_in_meters = c("100", "4x100"),
      stringsAsFactors = FALSE
    )
    expect_error(
      clean_swim_data(data),
      "Missing required columns: results"
    )
  })
