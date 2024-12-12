## code to prepare `DATASET` dataset goes here

Olympic_Swimming_Results_1912to2020 <- readr::read_csv("data-raw/Olympic_Swimming_Results_1912to2020.csv")

Olympic_Swimming <- dplyr::mutate(
  Olympic_Swimming_Results_1912to2020,
  `Relay?` = dplyr::case_when(`Relay?` == 0 ~ "No",
                              `Relay?` == 1 ~ "Yes")
)

usethis::use_data(Olympic_Swimming, overwrite = TRUE)

