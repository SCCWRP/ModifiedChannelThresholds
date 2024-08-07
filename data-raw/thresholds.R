thresholds <- readxl::read_excel("data-raw/thresholds_08072024.xlsx")

usethis::use_data(thresholds, overwrite = TRUE)
