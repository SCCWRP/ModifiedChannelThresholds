thresholds <- readxl::read_excel("data-raw/thresholds_07032024.xlsx")

usethis::use_data(thresholds, overwrite = TRUE)
