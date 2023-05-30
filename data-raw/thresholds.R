thresholds <- readxl::read_excel("data-raw/thresholds.xlsx")

usethis::use_data(thresholds, overwrite = TRUE)
