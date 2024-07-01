thresholds <- readxl::read_excel("data-raw/thresholds_07012024.xlsx")

usethis::use_data(thresholds, overwrite = TRUE)
