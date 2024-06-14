thresholds <- readxl::read_excel("data-raw/thresholds_06142024.xlsx")

usethis::use_data(thresholds, overwrite = TRUE)
