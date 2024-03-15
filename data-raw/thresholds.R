thresholds <- readxl::read_excel("data-raw/thresholds_03152024.xlsx")

usethis::use_data(thresholds, overwrite = TRUE)
