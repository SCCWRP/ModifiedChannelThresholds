channel_cross_walk <- readxl::read_excel("data-raw/ChannelClassCrosswalk.xlsx")

usethis::use_data(channel_cross_walk, overwrite = TRUE)
