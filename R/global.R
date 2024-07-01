library(ggplot2)
library(shiny)

source('data-raw/channel_cross_walk.R')
source('data-raw/thresholds.R')

ggplot2::theme_set(ggplot2::theme_bw(base_size = 12))

data(thresholds)
data(obs_points_df)
data(raw_dat)
data(channel_cross_walk)

max_TN <- 3
max_TP <- 1.5
max_chl <- 300
max_afdm <- 400
max_cov <- 100

region_choices <- channel_cross_walk |>
  dplyr::distinct(Region) |>
  dplyr::pull()

flow_duration_choices <- channel_cross_walk |>
  dplyr::distinct(FlowDuration) |>
  dplyr::pull()


modification_type_choices <- list(`Natural or minimally modified channel` = "Natural",
                                  `Hard-bottom engineered channel` = "HB",
                                  `Soft-bottom engineered channel with 2 hardened sides` = "SB2",
                                  `Soft-bottom engineered channel with 1 hardened side` = "SB1",
                                  `Soft-bottom engineered channel with 0 hardened sides` = "SB0",
                                  `Constructed Channel` = "CC")

stringency_choices <- thresholds |> 
  dplyr::distinct(Stringency) |> 
  dplyr::arrange(desc(Stringency)) |>
  dplyr::pull()

indicator_choices <- c("ASCI_D", "ASCI_H", "CSCI", "TN", "TP", "Chl-a", "AFDM", "% cover")
