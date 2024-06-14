library(dplyr)
library(ggplot2)

mydf.c <- readr::read_csv("data-raw/mydf.c.csv") |>
  mutate(
    PSA2 = if_else(PSA6c %in% c("NC", "SN"), "Wet", "Arid"),
    Ash_Free_Dry_Mass_gPerm2 = Ash_Free_Dry_Mass_mgPercm2 * 10, # convert from mg/cm2 to g/m2
  )

asci.df.c <- readr::read_csv("data-raw/asci.df.c.csv") |>
  mutate(
    PSA2 = if_else(PSA6c %in% c("NC", "SN"), "Wet", "Arid"),
    Ash_Free_Dry_Mass_gPerm2 = Ash_Free_Dry_Mass_mgPercm2 * 10, # convert from mg/cm2 to g/m2
  )

#Biostiulatory variables
chem.varz <- c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", NULL)
om.varz <- c("Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_gPerm2", "PCT_MAP", NULL)
bs.varz <- c(chem.varz, om.varz)
bs.varz_pretty <- c("Total N", "Total P", "Chl-a", "AFDM", "% cover") #This is for graphs and plots
bs.varz.df <- data.frame(BiostimVar = bs.varz, BSPretty = factor(bs.varz_pretty, levels = bs.varz_pretty))
mod.id.varz <- c("MasterID", "MasterDate", "MasterDate_rep", "DevSet", "SelectedSample", "PSA2", "New_Lat", "New_Long")

mod.dat.csci <- mydf.c |>
  select(all_of(mod.id.varz), CSCI, all_of(chem.varz), all_of(om.varz)) |>
  rename(IndexScore = CSCI) |>
  mutate(Index = "CSCI")
mod.dat.asci_d<- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_D, all_of(chem.varz), all_of(om.varz)) |>
  rename(IndexScore = ASCI_D) |>
  mutate(Index = "ASCI_D")
mod.dat.asci_h <- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_H, all_of(chem.varz), all_of(om.varz)) |>
  rename(IndexScore = ASCI_H) |>
  mutate(Index = "ASCI_H")

mod.dat <- bind_rows(mod.dat.csci, mod.dat.asci_d, mod.dat.asci_h)

#######
#CREATE MODELS
#######
length.out.x <- 1000

max_TN <- 3
max_TP <- 1.5
max_chl <- 300
max_afdm <- 400
max_cov <- 100

my.newdfs <- data.frame(
  Nitrogen_Total_mgPerL = seq(from = 0, to = max_TN, length.out = length.out.x),
  Phosphorus_as_P_mgPerL = seq(from = 0, to = max_TP, length.out = length.out.x),
  Chlorophyll_a_mgPerm2 = seq(from = 0, to = max_chl, length.out = length.out.x),
  Ash_Free_Dry_Mass_gPerm2 = seq(from = 0, to = max_afdm, length.out = length.out.x),
  PCT_MAP = seq(from = 0, to = max_cov, length.out = length.out.x)
) |>
  tidyr::pivot_longer(cols = everything(), names_to = "BiostimVar", values_to = "Biostim") |>
  tidyr::nest(.by = "BiostimVar")

model.summary <- mod.dat |>
  filter(DevSet == "Cal", SelectedSample == "Selected") |>
  tidyr::pivot_longer(cols = Nitrogen_Total_mgPerL:PCT_MAP, names_to = "BiostimVar", values_to = "Biostim") |>
  group_by(Index, BiostimVar) |>
  mutate(
    model = list(scam::scam(IndexScore ~ s(Biostim, bs = "mpd"))),
    null_model = list(scam::scam(IndexScore ~ 1)),
    performance = purrr::map(model, function(mod) {
      list(AIC = mod$aic, gcv.ubre = mod$gcv.ubre, dgcv.ubre = mod$dgcv.ubre)
    }),
    AIC_null = purrr::map_dbl(null_model, function(mod) mod$aic),
    summary = purrr::map(model, function(mod) summary(mod)),
    summ_perf = purrr::map(summary, function(summ) {
      list(rsq = summ$r.sq, biostim_F = summ$s.table[3], biostim_p = summ$s.table[4], n = summ$n)
    })
  ) |>
  tidyr::unnest_wider(c(performance, summ_perf))

raw_dat <- model.summary |>
  inner_join(my.newdfs) |>
  select(model, data) |>
  distinct() |>
  mutate(
    Fit = list(predict(first(model), newdata = first(data), type = "response", se = T)$fit),
    SE = list(predict(first(model), newdata = first(data), type = "response", se = T)$se.fit)
  ) |>
  tidyr::unnest(cols = c(data, Fit, SE)) |>
  inner_join(bs.varz.df) |>
  ungroup() |>
  select(-model, Stressor = BSPretty, BiostimValue = Biostim) 

obs_points_df <- mod.dat |>
  tidyr::pivot_longer(cols = all_of(bs.varz), names_to = "BiostimVar", values_to = "BiostimValue", values_drop_na = T) |>
  filter(!(BiostimVar == "Nitrogen_Total_mgPerL" & BiostimValue > max_TN)) |>
  filter(!(BiostimVar == "Phosphorus_as_P_mgPerL" & BiostimValue > max_TP)) |>
  filter(!(BiostimVar == "Chlorophyll_a_mgPerm2" & BiostimValue > max_chl)) |>
  filter(!(BiostimVar == "Ash_Free_Dry_Mass_gPerm2" & BiostimValue > max_afdm)) |>
  inner_join(bs.varz.df) |>
  rename(Stressor = BSPretty)

usethis::use_data(obs_points_df, overwrite = TRUE)
usethis::use_data(raw_dat, overwrite = TRUE)
