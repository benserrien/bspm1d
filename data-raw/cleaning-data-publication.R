# script to clean the data that were part of the original publication

library(tidyverse)
library(here)


# -------------------------------------------------------------------------
# gait_symmetry -----------------------------------------------------------

load(here("data-raw", "data-publication-IntBiomech", "ex_GaitSymmetry.RData"))
str(knee_angle)

gait_symmetry <- knee_angle %>%
  mutate(
    leg = rep(c("right_leg","left_leg"), each = 99),
    gait_cycle = rep(1:99, 2),
    .before = V1
  ) %>%
  nest(.by = c(gait_cycle, leg)) %>%
  pivot_wider(values_from = data, names_from = leg)



# -------------------------------------------------------------------------
# plantar_arch_angle ------------------------------------------------------

load(here("data-raw", "data-publication-IntBiomech",
          "ex_PlantarArchAngle.RData"))
str(Y)

plantar_arch_angle <- Y %>%
  mutate(
    condition = rep(c("condition 1","condition 2"), each = 10),
    person = paste0("SUBJ_", sprintf("%02d", rep(1:10, 2))),
    .before = V1
  ) %>%
  nest(.by = c(person, condition)) %>%
  pivot_wider(values_from = data, names_from = condition)




# -------------------------------------------------------------------------
# simulated_two_local_max -------------------------------------------------

load(here("data-raw", "data-publication-IntBiomech",
          "ex_SimulatedTwoLocalMax.RData"))
str(Y)

simulated_two_local_max <- Y %>%
  mutate(
    group  = rep(c("group 1","group 2"), each = 6),
    rep = paste0("REP_", sprintf("%02d", 1:12)),
    .before = V1
  ) %>%
  nest(.by = c(rep, group), .key = "simulated_data")


# -------------------------------------------------------------------------
# save as package data ----------------------------------------------------

usethis::use_data(
  gait_symmetry, overwrite = TRUE
)

usethis::use_data(
  plantar_arch_angle, overwrite = TRUE
)

usethis::use_data(
  simulated_two_local_max, overwrite = TRUE
)

