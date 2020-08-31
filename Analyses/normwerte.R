
library(tidyverse)
library(here)
library(rio)
library(xtable)

daten <- import("cs_param.dbf", setclass = "tbl")

test <- daten %>% 
  filter(VERS < 12) %>% 
  mutate(Hand = str_extract(PERS, "[A-Z]$"),
         ID = rep(1:51, each = 21)) %>% 
  select(ID, PERS,VERS,  Hand, FREQ, PRESS, TIMEON, NIV,
         ABSVON, ABSVOFF, PERCON)

test %>% 
  select(ID, VERS ,Hand, FREQ, PRESS, TIMEON, NIV,
         ABSVON, ABSVOFF, PERCON) %>% 
  pivot_wider(
    names_from = c(VERS, Hand),
    values_from = c(FREQ, PRESS, TIMEON, NIV,
                    ABSVON, ABSVOFF, PERCON)
  ) %>% 
  print(n = 51)

# merge this with demographics

daten1 <- daten %>% 
  select(VERS, FREQ, PRESS, TIMEON, NIV,
         ABSVON, ABSVOFF, PERCON) %>% 
  filter(VERS < 12) %>% 
  group_by(VERS) %>% 
  summarise_all(list(mean = mean, sd = sd)) %>% 
  mutate_if(is.numeric, round, 2)

daten1 <- daten1 %>% 
  select(VERS, FREQ_mean, FREQ_sd, PRESS_mean, PRESS_sd, 
         TIMEON_mean, TIMEON_sd, 
         NIV_mean, NIV_sd, ABSVON_mean, ABSVON_sd, ABSVOFF_mean,
         ABSVOFF_sd, PERCON_mean, PERCON_sd)

export(daten1, "normwerte.xlsx")

daten <- daten %>% 
  select(-PEDESC)

export(daten, "cs_param.xlsx")

daten2 <- import("CS_Stat_A.dbf", setclass = "tbl")
export(daten2, "cs_acceleration.xlsx")

daten3 <- import("CS_Stat_V.dbf", setclass = "tbl")
export(daten3, "cs_velocity.xlsx")

################################
# Reference values Master thesis
################################

daten %>% 
  select(VERS, FREQ, PRESS, NIV, PERCNIV,
         ABSVON, PERCON) %>% 
  group_by(VERS) %>% 
  summarise_all(list(mean = mean, sd = sd)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(Task = VERS, FREQ_mean, FREQ_sd, PRESS_mean, PRESS_sd,
         NIV_mean, NIV_sd, PERCNIV_mean, V_mean = ABSVON_mean, V_sd = ABSVON_sd, 
         PERCON_mean) %>% 
  xtable(digits = c(0, 0, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1)) %>% 
  print(include.rownames = FALSE)

















