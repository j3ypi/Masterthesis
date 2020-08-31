
library(tidyverse)
library(lubridate)
library(here)
library(rio)

source("functions/descriptive.R")

demo <- import("demo.xlsx", setclass = "tbl")
velocity <- import("CS_Stat_V.dbf", setclass = "tbl")
acceleration <- import("CS_Stat_A.dbf", setclass = "tbl")
inversions <- import("cs_param.dbf", setclass = "tbl")

#########################
# Params for regression
#########################

# Mean velocity
velo <- velocity %>% 
  filter(VERS == 13 | VERS == 14 | VERS == 15) %>% 
  filter_at(vars(PERS), ~ str_detect(., "R$")) %>% 
  select(PERS, VERS, V = MAX)

# Mean acceleration
acc <- acceleration %>% 
  filter(VERS == 13 | VERS == 14 | VERS == 15) %>% 
  filter_at(vars(PERS), ~ str_detect(., "R$")) %>% 
  select(A = MAX)

# NIV
niv <- inversions %>% 
  filter(VERS == 13 | VERS == 14 | VERS == 15) %>% 
  filter_at(vars(PERS), ~ str_detect(., "R$")) %>% 
  select(NIV)

# Join velo, acc, niv and demo
binded <- cbind(velo, acc, niv) %>% 
  as_tibble() %>% 
  pivot_wider(
    names_from = "VERS",
    values_from = V:NIV
  ) %>%
  mutate(PERS = as.character(PERS)) %>% 
  mutate(PERS = if_else(PERS == "MAMR", "MIMR", PERS)) %>% 
  arrange(PERS) %>% 
  select(-PERS)

demo <- demo %>% 
  mutate(Probandencode = str_extract(Probandencode, "^[a-zA-Z0-9]+")) %>% 
  arrange(Probandencode)

daten <- cbind(demo, binded) %>% 
  as_tibble()

export(daten, "Daten_clean.xlsx")

######################################
# Velocity and acceleration profile
# for each person
# (for shapiro wilks and density plot)
######################################

files <- dir("Coordinates", pattern = ".dbf$")

raw <- import_list(here("Coordinates", files), setclass = "tbl")

calc_params <- function(data) {
  
  data <- data[-c(1:4) , ]
  veloc <- vector("numeric", nrow(data))
  accel <- vector("numeric", nrow(data))
  
  # calc velocity_i from coordinates
  for (i in 1:nrow(data)) {
    x <- (data[["X"]][i + 1] - data[["X"]][i])^2
    y <- (data[["Y"]][i + 1] - data[["Y"]][i])^2
    
    path <- sqrt(x + y)
    
    veloc[i] <- path / (data[["TIME"]][i + 1] - data[["TIME"]][i])
  }
  
  data[["V"]] <- veloc  
  
  # calc acceleration_i from coordinates
  for (i in 1:nrow(data)) {
    accel[i] <- (data[["V"]][i + 1] - data[["V"]][i]) / 
      (data[["TIME"]][i + 1] - data[["TIME"]][i])
  }
  
  data[["A"]] <- accel
  
  data
}

rawv <- raw %>% 
  map(~ calc_params(.x)) 

# Add personal identifier
for (i in 1:51) {
  rawv[[i]][["PERS"]] <- rep(names(rawv)[i], nrow(rawv[[i]]))
}

daten1 <- rawv %>% 
  map_df(rbind)

# export(daten1, "Daten_long.xlsx")





