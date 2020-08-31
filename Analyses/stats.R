
library(tidyverse)
library(here)
library(rio)
library(broom)
library(afex)
library(xtable)
library(emmeans)

source("functions/descriptive.R")

daten <- import("Daten_clean.xlsx", setclass = "tbl")
daten_long <- import("Daten_long.xlsx", setclass = "tbl")

# daten %>% 
#   summarise(
#     mean = mean(V_15),
#     median = median(V_15),
#     sd = sd(V_15),
#     min = min(V_15),
#     Q1 = quantile(V_15, 0.25),
#     Q3 = quantile(V_15, 0.75),
#     max = max(V_15),
#     se = sd / n(),
#     N = n()
#   )
# 
# daten %>%
#   summarise(
#     mean = mean(A_15),
#     median = median(A_15),
#     sd = sd(A_15),
#     min = min(A_15),
#     Q1 = quantile(A_15, 0.25),
#     Q3 = quantile(A_15, 0.75),
#     max = max(A_15),
#     se = sd / n(),
#     N = n()
#   )

###################
# Regression
###################
avs <- daten %>% 
  select(V_15, A_15, NIV_15) %>% 
  as.matrix()

mvreg <- lm(avs ~ Bildungsjahre + Geschlecht + Alter + Schreib_freq,
           data = daten)

summary(mvreg)
tidy(mvreg)

tidy(mvreg) %>% 
  select(-response) %>% 
  xtable(digits = c(0, 0, 2, 2, 2, 3)) %>% 
  print(include.rownames = FALSE)


reg_v <- lm(V_15 ~ Bildungsjahre + Geschlecht + Alter + Schreib_freq, data = daten)
reg_a <- lm(A_15 ~ Bildungsjahre + Geschlecht + Alter + Schreib_freq, data = daten)
reg_niv <- lm(NIV_15 ~ Bildungsjahre + Geschlecht + Alter + Schreib_freq, data = daten)

tidy(reg_v)
tidy(reg_a)
tidy(reg_niv)

glance(reg_v)
glance(reg_a)
glance(reg_niv)
# 
# rbind(
#   tidy(reg_v),
#   tidy(reg_a),
#   tidy(reg_niv)
# ) %>% 
#   xtable(digits = c(0, 0, 2, 3, 2, 3)) %>% 
#   print(include.rownames = FALSE)

###################
# ANOVA
###################

daten_aov <- daten %>% 
  select(Probandencode, Altersgruppe, Geschlecht, V_13, V_14) %>% 
  mutate(Geschlecht = if_else(Geschlecht == 0, "m", "f")) %>% 
  mutate_at(vars(Altersgruppe, Geschlecht), as.factor) %>% 
  pivot_longer(
    cols = V_13:V_14,
    names_to = "Richtung",
    values_to = "V"
  ) %>% 
  mutate(Richtung = if_else(Richtung == "V_13", "Right_to_left", "Left_to_right"))

daten_aov %>%
  group_by(Richtung) %>% 
  summarise(
    mean = mean(V),
    median = median(V),
    sd = sd(V),
    min = min(V),
    Q1 = quantile(V, 0.25),
    Q3 = quantile(V, 0.75),
    max = max(V),
    se = sd / n(),
    N = n()
  )

anova <- aov_4(V ~ Altersgruppe + Geschlecht + (Richtung | Probandencode), data = daten_aov)
anova
emmeans(anova, ~ Richtung)
###################
# Shapiro Wilks
###################
options(scipen = 100)

clean <- daten_long %>% 
  filter(Z > 0) %>% 
  filter(V != 0)

# Velocity
N <- clean %>% 
  count(PERS) %>% 
  pluck("n")

result <- clean %>% 
  group_by(PERS) %>% 
  group_modify(~ tidy(shapiro.test(.x[["V"]]))) %>% 
  ungroup() %>% 
  select(W = statistic, p.value)

# Acceleration
result2 <- clean %>% 
  group_by(PERS) %>% 
  group_modify(~ tidy(shapiro.test(.x[["A"]]))) %>% 
  ungroup() %>% 
  select(W1 = statistic, p1 = p.value)

# LaTeX table
a <- result %>% 
  mutate(N = N) %>%
  select(N, W, p.value) 

cbind(a, result2) %>%
  as_tibble() %>% 
  mutate(p.value = if_else(p.value < 0.001, 0.001, p.value),
         p1 = if_else(p1 < 0.001, 0.001, p1)) %>% 
  xtable(digits = c(0, 0, 2, 3, 2, 3))


######################
# Descr. Stats for DV
##########################

clean %>% 
  select(V, A) %>%
  mutate(V = V * 10^3,
         A = A * 10^6) %>% 
  descriptive() %>% 
  bind_rows(
    daten %>% 
      select(NIV = NIV_15) %>%  
      descriptive()
  ) %>% 
  select(-SD, -Q1, -Q3) %>% 
  xtable(digits = c(0, 0, 0, 1, 1, 1, 1, 2)) %>% 
  print(include.rownames = FALSE)





