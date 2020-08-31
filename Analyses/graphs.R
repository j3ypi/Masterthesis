
library(tidyverse)
library(here)
library(rio)
library(ggpubr)
library(patchwork)
library(broom)
library(latex2exp)

source("functions/descriptive.R")

daten <- import("Daten_clean.xlsx", setclass = "tbl")
daten_long <- import("Daten_long.xlsx", setclass = "tbl")

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

###################
# Q-Q Plot
###################

reg_v <- lm(V_15 ~ Bildungsjahre + Geschlecht + Alter + Schreib_freq, data = daten)
reg_a <- lm(A_15 ~ Bildungsjahre + Geschlecht + Alter + Schreib_freq, data = daten)
reg_niv <- lm(NIV_15 ~ Bildungsjahre + Geschlecht + Alter + Schreib_freq, data = daten)

q1 <- ggqqplot(augment(reg_v)$.resid) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
  ) 
q2 <- ggqqplot(augment(reg_a)$.resid) + 
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
  ) 
q3 <- ggqqplot(augment(reg_niv)$.resid) +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
  ) 
q4 <- ggqqplot(daten_aov$V) + 
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
  )  

q1 + q2 + q3 + q4 + 
  plot_layout(
    ncol = 2
  ) + 
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

# ggsave("qqplots.jpg", width = 8, height = 6, dpi = 800)

###################
# Barplot
###################

df <- daten_aov %>% 
  group_by(Altersgruppe, Richtung, Geschlecht) %>% 
  descriptive() %>% 
  select(-Variable) %>% 
  mutate(Altersgruppe = fct_relevel(Altersgruppe, "Jung", "Mittel", "Alt"),
         Richtung = if_else(Richtung == "Left_to_right", "Rightwards", "Leftwards"),
         Richtung = fct_relevel(Richtung, "Rightwards"),
         Geschlecht = if_else(Geschlecht == "f", "Female", "Male"),
         Altersgruppe = case_when(Altersgruppe == "Jung" ~ "20-29",
                                  Altersgruppe == "Mittel" ~ "30-49",
                                  Altersgruppe == "Alt" ~ "50-61"))

ggplot(df, aes(x = Altersgruppe, y = Mean, fill = Richtung)) + 
  geom_col(position = position_dodge(0.95)) + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                position = position_dodge(0.95), width = 0.4) + 
  facet_wrap(~ Geschlecht) + 
  scale_fill_grey() + 
  theme_pubr() + 
  theme(
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 16),
    strip.background = element_blank(),
    strip.text = element_text(size = 17),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.85),
    legend.text = element_text(size = 16),
  ) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 800)) + 
  labs(x = "Age group", 
       y = "Peak velocity in mm per s") 

ggsave("barplot.jpg", width = 8, height = 4, dpi = 600)

###################
# Density plot
###################

test <- daten_long %>% 
  mutate(V = V * 10^3)

test %>% 
  select(-STAT, -TIME, -X, -Y) %>% 
  descriptive()

densV <- test %>% 
  filter(Z > 0) %>%
  filter(V > 0) %>%
  filter(V < 1000) %>% 
  ggplot(aes(x = V)) + 
  geom_density(fill = "grey", alpha = 0.8) + 
  theme_pubr() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.003),
                     breaks = c(0.001, 0.002, 0.003)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1050), 
                     breaks = c(0, 250, 500, 750, 1000)) + 
  ggtitle("") +  
  labs(x = TeX("Velocity in $ $ $\\frac{mm}{s}$"),
                      y = "Density") + 
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13)
  ) 

densV <- test %>% 
  filter(Z > 0) %>%
  filter(V > 0) %>% 
  ggplot(aes(x = V)) + 
  geom_density(fill = "grey", alpha = 0.8) + 
  theme_pubr() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.003),
                     breaks = c(0.001, 0.002, 0.003)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1570), 
                     breaks = c(0, 250, 500, 750, 1000,
                                1250, 1500)) + 
  ggtitle("") +  
  labs(x = TeX("Velocity in $ $ $\\frac{mm}{s}$"),
       y = "Density") + 
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13)
  ) 

densA <- test %>% 
  filter(Z > 0) %>%
  filter(A > -0.025) %>% 
  filter(A < 0.025) %>% 
  ggplot(aes(x = A)) + 
  geom_density(fill = "grey", alpha = 0.8) + 
  theme_pubr() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250), 
                     breaks = c(0, 50, 150, 250)) + 
  ggtitle("") + 
  labs(x = TeX("Acceleration in $ $ $\\frac{mm}{ms^2}$"),
       y = "Density") + 
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
  ) 

dens <- test %>% 
  filter(Z > 0) %>% 
  filter(PERS == "ALAR0115") %>% 
  ggplot(aes(x = V)) + 
  geom_density(fill = "grey", alpha = 0.8) + 
  theme_pubr() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.004),
                     breaks = c(0.001, 0.002, 0.003, 0.004)) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 820), 
                     breaks = c(0, 200, 400, 600, 800)) +
  ggtitle("") + 
  labs(x = TeX("Velocity in $ $ $\\frac{mm}{s}$"),
       y = "Density") + 
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13)
  ) 

qq <- test %>%
  filter(Z > 0) %>% 
  filter(PERS == "ALAR0115") %>% 
  pluck("V") %>% 
  ggqqplot(size = 1) + 
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
  ) + 
  lims(x = c(-3, 3), y = c(0, 800))

densV + densA + dens + qq + 
  plot_layout(
    ncol = 2,
  ) + 
  plot_annotation(
    tag_levels = "a",
    tag_prefix = "(",
    tag_suffix = ")"
  )

ggsave("densities.jpg", width = 8, height = 6, dpi = 800)

# Look at all velocity profiles
# sampling <- function(names) {
#   test %>% 
#     filter(Z > 0) %>% 
#     filter(PERS == names) %>% 
#     ggplot(aes(x = V)) + 
#     geom_density(fill = "grey", alpha = 0.8)  
# }
# 
# persons <- unique(test$PERS)
# 
# persons %>% 
#   map(~sampling(.x))

