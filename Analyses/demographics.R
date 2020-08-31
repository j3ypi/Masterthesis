
library(tidyverse)
library(lubridate)
library(here)
library(rio)

source("functions/descriptive.R")

daten <- dir("Demographie") %>% 
  map(~ import(here("Demographie", .x), 
               setclass = "tbl",
               col_names = FALSE))

test <- list()

for (i in seq_len(length(dir("Demographie")))) {
  
   test[[i]] <- daten[[i]] %>% 
    rename(Variable = ...1, Wert = ...2, Kommentar = ...3) %>% 
    select(-Kommentar) %>% 
    pivot_wider(names_from = "Variable", values_from = "Wert")
  
}

daten1 <- test %>% 
  map_df(rbind)

##################
# Datum
daten2 <- dir("Demographie") %>% 
  map(~ import(here("Demographie", .x), 
               setclass = "tbl",
               col_names = FALSE,
               col_types = c("text", "date", "text")))

test <- list()

for (i in seq_len(length(dir("Demographie")))) {
  
  test[[i]] <- daten2[[i]] %>% 
    rename(Variable = ...1, Wert = ...2, Kommentar = ...3) %>% 
    select(-Kommentar) %>% 
    pivot_wider(names_from = "Variable", values_from = "Wert")
  
}

daten3 <- test %>% 
  map_df(rbind) %>% 
  mutate_at(vars(Untersuchungsdatum, Geburtsdatum), ymd)

###################

daten <- daten1 %>% 
  mutate(Untersuchungsdatum = daten3$Untersuchungsdatum,
         Geburtsdatum = daten3$Geburtsdatum)

daten <- daten %>% 
  mutate(Alter = year(Untersuchungsdatum) - year(Geburtsdatum),
         Altersgruppe = case_when(
           Alter < 30 ~ "Jung",
           between(Alter, 30, 49) ~ "Mittel",
           Alter >= 50 ~ "Alt"
         ))

daten %>% 
  arrange(Untersuchungsdatum) %>% 
  select(Probandencode:Geburtsdatum, Alter, Altersgruppe) %>% 
  print(n = 51) 

daten %>% 
  group_by(Altersgruppe) %>% 
  count(Altersgruppe)

daten %>% 
  summarise(min = min(Alter),
            Mean = mean(Alter),
            max = max(Alter)) 

daten %>% 
  group_by(Geschlecht) %>% 
  count()

vars <- daten %>% 
  select(`Werfen`:`Einfädeln (Faden auf die Nadel zubewegen)`) %>% 
  mutate_all(as.numeric)

daten <- daten %>% 
  mutate(Hand_EHI = rowSums(vars)) 

demo <- daten %>% 
  select(Probandencode, Alter, Altersgruppe, 
         Geschlecht, Bildungsjahre, Schultypen, 
         Schreib_freq = Schreibhäufigkeit,
         Hand_EHI) %>% 
  mutate_at(vars(Geschlecht, Bildungsjahre, Schreib_freq), as.numeric)

# export(demo, "demo.xlsx")

######################
# Descriptive Stats
######################

demo <- import("Daten_clean.xlsx", setclass = "tbl")

demo %>% 
  descriptive()

daten %>% 
  group_by(Geschlecht) %>% 
  count()

daten %>% 
  select(Hand_EHI) %>%
  group_by(Hand_EHI) %>% 
  count()

daten %>% 
  count(Schreib_freq)

ggplot(demo, aes(x = Alter, y = ..density..)) +   
  geom_histogram(color = "black", fill = "white", bins = 18) + 
  geom_density(fill = "grey", alpha = 0.4) + 
  ggpubr::theme_pubr() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.08)) + 
  labs(x = "Age in years", y = "Density") + 
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
  ) + 
  ggtitle("") 

ggsave("histogram.jpg", width = 8, height = 4, dpi = 800)

daten_long <- import("Daten_long.xlsx", setclass = "tbl")

daten_long %>% 
  select(V, A, PERS) %>% 
  mutate(V = V * 10^3,
         A = A * 10^6) %>% 
  group_by(PERS) %>% 
  descriptive() %>% 
  arrange(Variable) %>% 
  as_tibble() %>% 
  print(n = 112)

daten_long %>% 
  select(V, A, PERS) %>% 
  print(n = 200) %>% 
  as.data.frame()



