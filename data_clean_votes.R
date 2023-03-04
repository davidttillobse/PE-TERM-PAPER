#Data cleaning at country station level
rm(list=ls()) 

library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)

# AR 2022 -----------------------------------------------------------------
#official results i.e. with rep
AR_2022_Globais_REPEU <- read_excel("Electoral Data/AR_2022_Globais_REPEU.xlsx",
                                    sheet = "AR_2022_País", range = "A4:K23")

AR_2022_Globais_REPEU <- AR_2022_Globais_REPEU %>% 
  mutate(código = as.numeric(código)) %>% 
  filter(!código %in% c(600000, 800000, 900000)) %>% 
  mutate(territory = ifelse(código < 900000, "Europa", "Fora da Europa")) %>% 
  rename(country = "nome do território") %>% 
  select(código, country, inscritos, votos, brancos, nulos, territory) %>% 
  mutate(year = 2022)

#Old results i.e. without rep
AR_2022_Globais <- read_excel("Electoral Data/AR_2022_Globais.xlsx",
                                    sheet = "AR_2022_País", range = "A4:K23")

AR_2022_Globais <- AR_2022_Globais %>% 
  mutate(código = as.numeric(código)) %>% 
  filter(!código %in% c(600000, 800000, 900000)) %>% 
  mutate(territory = ifelse(código < 900000, "Europa", "Fora da Europa")) %>% 
  rename(country = "nome do território") %>% 
  select(código, country, inscritos, votos, brancos, nulos, territory) %>% 
  mutate(year = 2022)

AR_2022_Globais$territory[AR_2022_Globais$territory == "Europa"] <- "Europa_no treatment"

# AR 2019 -----------------------------------------------------------------
