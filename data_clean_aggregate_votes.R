#Data cleaning aggregate votes
rm(list=ls()) 

library(dplyr)
library(ggplot2)
library(readxl)
library(gridExtra)

#select europe and outside europe function
select.foreign <- function(dataset){
  dataset <- dataset %>% 
    filter(territory == "Europa" | territory == "Fora da Europa")
}

# AR 2022 -----------------------------------------------------------------
#official results i.e. with rep
AR_2022_Globais_REPEU <- read_excel("Electoral Data/AR_2022_Globais_REPEU.xlsx",
                                    sheet = "AR_2022_Global", range = "A4:M9")

AR_2022_Globais_REPEU <- AR_2022_Globais_REPEU %>% 
  select("nome do território", "inscritos", "votos", "brancos", "nulos") %>%
  rename(territory = "nome do território", 
         registered = "inscritos", 
         votes = "votos", 
         blank = "brancos", 
         null = "nulos") %>% 
  mutate(year = 2022,
         election = "AR") %>% 
  select.foreign

AR_2022_Globais <- read_excel("Electoral Data/AR_2022_Globais.xlsx",
                                    sheet = "AR_2022_Global", range = "A4:M9")

#Old results i.e. without rep
AR_2022_Globais <- AR_2022_Globais %>% 
  select("nome do território", "inscritos", "votos", "brancos", "nulos") %>%
  rename(territory = "nome do território", 
         registered = "inscritos", 
         votes = "votos", 
         blank = "brancos", 
         null = "nulos") %>% 
  mutate(year = 2022,
         election = "AR") %>% 
  filter(territory == "Europa")

AR_2022_Globais$territory[AR_2022_Globais$territory == "Europa"] <- "Europa_no treatment"


# AR 2019 -----------------------------------------------------------------
AR_2019_Globais <- read_excel("Electoral Data/AR_2019_Globais.xlsx",
                              sheet = "AR_2019_Global", range = "A4:M9")

AR_2019_Globais <- AR_2019_Globais %>% 
  select("círculo", "inscritos", "votantes", "brancos", "nulos") %>%
  rename(territory = "círculo", 
         registered = "inscritos", 
         votes = "votantes", 
         blank = "brancos", 
         null = "nulos") %>% 
  mutate(year = 2019,
         election = "AR") %>% 
  select.foreign


# AR 2015 -----------------------------------------------------------------
AR_2015_Globais <- read_excel("Electoral Data/AR_2015_Globais.xls",
                              sheet = "AR_2015_Global", range = "A5:M10")
AR_2015_Globais <- AR_2015_Globais %>% 
  select("Círculo", "inscritos", "votantes", "brancos", "nulos") %>%
  rename(territory = "Círculo", 
         registered = "inscritos", 
         votes = "votantes", 
         blank = "brancos", 
         null = "nulos") %>% 
  mutate(year = 2015,
         election = "AR") %>% 
  select.foreign


# AR 2011 -----------------------------------------------------------------
AR_2011_Globais <- read_excel("Electoral Data/AR_2011_Globais.xls",
                              sheet = "AR_2011_Global", range = "A5:M11")

AR_2011_Globais <- AR_2011_Globais %>% 
  select("Círculo", "inscritos", "votantes", "brancos", "nulos") %>%
  rename(territory = "Círculo", 
         registered = "inscritos", 
         votes = "votantes", 
         blank = "brancos", 
         null = "nulos") %>%
  mutate(year = 2011,
         election = "AR") %>% 
  filter(!is.na(territory)) %>% 
  select.foreign

# AR 2009 -----------------------------------------------------------------
AR_2009_Globais <- read_excel("Electoral Data/AR2009_Globais.xls",
                              sheet = "AR2009_Globais", range = "A28:M30")

AR_2009_Globais <- AR_2009_Globais %>% 
  select("nome do território", "inscritos", "votantes", "brancos", "nulos") %>%
  rename(territory = "nome do território", 
         registered = "inscritos", 
         votes = "votantes", 
         blank = "brancos", 
         null = "nulos") %>%
  mutate(year = 2009,
         election = "AR") %>% 
  filter(!is.na(territory)) %>% 
  select.foreign

# AR 2005 -----------------------------------------------------------------
AR_2005_Globais_1 <- read_excel("Electoral Data/AR2005_VOT_RESID_ESTRANG.xls",
                              sheet = "TOTAL", range = "C4:H6")
AR_2005_Globais_1$territory <- "Europa"

AR_2005_Globais_2 <- read_excel("Electoral Data/AR2005_VOT_RESID_ESTRANG.xls",
                                sheet = "TOTAL", range = "C13:H15")
AR_2005_Globais_2$territory <- "Fora da Europa"

AR_2005_Globais <- rbind(AR_2005_Globais_1, AR_2005_Globais_2)

AR_2005_Globais <- AR_2005_Globais %>% 
  select("Inscritos", "Votantes", "Em Branco", "Nulos", "territory") %>%
  rename(registered = "Inscritos", 
         votes = "Votantes", 
         blank = "Em Branco", 
         null = "Nulos") %>%
  mutate(year = 2005,
         election = "AR") %>% 
  filter(!is.na(null)) %>% 
  select.foreign

# AR 2002 -----------------------------------------------------------------
AR_2002_Globais_1 <- read_excel("Electoral Data/AR2002_VOT_RESID_ESTRANG.xls",
                                sheet = "TOTAL", range = "C4:H6")
AR_2002_Globais_1$territory <- "Europa"

AR_2002_Globais_2 <- read_excel("Electoral Data/AR2002_VOT_RESID_ESTRANG.xls",
                                sheet = "TOTAL", range = "C13:H15")
AR_2002_Globais_2$territory <- "Fora da Europa"

AR_2002_Globais <- rbind(AR_2002_Globais_1, AR_2002_Globais_2)

AR_2002_Globais <- AR_2002_Globais %>% 
  select("Inscritos", "Votantes", "Em Branco", "Nulos", "territory") %>%
  rename(registered = "Inscritos", 
         votes = "Votantes", 
         blank = "Em Branco", 
         null = "Nulos") %>%
  mutate(year = 2002,
         election = "AR") %>% 
  filter(!is.na(null)) %>% 
  select.foreign

# AR 1999 -----------------------------------------------------------------
AR_1999_Globais_1 <- read_excel("Electoral Data/AR1999_VOT_RESID_ESTRANG.xls",
                                sheet = "TOTAL", range = "C4:H6")
AR_1999_Globais_1$territory <- "Europa"

AR_1999_Globais_2 <- read_excel("Electoral Data/AR1999_VOT_RESID_ESTRANG.xls",
                                sheet = "TOTAL", range = "C13:H15")
AR_1999_Globais_2$territory <- "Fora da Europa"

AR_1999_Globais <- rbind(AR_1999_Globais_1, AR_1999_Globais_2)

AR_1999_Globais <- AR_1999_Globais %>% 
  select("Inscritos", "Votantes", "Em Branco", "Nulos", "territory") %>%
  rename(registered = "Inscritos", 
         votes = "Votantes", 
         blank = "Em Branco", 
         null = "Nulos") %>%
  mutate(year = 1999,
         election = "AR") %>% 
  filter(!is.na(null)) %>% 
  select.foreign

# Join dataset ------------------------------------------------------------

#Bind all data together
elections_master <- AR_2022_Globais_REPEU %>% 
  bind_rows(AR_2022_Globais,
            AR_2019_Globais,
            AR_2015_Globais,
            AR_2011_Globais,
            AR_2009_Globais,
            AR_2005_Globais,
            AR_2002_Globais,
            AR_1999_Globais) %>%
  mutate(year = as.Date(paste0(year, "-01-01"))) %>%
  mutate(year = format(year, "%Y")) %>% 
  mutate(election = factor(election))

#Make a copy of all Europa obs and make a copy as Europa_ [no repetition] 
AR_eu <- elections_master[elections_master$territory == "Europa", ]
AR_eu$territory[AR_eu$territory == "Europa"] <- "Europa_no treatment"
AR_eu <- filter(AR_eu, year < 2022)

#Bind again
elections_master <- elections_master %>% 
  bind_rows(AR_eu) %>% 
  mutate(territory = factor(territory))

# Create rates [turnout, etc] ---------------------------------------------
elections_master$turnout_rate <- elections_master$votes/elections_master$registered
elections_master$blank_rate <- elections_master$blank/elections_master$votes
elections_master$null_rate <- elections_master$null/elections_master$votes

# List all objects in the environment
all_objects <- ls()

# Remove all objects except the one called "X"
for (obj in all_objects) {
  if (obj != "elections_master") {
    rm(list = obj)
  }
}
