#This would be how we would clean the PR elecitons data... 
#but I don't think we should use these as they are not comparable
#the turnout rates are very different and a lot less people voted as there is no mail in option

# PR 2021 -----------------------------------------------------------------
PR_2021_Globais <- read_excel("Electoral Data/PR_2021_Globais.xlsx",
                              sheet = "PR_2021_País", range = "A5:N65")
PR_2021_total <- PR_2021_Globais[1,] #save first row which contains totals

PR_2021_Globais <- PR_2021_Globais %>% 
  slice(2:n()) %>%  #select all but first row
  select("código", "inscritos", "votantes", "brancos", "nulos", "votos validamente expressos") %>% 
  rename(registered = "inscritos",
         votes = "votos validamente expressos",
         blank = "brancos",
         null = "nulos") %>% 
  mutate(código = as.numeric(código)) %>% 
  mutate(year = 2021,
         election = "PR") %>% 
  mutate(territory = ifelse(código < 900000, "Europa", "Fora da Europa")) %>% 
  group_by(territory, year, election) %>% 
  summarise(registered = sum(registered),
            votes = sum(votes),
            blank = sum(blank),
            null = sum(null))

sum(PR_2021_Globais$votes) == PR_2021_total$`votos validamente expressos` #checks out we are good

