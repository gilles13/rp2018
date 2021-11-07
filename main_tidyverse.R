# Fichier individu localisé au canton-ville (filcv)
# https://www.insee.fr/fr/statistiques/fichier/5542859/RP2018_INDCVI_csv.zip

# install.packages("tidyverse")

library("tidyverse")
library("readr")

# sélection de variables
sel <- c("CANTVILLE", "REGION", "DEPT", "IPONDI",
         "CATL", "RECH", "MOCO", "CS1", "DIPL",
         "EMPL", "IMMI", "INATC", "SEXE", "NBPI",
         "TACT", "AGER20", "NA5")

# lecture du fichier
# rp18 <- read_csv2(file = "~/Documents/data/FD_INDCVI_2018.csv")

#                   col_types = list(character=(c("REGION", "DEPT", "CATL", "AGER20",
#                                              "CS1", "DIPL", "EMPL", "TACT", "NA5"))))

# transform tibble
rp18 <- as_tibble(rp18)

# hab par REGION + DEPT
rp18 %>%
  filter(REGION == "93") %>% 
  group_by(DEPT) %>% 
  summarise(n = sum(IPONDI, na.rm = TRUE))

# hab ~ REGION + DEP + CS1 large
rp18 %>%
  filter(REGION == "93") %>% 
  group_by(DEPT, CS1) %>% 
  summarise(n = sum(IPONDI, na.rm = TRUE)) %>%
  pivot_wider(id_cols = DEPT,
              names_from = CS1,
              values_from = n)

# hab ~ REGION + DEP + AGER20 large
rp18 %>%
  filter(REGION == "93") %>% 
  group_by(DEPT, AGER20) %>% 
  summarise(n = sum(IPONDI, na.rm = TRUE)) %>%
  pivot_wider(id_cols = DEPT,
              names_from = AGER20,
              values_from = n)
