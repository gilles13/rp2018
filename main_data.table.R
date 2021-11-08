# Fichier individu localisé au canton-ville (filcv)
# https://www.insee.fr/fr/statistiques/fichier/5542859/RP2018_INDCVI_csv.zip

library("data.table")

# sélection de variables
sel <- c("CANTVILLE", "REGION", "DEPT", "IPONDI",
         "CATL", "RECH", "MOCO", "CS1", "DIPL",
         "EMPL", "IMMI", "INATC", "SEXE", "NBPI",
         "TACT", "AGER20", "NA5")

# lecture du fichier
rp18 <- fread(file = "~/Documents/data/FD_INDCVI_2018.csv",
              sel = sel,
              colClasses = list(character=(c("REGION", "DEPT", "CATL", "AGER20",
                                             "CS1", "DIPL", "EMPL", "TACT", "NA5"))))

# rp18[REGION == "93",
# 		 .(n = sum(IPONDI)),
# 		 by = DEPT][, .(n = n, prop = round(100*n/sum(n),2))]

# hab ~ REG + DEPT
d01 <- rp18[, {
  n = round(sum(IPONDI), 0) ;
  list(n = n)
}, by = list(REGION, DEPT)][
, .(DEPT=DEPT,
    n=n,
    prop=round(n/sum(n)*100, 2)),
by=REGION]

# hab ~ REG + DEPT + CS1
# https://www.insee.fr/fr/statistiques/2012701
# FILTRE 15 ans ou plus
d02 <- rp18[!AGER20 %in% c("02", "05", "10", "14"),
            .(n = sum(IPONDI)),
            by=list(REGION, DEPT, CS1)]
d02[, total := sum(n),
   by = list(REGION, DEPT)][
   , prop := round(n/total*100, 1),
   by = c("REGION", "DEPT")][ , total := NULL]

dcast(d02[REGION == "93"], formula = DEPT ~ CS1, value.var = "prop")

# hab ~ REG + AGER20 + DEP
d03 <- rp18[, {
  n = sum(IPONDI) ;
  list(n = n)
}, by = list(REGION, DEPT, AGER20)][
, .(DEPT=DEPT,
    ager20=AGER20,
    n=n,
    prop=round(n/sum(n)*100, 2)),
by=REGION]

dcast(d03[REGION == "93"], DEPT ~ ager20, value.var="prop")

# hab ~ REG + CS1 + IMMI
d04 <- rp18[, {
  n = sum(IPONDI) ;
  list(n = n)
}, by = list(REGION, CS1, IMMI)][
, .(CS1=CS1,
    n=n,
    prop = round(n/sum(n)*100, 1)),
by = c("REGION", "IMMI")][order(REGION)]

dcast(d04[REGION == "93"], CS1 ~ IMMI, value.var = "prop")

# plot

library("ggplot2")

# hab par DEPT en PACA
g1 <- ggplot(data = d01[REGION == "93"],
						 mapping = aes(x = DEPT, y = n)) +
	geom_bar(stat = "identity")
g1


# hab par REG + AGER20

d03

g2 <- ggplot(data = d03[REGION == "93"],
						 mapping = aes(x = DEPT,
													 y = n),
						 col = "ager20",
						 stat = "stack")
