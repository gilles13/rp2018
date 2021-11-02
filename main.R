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

# hab ~ REG + DEPT
d01 <- rp18[, {
	n = round(sum(IPONDI), 0) ;
	list(n = n)
}, by = list(REGION, DEPT)][
, .(DEPT=DEPT,
		n=n,
		prop=round(n/sum(n)*100, 2)),
by=REGION]

# région Paca (code 93)
d01[REGION == "93"]

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
