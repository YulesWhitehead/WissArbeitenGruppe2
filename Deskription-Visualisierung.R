attach(data)

#Ueberblick von Daten und deren Auspraegungen
str(data)

# Berechnung von deskriptiven Statistiken fuer metrische Variablen
metr_stat(data[,c(1:4, 5:8)])

# Berechnung von deskriptiven Statistiken fuer kategoriale Variablen
kateg_stat(data[,9:12])

# a-iii Zusammenhang zwischen zwei kategorialen Variablen (z.B. Geschlecht und Überlebensstatus)
zusammenhang_kat_var <- kat_stat(data$Sex, data$Survived)

# a-iv Zusammenhang zwischen einer metrischen und einer dichotomen Variable (z.B. Alter und Überlebensstatus)
zusammenhang_met_dich <- analyzeMetricDichotomous(data$Age, data$Survived)
