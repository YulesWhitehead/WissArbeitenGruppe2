attach(data)

#Ueberblick von Daten und deren Auspraegungen
str(data)

# Teildatensaetze
data_metrisch <- data[, 5:8]
data_kategorial <- data[, c(2:4, 9:12)]

# Berechnung von deskriptiven Statistiken fuer metrische Variablen
metr_stat(data_metrisch)

# Berechnung von deskriptiven Statistiken fuer kategoriale Variablen
kateg_stat(data_kategorial)

# a-iii Zusammenhang zwischen zwei kategorialen Variablen (z.B. Geschlecht und Überlebensstatus)
zusammenhang_Survived_Sex <- kat_stat(data$Survived, data$Sex)
#Cramer  normKont 
#0.5409359 0.6728632 
zusammenhang_Survived_Pclass <- kat_stat(data$Survived, data$Pclass)
#Cramer  normKont 
#0.3398174 0.4550200 
zusammenhang_Survived_Side <- kat_stat(data$Survived, data$Side)
#Cramer   normKont 
#0.05982927 0.08446033 
zusammenhang_Survived_Deck <- kat_stat(data$Survived, data$Deck)
#Cramer  normKont 
#0.1182824 0.1661186
zusammenhang_Survived_Anrede <- kat_stat(data$Survived, data$Anrede)
#Cramer  normKont 
#0.5796337 0.7092011

# a-iv Zusammenhang zwischen einer metrischen und einer dichotomen Variable (z.B. Alter und Überlebensstatus)
zusammenhang_Survived_Age <- analyzeMetricDichotomous(data$Age, data$Survived)
# p-Wert des t-Tests liegt bei 0,009
zusammenhang_Survived_Fare <- analyzeMetricDichotomous(data$Fare, data$Survived)
# p-Wert des t-Tests liegt bei p-value = 2.699e-11
zusammenhang_Survived_SibSp <- analyzeMetricDichotomous(data$SibSp, data$Survived)
# p-Wert des t-Tests liegt bei p-value = 0.2327
zusammenhang_Survived_Parch <- analyzeMetricDichotomous(data$Parch, data$Survived)
# p-Wert des t-Tests liegt bei p-value = 0.01339


# a-v Visualisierung
create_categorical_plot(data_kategorial, a = 1, b = 3)

