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

# a-iii Zusammenhang zwischen zwei kategorialen Variablen
zusammenhang_Survived_Sex <- kat_stat(Survived, Sex)
#Cramer  normKont 
#0.5409359 0.6728632 
#Diese Ergebnisse deuten auf einen mittleren bis hoeheren Zusammenhang zwischen 
#Sex und Survived hin.

zusammenhang_Survived_Pclass <- kat_stat(Survived, Pclass)
#Cramer  normKont 
#0.3398174 0.4550200
#Diese Ergebnisse deuten auf einen mittleren Zusammenhang zwischen 
#Pclass und Survived hin.

zusammenhang_Survived_Side <- kat_stat(Survived, Side)
#Cramer   normKont 
#0.05982927 0.08446033 
#Diese Ergebnisse deuten darauf hin, dass es fast kein Zusammenhang zwischen 
#Side und Survived gibt.

zusammenhang_Survived_Deck <- kat_stat(Survived, Deck)
#Cramer  normKont 
#0.1182824 0.1661186
#Diese Ergebnisse deuten auf einen schwachen Zusammenhang zwischen 
#Pclass und Survived hin.

zusammenhang_Survived_Anrede <- kat_stat(Survived, Anrede)
#Cramer  normKont 
#0.5796337 0.7092011
#Diese Ergebnisse deuten auf einen mittleren bis hoeheren Zusammenhang zwischen 
#Anrede und Survived hin.

# a-iv Zusammenhang zwischen einer metrischen und einer dichotomen Variable
zusammenhang_Survived_Age <- analyzeMetricDichotomous(Age, Survived)
# p-Wert des t-Tests liegt bei 0,009279

zusammenhang_Survived_Fare <- analyzeMetricDichotomous(Fare, Survived)
# p-Wert des t-Tests liegt bei p-value = 2.699e-11

zusammenhang_Survived_SibSp <- analyzeMetricDichotomous(SibSp, Survived)
# p-Wert des t-Tests liegt bei p-value = 0.2327

zusammenhang_Survived_Parch <- analyzeMetricDichotomous(Parch, Survived)
# p-Wert des t-Tests liegt bei p-value = 0.01339


# a-v Visualisierung

#Der Balkendiagramm zeigt die Häufigkeiten, mit denen Passagiere an verschiedenen Orten an Bord gegangen sind.
#Ein überwältigender Anteil der Passagiere ist in Southampton an Bord gegangen (ungefähr 70%), 
# gefolgt von einer deutlich kleineren Anzahl in Cherbourg, etwa 20%, und noch weniger aus Queenstown (circa 10%).
create_categorical_plot(data_kategorial, a = 1, b = 3)

