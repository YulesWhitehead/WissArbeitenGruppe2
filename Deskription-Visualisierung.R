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


# Erstellen eines Indikators für obere vs. untere Decks
data$DeckNumeric <- as.integer(factor(data$Deck, levels = c("A", "B", "C", "D", "E", "F", "G")))
data$DeckIndicator <- ifelse(data$DeckNumeric <= 3, 1, 0)
# Analyse des Zusammenhangs zwischen Deck und Ticketpreis hinsichtlich der Überlebensrate
zusammenhang_Deck_Fare <- analyzeMetricDichotomous(data$Fare, data$DeckIndicator)
print(zusammenhang_Deck_Fare)


# a-v Visualisierung
create_categorical_plot(data_kategorial, a = 1, b = 3)


# Visualisierung des Zusammenhangs zwischen der Anzahl der Geschwister/Partner und der Überlebensrate
# Für diese Analyse wird ein Balkendiagramm für die Anzahl der Geschwister/Partner (SibSp) erstellen, um zu sehen, wie diese die Überlebensrate beeinflusst.

sibsp_survival_rate <- aggregate(Survived ~ SibSp, data, mean)

barplot(sibsp_survival_rate$Survived, names.arg = sibsp_survival_rate$SibSp,
        xlab = "Anzahl der Geschwister/Partner (SibSp)", ylab = "Überlebensrate",
        main = "Überlebensrate nach Anzahl der Geschwister/Partner",
        col = "blue")
