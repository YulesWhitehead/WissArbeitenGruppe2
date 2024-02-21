# moda - Funktion zur Bestimmung des Modalwerts 
#        (am haeufigsten vorkommender Wert)
#
# Eingabe: 
# v    - eine kategoriale Variable
#
# Ausgabe: Modalwert

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# CramersV - Funktion zur Berechnung von Cramers V
#
# Eingabe:  zwei Vektoren mit Auspraegungen von kategorialen Variablen gleicher
#           Laenge
#
# Ausgabe:  Cramers V

CramersV <- function(Merkmal1, Merkmal2){
  Tabelle <- table(Merkmal1, Merkmal2)
  m <- min(ncol(Tabelle)-1, nrow(Tabelle)-1)
  n <- length(Merkmal1)
  Liste <- chisq.test(Merkmal1, Merkmal2)
  Chiquadrat <- as.numeric(Liste[1])
  sqrt(Chiquadrat/(n*m))
}

# normKontingenz -  Funktion zur Berechnung des normierten
#                   Kontingenzkoeffizienten nach Pearson
#
# Eingabe:  zwei Vektoren mit Auspraegungen von kategorialen Variablen gleicher
#           Laenge
#
# Ausgabe:  normierter Kontingenzkoeffizient nach Pearson

normKontingenz <- function(Merkmal1, Merkmal2){
  Tabelle <- table(Merkmal1, Merkmal2)
  m <- min(ncol(Tabelle), nrow(Tabelle))
  n <- length(Merkmal1)
  Liste <- chisq.test(Merkmal1, Merkmal2)
  Chiquadrat <- as.numeric(Liste[1])
  Kontingenz <- sqrt(Chiquadrat/(Chiquadrat + n))
  Korrektur <- sqrt((m-1)/m)
  Kontingenz/Korrektur
}
