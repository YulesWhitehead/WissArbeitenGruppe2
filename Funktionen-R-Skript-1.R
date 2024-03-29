# Laden der benoetigten Pakete
library(psych)
library(e1071)

#a-i

# metr_stat - Funktion zur Berechnung deskriptiver Statistiken fuer metrische
#             Variablen
# 
# Eingabe: 
# data      - Datensatz, fuer den die Statistiken berechnet werden sollen
#
# Ausgabe:
# result    - Ergebnismatrix als Dataframe mit allen berechneten Statistiken

metr_stat <- function(data) {
  # Erstellen einer leeren Ergebnismatrix
  result <- matrix(NA, nrow = length(data), ncol = 16)
  colnames(result) <- c("Variable", "Anzahl", "NA-Anzahl", "Minimum", "Maximum", 
                        "Spannweite", "Median", "Erste Quartil", "Letzte Quartil", 
                        "Interquartilsabstand", "Arithm. Mittel", 
                        "Varianz", "Standardabw.", "Variationskoef.",
                        "Schiefe", "Kurtosis")
  
  # Iteration ueber die Variablen im Datensatz
  for(i in 1:length(data)) {
    var_name <- names(data)[i]  
    var_data <- data[[i]]
    Max <- max(var_data)
    Min <- min(var_data)
    Mean <- mean(var_data)
    Sd <- sd(var_data)
    q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    q2 <- quantile(var_data, 0.75, na.rm = TRUE)
    
    # Speichern der berechneten Statistiken in die Ergebnismatrix
    result[i, 1] <- var_name
    result[i, 2] <- length(var_data)
    result[i, 3] <- sum(is.na(var_data))
    result[i, 4] <- Min
    result[i, 5] <- Max
    result[i, 6] <- Max - Min
    result[i, 7] <- median(var_data)
    result[i, 8] <- q1
    result[i, 9] <- q2
    result[i, 10] <- q2 - q1
    result[i, 11] <- Mean
    result[i, 12] <- var(var_data)
    result[i, 13] <- Sd
    result[i, 14] <- Sd / Mean
    result[i, 15] <- skewness(var_data)
    result[i, 16] <- kurtosis(var_data)
  }
  
  # Rueckgabe der Ergebnismatrix als Dataframe
  return(as.data.frame(result))
}


#a-ii

# kateg_stat - Funktion zur Berechnung deskriptiver Statistiken fuer kategoriale
#              Variablen
# 
# Eingabe: 
# data       - Datensatz, fuer den die Statistiken berechnet werden sollen
#
# Ausgabe:
# result     - Ergebnismatrix als Dataframe mit allen berechneten Statistiken

kateg_stat <- function(data){
  # Erstellen einer leeren Ergebnismatrix
  result <- matrix(NA, nrow = ncol(data), ncol = 5)
  colnames(result) <- c("Variable", "Anzahl", "NA-Anzahl", "Modalwert", 
                        "Relative Haeufigkeit")
  
  # Iteration über die Variablen im Datensatz
  for(i in 1:ncol(data)) {
    var_name <- names(data)[i]  
    var_data <- data[[i]]
    
    # Berechnung der Modalwert
    modalwert <- moda(var_data)
    
    # Berechnung der relativen Haeufigkeit
    rel_h <- table(var_data) / length(var_data)
    
    # Speichern der berechneten Statistiken in die Ergebnismatrix
    result[i, 1] <- var_name
    result[i, 2] <- length(var_data)
    result[i, 3] <- sum(is.na(var_data))
    result[i, 4] <- modalwert
    result[i, 5] <- rel_h[as.character(modalwert)] 
    # Nur die relative Haeufigkeit des Modalwerts speichern
  }
  
  # Rückgabe der Ergebnismatrix als Dataframe
  return(as.data.frame(result))
}

#a-iii

# kat_stat -  Funktion zur Berechnung bivariater deskriptiver Statistiken fuer
#             den Zusammenhang zweier kategorialer Variablen
# 
# Eingabe: 
# data1     - Vektor der Auspraegungen der ersten kategorialen Variable
# data2     - Vektor der Auspraegungen der zweiten kategorialen Variable
#
# Ausgabe:
# result    - benannter Vektor mit Cramers V und dem normierten
#             Kontingenzkoeffizienen nach Pearson


kat_stat <- function(data1, data2) {
  c(Cramer = CramersV(data1, data2), normKont = normKontingenz(data1, data2))
}

#a-iv

# Funktion zur Analyse des Zusammenhangs zwischen einer metrischen und einer 
#dichotomen Variable 
analyzeMetricDichotomous <- function(metricVar, dichotomousVar) {
  
  # Vorbereitung der Daten mit der Helfer-Funktion
  preparedData <- prepareDataForAnalysis(metricVar, dichotomousVar)
  metricVar <- preparedData$metricVar
  dichotomousVar <- preparedData$dichotomousVar
  
  # Berechnung der Mittelwerte für jede Gruppe der dichotomen Variable
  means <- tapply(metricVar, dichotomousVar, mean, na.rm = TRUE)
  
  # Durchführung eines t-Tests
  t.testResult <- t.test(metricVar ~ dichotomousVar)
  
  # Berechnung der Standardabweichungen für jede Gruppe
  sds <- tapply(metricVar, dichotomousVar, sd, na.rm = TRUE)
  
  # Ausgabe der Ergebnisse
  list(
    Means = means,
    StandardDeviations = sds,
    TTest = t.testResult
  )
}

#

#* Diese Funktion visualisiert alle kategoriellen Variablen eines gegebenen 
#* Dataframes.
#* "dataframe" enthält den Dataframe in welchem die Daten gespeichert sind,
#* "a" und "b" geben an die viele Plots pro Reihe/Spalte am Ende angezeigt
#* werden sollen. Um alle Plots einzeln zu bekommen beide auf 1 setzen.

create_categorical_plot <- function(dataframe, a = 1, b = 1) {
  
  n <- length(dataframe[,1])
  categorical_vars <- sapply(dataframe, is.factor)
  
  par(mfrow = c(a, b))
  
  # Plot für jede kategoriale Variable erstellen
  for (col in names(dataframe)[categorical_vars]) {
    cat_levels <- levels(dataframe[[col]])
    cat_counts <- table(dataframe[[col]])/n
    
    # Balkendiagramm erstellen
    barplot(cat_counts, main = paste("Barplot for", col),
            xlab = col, ylab = "Frequency", col = rainbow(length(cat_levels)))
  }
}
