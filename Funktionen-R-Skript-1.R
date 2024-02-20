# Laden der benötigten Pakete
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
  result <- matrix(NA, nrow = length(data), ncol = 15)
  colnames(result) <- c("Variable", "Anzahl", "Minimum", "Maximum", "Spannweite",
                        "Median", "Erste Quartil", "Letzte Quartil", 
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
    result[i, 3] <- Min
    result[i, 4] <- Max
    result[i, 5] <- Max - Min
    result[i, 6] <- median(var_data)
    result[i, 7] <- q1
    result[i, 8] <- q2
    result[i, 9] <- q2 - q1
    result[i, 10] <- Mean
    result[i, 11] <- var(var_data)
    result[i, 12] <- Sd
    result[i, 13] <- Sd / Mean
    result[i, 14] <- skewness(var_data)
    result[i, 15] <- kurtosis(var_data)
  }
  
  # Rueckgabe der Ergebnismatrix als Dataframe
  return(as.data.frame(result))
}


#a-ii

# Die Funktion bestimmt den Modalwert (am haeufigsten vorkommender Wert)
# einer kategorialen Variable
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# metr_stat - Funktion zur Berechnung deskriptiver Statistiken fuer kategoriale
#             Variablen
# 
# Eingabe: 
# data      - Datensatz, fuer den die Statistiken berechnet werden sollen
#
# Ausgabe:
# result    - Ergebnismatrix als Dataframe mit allen berechneten Statistiken

kateg_stat <- function(data){
  # Erstellen einer leeren Ergebnismatrix
  result <- matrix(NA, nrow = length(data), ncol = 3)
  colnames(result) <- c("Variable", "Anzahl", "Modalwert")
  
  # Iteration über die Variablen im Datensatz
  for(i in 1:length(data)) {
    var_name <- names(data)[i]  
    var_data <- data[[i]]
    
    # Speichern der berechneten Statistiken in die Ergebnismatrix
    result[i, 1] <- var_name
    result[i, 2] <- length(var_data)
    result[i, 3] <- moda(var_data)
  }
  
  # Rueckgabe der Ergebnismatrix als Dataframe
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
