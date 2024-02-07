library(psych)
library(e1071)

metr_stat <- function(data) {
  result <- matrix(NA, nrow = length(data), ncol = 15)
  colnames(result) <- c("Variable", "Anzahl", "Minimum", "Maximum", "Spannweite",
                        "Median", "Erste Quartil", "Letzte Quartil", 
                        "Interquartilsabstand", "Arithm. Mittel", 
                        "Varianz", "Standardabw.", "Variationskoef.",
                        "Schiefe", "Kurtosis")
  
  for(i in 1:length(data)) {
    var_name <- names(data)[i]  
    var_data <- data[[i]]
    Max <- max(var_data)
    Min <- min(var_data)
    Mean <- mean(var_data)
    Sd <- sd(var_data)
    q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    q2 <- quantile(var_data, 0.75, na.rm = TRUE)
    
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
  
  return(as.data.frame(result))
}

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

kateg_stat <- function(data){
  result <- matrix(NA, nrow = length(data), ncol = 3)
  colnames(result) <- c("Variable", "Anzahl", "Modalwert")
  
  for(i in 1:length(data)) {
    var_name <- names(data)[i]  
    var_data <- data[[i]]
    
    result[i, 1] <- var_name
    result[i, 2] <- length(var_data)
    result[i, 3] <- moda(var_data)
  }
  
  return(as.data.frame(result))
}
