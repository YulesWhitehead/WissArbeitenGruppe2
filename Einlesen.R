read.csv("titanic.csv")
## Eine Funktion zum extrahieren der Anreden

anrede <- function(x) {
  n <- length(x)
  an <- vector(mode = "character", length = n)
  for (i in 1:n) {
    temp1 <- regexpr(", ", x[i]) + 1
    temp2 <- regexpr("\\.", x[i])
    an[i] <- substr(x[i], temp1 + 1, temp2 - 1)
  }
  return(an)
}
# Anwendung auf die Daten und erstellung eines neuen Vektors für die Anreden
data$Anrede <- anrede(data$Name)
