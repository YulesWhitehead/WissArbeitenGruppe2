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

## Extrahieren der einzigartigen Anreden

unique(data$Anrede)
# [1] "Mr"           "Mrs"          "Miss"         "Master"       "Don"         
# [6] "Rev"          "Dr"           "Mme"          "Ms"           "Major"       
# [11] "Lady"         "Sir"          "Mlle"         "Col"          "Capt"        
# [16] "the Countess" "Jonkheer" 

## Zusammenfassen gleichbedeutender Anreden
comp <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] == "Miss") {
      x[i] <- "Ms"
    }
    else if (x[i] == "Mile") {
      x[i] <- "Ms"
    }
    else if (x[i] == "Mme") {
      x[i] <- "Mrs"
    }
    else if (x[i] == "Don") {
      x[i] <- "Mr"
    }
  }
  return(x)
}

data$Anrede <- comp(data$Anrede)
## Die anderen Anreden sind inhaltlich nicht exakt gleich und es ist zu
## erwarten das die betroffenen Personen sich jeweils in wichtigen 
## Attributen unterscheide (Durchschnittsalter, etc)

## Faktorisierung von "Survived"

data$Survived <- factor(data$Survived, 0:1, c("No", "Yes"))

## Faktorisierung von "Sex"

data$Sex <- factor(data$Sex, c("male", "female"), c("Male", "Female"))

## Faktorisierung von "Embarked"

data$Embarked <- factor(data$Embarked, c("C", "Q", "S"), c("Cherbourg", "Queenstown", "Southampton"))
