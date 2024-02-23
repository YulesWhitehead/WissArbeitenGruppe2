## Ließt die Datei unverarbeitet ein

data <- read.csv("titanic.csv")

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

## Faktorisierung von "Pclass" (als ordered Faktor)

data$Pclass <- ordered(data$Pclass, 3:1)

## Funktion zum imputieren der fehlenden Werte

imp <- function(x, anrede) {
  un <- unique(anrede)
  n <- length(un)
  m <- vector("numeric", n)
  
  for (i in 1:n) {
    m[i] <- mean(x[anrede == un[i]], na.rm = TRUE)
  }
  
  l <- length(x)
  
  for (i in 1:l) {
    if (is.na(x[i])) {
      temp <- match(anrede[i], un)
      x[i] <- m[temp]
    }
  }
  
  return(x)
}

## Anwenden der Funktion

data$Age <- imp(data$Age, data$Anrede)

## Funktion zum extrahieren der relevanten Daten aus dem Cabin Vektor

extract <- function(x) {
  n <- length(x)
  side <- vector("character", n)
  deck <- vector("character", n)
  
  for (i in 1:n) {
    if (x[i] == "") {
      x[i] <- NA
      side[i] <- NA
      deck[i] <- NA
    }
    else if (!grepl(" ", x[i])){
      deck[i] <- substring(x[i], 1, 1)
      side[i] <- ifelse((as.numeric(substring(x[i], 2)) %% 2) == 0, "Backboard", "Starboard")
    }
    else {
      temp <- strsplit(x[i], " ")[[1]]
      tempn <- length(temp)
      tempd <- vector("character", tempn)
      temps <- vector("character", tempn)
      for (j in 1:tempn) {
        tempd[j] <- substring(temp[j], 1, 1)
        temps[j] <- ifelse((as.numeric(substring(temp[j], 2)) %% 2) == 0, "Backboard", "Starboard")
      }
      side[i] <- paste(temps, collapse = " ")
      deck[i] <- paste(tempd, collapse = " ")
    }
  }
  
  return(data.frame(Side = side, Deck = deck))
}

## Anwendung der Funktion

temp <- extract(data$Cabin)

data$Side <- temp$Side
data$Deck <- temp$Deck

rm(temp)

## Faktorisierung von "Side" und "Deck"
data$Side <- factor(data$Side, 0:1, c("Backboard", "Starboard"))
data$Deck <- factor(data$Deck, 0:6, c("A", "B", "C", "D", "E", "F", "G"))

## Entfernen der nicht mehr benötigten Variablen

remove <- c("PassengerID", "Name", "Ticket", "Cabin")

for (col in remove) {
  data[[col]] <- NULL
}

rm(remove)
rm(col)

## Speichern der Daten zum späteren benutzen

save(data, file = "titanic.RData")

