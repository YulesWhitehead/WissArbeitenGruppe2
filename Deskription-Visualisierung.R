attach(data)

str(data)

metr_stat(data[,5:8])

kateg_stat(data[,c(2:4,9:12)])

# a-iii Zusammenhang zwischen zwei kategorialen Variablen (z.B. Geschlecht und Überlebensstatus)
zusammenhang_kat_var <- kat_stat(data$Sex, data$Survived)

# a-iv Zusammenhang zwischen einer metrischen und einer dichotomen Variable (z.B. Alter und Überlebensstatus)
zusammenhang_met_dich <- analyzeMetricDichotomous(data$Age, data$Survived)
