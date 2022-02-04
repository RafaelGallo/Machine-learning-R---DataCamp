# Naive Bayes

library(naivebayes)

# Dados
m <- read_csv("https://assets.datacamp.com/production/course_2906/datasets/locations.csv")
m

# Visualizando os cinco primeiros dados
head(m)

# Visualizando os cinco ultimos dados
tail(m)

# Modelo Naive bayes
m <- naive_bayes(m ~ time_of_day, data = location_history)
m

# Fazendo previsões com Naive Bayes
future_location <- predict(m, future_conditions)
future_location

# Calcula P(A)
p_A <- nrow(subset(where9am, location == "office")) / nrow(where9am)

# Calcula P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / nrow(where9am)

# Calcular o P(A e B) observado
p_AB <- nrow(subset(where9am, location == "office" & daytype == "weekday")) / nrow(where9am)

# Calcule P(A | B) e imprima seu valor
p_A_given_B <- p_AB / p_B
p_A_given_B

# Modelo de previsão
locmodel <- naive_bayes(location ~ daytype, data = where9am)
locmodel

# Prever a localização das 9h de quinta-feira
predict(locmodel, newdata = saturday9am)

# O pacote 'naivebayes' é carregado na área de trabalho 
# E o 'locmodel' Naive Bayes foi construído 
# Examine o modelo de previsão de localização

locmodel

# O pacote 'naivebayes' já está carregado na área de trabalho # Construir um modelo NB de localização 
locmodel <- naive_bayes(local ~ tipo de dia + tipo de hora, dados = locais)

# Prever a localização de Brett em uma tarde de segunda a sexta 
predict(locmodel, newdata = weekday_afternoon)

# O pacote 'naivebayes' já está carregado na área de trabalho 
# O modelo de localização Naive Bayes (locmodel) já foi construído # Observe as probabilidades previstas para uma tarde de fim de semana 

predict(locmodel, newdata = week_afternoon, type = "prob")

# Observe as novas probabilidades previstas para uma tarde de fim de semana
predict(locmodel2, newdata = weekend_afternoon, type = "prob")