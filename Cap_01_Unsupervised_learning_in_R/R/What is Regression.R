# Datacamp R - Supervised Learning in R Regression

# Capítulo 1: O que é Regressão? Neste capítulo, apresentamos o conceito de regressão do ponto de vista do aprendizado de máquina. 
# Apresentaremos o método de regressão fundamental: regressão linear. 
# Mostraremos como ajustar um modelo de regressão linear e fazer previsões a partir do modelo.

# 1.1: Identifique as tarefas de regressão
# De uma perspectiva de aprendizado de máquina, o termo regressão geralmente abrange a previsão de valores contínuos. 
# Estatisticamente, essas previsões são o valor esperado, ou o valor médio que se observaria para os valores de entrada fornecidos.

unemployment<-readRDS("unemployment.rds")
summary(unemployment)

# Define a formula to express female_unemployment as a function of male_unemployment
fmla <- female_unemployment ~ male_unemployment
# Print it
fmla

# Use the formula to fit a model: unemployment_model
unemployment_model <- lm(fmla, data = unemployment)
# Print it
unemployment_model

# 1.3: Examining a model

# 1.3: Examinando um modelo Vejamos o modelo desemprego_modelo que você acabou de criar. 
# Há uma variedade de maneiras diferentes de examinar um modelo; cada maneira fornece informações diferentes. 
# Usaremos summary(), broom::glance() e sigr::wrapFTest(). 
# Instruções 100 XP O objeto desemprego_modelo está em sua área de trabalho. 
# Imprima desemprego_model novamente. Quais informações ele informa? Chame summary() em desemprego_modelo.
# Além dos valores de coeficiente, você obtém erros padrão nas estimativas de coeficiente e algumas métricas de qualidade de ajuste, como R-quadrado. 
# Chame glance() no modelo para ver as métricas de desempenho em um quadro de dados ordenado. 
# Você pode combinar as informações de summary() com as colunas de glance()? 
# Agora chame wrapFTest() no modelo para ver o R-quadrado novamente. Esconder

# broom and sigr are already loaded in your workspace
# Print unemployment_model
install.packages("nlme")
install.packages("broom")
install.packages("sigr")

library(nlme)
library(broom)
library(sigr)
unemployment_model

# Call summary() on unemployment_model to get more details
summary(unemployment_model)

# Call glance() on unemployment_model to see the details in a tidier form
glance

# Call wrapFTest() on unemployment_model to see the most relevant details
wrapFTest(unemployment_model)

# 1.4: Predicting from the unemployment model

# Neste exercício, você usará seu modelo de desemprego desemprego_modelo para fazer previsões a partir dos dados de desemprego e comparar as taxas de desemprego femininas previstas com as taxas de desemprego femininas observadas reais nos dados de treinamento, desemprego. 
# Você também usará seu modelo para prever os novos dados em novas taxas, que consiste em apenas uma observação, onde o desemprego masculino é de 5%. 
# A interface predict() para modelos lm tem o formato predict(model, newdata) 
# Você usará o pacote ggplot2 para fazer os gráficos, então você adicionará a coluna de previsão ao quadro de dados de desemprego. 
# Você plotará o resultado versus a previsão e os comparará com a linha que representa as previsões perfeitas (ou seja, quando o resultado é igual ao valor previsto). 
# O comando ggplot2 para traçar um gráfico de dispersão de dframeoutcomeversusdframepred (pred no eixo x, resultado no eixo y).
# Juntamente com uma linha azul onde resultado == pred é o seguinte: ggplot(dframe, aes(x = pred, y = result )) + geom_point() + geom_abline(color = “blue”) Instruções 100 XP Os objetos desemprego, desemprego_modelo e novas taxas estão em sua área de trabalho. Use predict() para prever as taxas de desemprego feminino a partir dos dados de desemprego. Atribua-o a uma nova coluna: previsão. Use o comando library() para carregar o pacote ggplot2. Use ggplot() para comparar as previsões com as taxas de desemprego reais. Coloque as previsões no eixo x. Quão próximos estão os resultados da linha de previsão perfeita? Use as novas taxas do quadro de dados para prever a taxa esperada de desemprego feminino quando o desemprego masculino for de 5%.
# Atribua a resposta à variável pred e imprima-a.

newrates<-read.csv("new_rates.csv")
# unemployment is in your workspace
summary(unemployment)

# newrates is in your workspace
newrates

# Predict female unemployment in the unemployment data set
unemployment$prediction <-  predict(unemployment_model)
# load the ggplot2 package
library(ggplot2)

# Make a plot to compare predictions to actual (prediction on x axis)
ggplot(unemployment, aes(x = prediction, y = female_unemployment)) + 
  geom_point() +
  geom_abline(color = "blue")

# Predict female unemployment rate when male unemployment is 5%
pred <- predict(unemployment_model, newdata = newrates)
# Print it
pred

# 1.5: Multivariate linear regression (Part 1)

# Neste exercício, você trabalhará com o conjunto de dados de pressão arterial (Fonte) e modelará a pressão_sangue em função do peso e da idade. 
# Instruções 100 XP O quadro de dados da pressão arterial está na área de trabalho. 
# Defina uma fórmula que expresse a pressão sanguínea explicitamente em função da idade e do peso. 
# Atribua a fórmula à variável fmla e imprima-a. 
# Use fmla para ajustar um modelo linear para prever blood_pressure de idade e peso no conjunto de dados de pressão arterial. 
# Chame o modelo bloodpressure_model. 
# Imprima o modelo e chame summary() nele. 
# A pressão arterial aumenta ou diminui com a idade? Com peso?

# bloodpressure is in the workspace
bloodpressure<-readRDS("bloodpressure.rds")
summary(bloodpressure)

# Create the formula and print it
fmla <- blood_pressure~age+weight
fmla

# Fit the model: bloodpressure_model
bloodpressure_model <- lm(fmla,data=bloodpressure)

# Print bloodpressure_model and call summary() 
bloodpressure_model

summary(bloodpressure_model)

# 1.6: Multivariate linear regression (Part 2)

# Agora você fará previsões usando o modelo de pressão arterial bloodpressure_model que você encaixou no exercício anterior.
# Você também comparará as previsões com os resultados graficamente. ggplot2 já está carregado em sua área de trabalho. Lembre-se de que o comando plot assume a forma:
  # ggplot(dframe, aes(x = pred, y = resultado)) + geom_point() + geom_abline(color = “blue”) `

#Instruções

# 100 EXP

# Os objetos bloodpressure e bloodpressure_model estão na área de trabalho.
# Use predict() para prever a pressão arterial no conjunto de dados de pressão arterial. 
# Atribua as previsões à previsão de coluna.
# Compare graficamente as previsões com as pressões sanguíneas reais. Coloque as previsões no eixo x. 
# Quão próximos estão os resultados da linha de previsão perfeita?

# bloodpressure is in your workspace
summary(bloodpressure)

# bloodpressure_model is in your workspace
bloodpressure_model

# predict blood pressure using bloodpressure_model :prediction
bloodpressure$prediction <- predict(bloodpressure_model)
# plot the results
ggplot(bloodpressure, aes(x = prediction, y = blood_pressure))+ 
  geom_point() +
  geom_abline(color = "blue")

