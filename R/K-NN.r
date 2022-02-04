# Curso machine learning - KNN
# k-Nearest Neighbors (kNN)


# Instalação das bibliotecas
install.packages("tidyverse")
install.packages("naivebayes")

# Bibliotecas
library(tidyverse)
library(class)
library(data.table)

# Dados
location <- read_csv("https://assets.datacamp.com/production/course_2906/datasets/locations.csv")
location

# Visualizando os cinco primeiros dados
head(location)

# Visualizando os cinco ultimos dados
tail(location)

#
where9am <- location %>%
  filter(hour == 9, hourtype == "morning")
head(location)

# Computing Probabilities

# Compute P(A) 
p_A <- nrow(subset(where9am, location == "office")) / 91
p_A

# Compute P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / 91
p_B

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, where9am$location == "office" & where9am$daytype == "weekday")) / 91
p_AB

# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
p_A_given_B

# Understanding Dependent Events

########### Naive Bayes ########### 

library(naivebayes)

thursday9am <- where9am %>%
  filter(weekday == "thursday") 

saturday9am <- where9am %>%
  filter(weekday == "saturday")

# Load the naivebayes package

library(naivebayes)

# Model prediction
locmodel <- naive_bayes(location ~ daytype, data = where9am)
locmodel

# Predict Thursday's 9am location
predict(locmodel, thursday9am)
predict

# Predict Saturdays's 9am location
predict(locmodel, saturday9am)

# Examining “raw” probabilities
print(locmodel)

# Obtain the predicted probabilities for Thursday at 9am
predict(locmodel, thursday9am , type = "prob")

# Obtain the predicted probabilities for Saturday at 9am
predict(locmodel, saturday9am, type = "prob")

# Location model
locations <- location

weekday_afternoon <- locations %>%
  filter(daytype == "weekday", hourtype == "afternoon")

weekday_evening <- locations %>%
  filter(daytype == "weekday", hourtype == "evening")

# Build a NB model of location
locmodel <- naive_bayes(location ~ daytype + hourtype, data = locations)

# Predict Brett's location on a weekday afternoon
predict(locmodel, weekday_afternoon)

# Predict Brett's location on a weekday evening
predict(locmodel, weekday_evening)

# Preparing for unforeseen circumstances
weekend_afternoon <- locations %>%
  filter(daytype == "weekend", hourtype == "afternoon")

# Observe the predicted probabilities for a weekend afternoon
predict(locmodel, weekend_afternoon, type = "prob")

# Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location ~ daytype + hourtype, data = locations, laplace = 1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2, weekend_afternoon, type = "prob")


# 11 Entendendo a Correção de Laplace

# Bom pessoal, por padrão a função naive_bayes() não tem correção de laplace. 
# Mas deixa o risco de que alguns resultados potenciais possam ser previstos como impossíveis.


# 12 Aplicando Naive Bayes a outros problemas

# Aqui, somos abordados com a pergunta: de que outras maneiras podemos aplicar Naive Bayes?
# Naive Bayes tende a funcionar bem onde as informações de vários atributos precisam ser consideradas ao mesmo tempo. 
# Esta analogia é semelhante à forma como um médico verifica os sintomas e executa o teste para que ele possa diagnosticar você se você tiver essa determinada doença ou tentar avaliar as chances 
# De um determinado time que você gerencia em um Football Manager, exceto de vencer outro time em seu liga (eu sei, eu sei)
# Naive Bayes também pode ser usado para classificar dados de texto se um e-mail é spam ou o número de vezes que eu posto “AAAAAAAAAAAAAAAAAAAAAAAAAA” ou invoco o nome de Emile Heskey e Adebayo Akinfenwa em minhas postagens do Facebook, pode ter minha conta do FB espancada pelo modificações
# Uma consequência disso é que cada preditor usado em um modelo Naive Bayes normalmente compreende um conjunto de categorias. As coisas numéricas como tempo, idade, dinheiro, entalpia e tudo mais não podem ser usadas como estão sem saber mais sobre as propriedades dos dados.



# 12.1 Binning de dados numéricos para Naive Bayes

# Eu sei. Levei 6 reproduções do vídeo para eu descobrir. mas é uma técnica simples que divide um conjunto de números em “caixas” com base em certas categorias não numéricas.
# Por exemplo, durante o Draft Combine da AFL (Australian Football League), olheiros, recrutadores ou como eles chamam os caras que fazem parte do departamento.
# De recrutamento de cada clube da AFL, vão querer dar uma olhada nos dados de desempenho dos potenciais recrutas.
# Em cada dos testes na colheitadeira (ou seja, Teste de Bipe, Contra-relógio de 3 km, Teste de Agilidade, Corrida e Saltos Verticais em Pé). 
# Depois de obter todos os resultados, eles agregarão todos os dados numéricos com base nos resultados desses testes e os agruparão. 
# Uma determinada categoria para ver não apenas o potencial, mas também prever sua colocação potencial no rascunho real.



# 13 Manipulando preditores numéricos

# Entre as ff: escolhas - Valores de idade registrados como “criança” ou “categorias adultas”
# Coordenadas geográficas registradas em regiões geográficas
# Pontuações de teste divididas em quatro grupos por percentil
# Renda Valores padronizados para seguir uma curva normal.
# Eu vou com a parte de valores de renda, pois não há indicação de que a transformação desses valores de renda criará um conjunto de categorias