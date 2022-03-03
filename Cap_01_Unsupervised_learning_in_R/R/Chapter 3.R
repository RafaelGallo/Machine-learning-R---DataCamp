# Chapter 3
# 3.1: Examining the structure of categorical inputs

# Para este exercício, você chamará model.matrix() para examinar como R representa dados com entradas categóricas e numéricas para modelagem. As flores do conjunto de dados (derivadas do pacote Sleuth3) são carregadas em seu espaço de trabalho. Possui as seguintes colunas:

# Flores: o número médio de flores em uma planta de meadowfoam Intensidade: 
# A intensidade de um tratamento de luz aplicado à planta Tempo: Uma variável categórica - quando (atrasado ou precoce) 
# No ciclo de vida o tratamento de luz ocorreu O objetivo final é prever flores como uma função de Tempo e Intensidade.

# Call str on flowers to see the types of each column
flowers<-read.csv("flowers.csv")
str(flowers)

# Use unique() to see how many possible values Time takes
unique(flowers$Time)

# Build a formula to express Flowers as a function of Intensity and Time: fmla. Print it
(fmla <- as.formula("Flowers ~ Intensity + Time"))

# Use fmla and model.matrix to see how the data is represented for modeling
mmat <- model.matrix(fmla, data = flowers)

# Examine the first 20 lines of flowers
head(flowers, n=20)

# Examine the first 20 lines of mmat
head(mmat, n=20)

# 3.3: Modeling with categorical inputs

# Para este exercício, você ajustará um modelo linear aos dados de flores, para prever flores em função do tempo e da intensidade.
# A fórmula de modelo fmla que você criou no exercício anterior ainda está em sua área de trabalho, assim como a matriz de modelo mmat.

# Instruções 100 XP

# Use fmla e lm para treinar um modelo linear que prevê Flores de Intensidade e Tempo. 
# Atribua o modelo à variável flower_model.
# Use summary() para se lembrar da estrutura do mmat.

# -Use summary() para examinar o flower_model. As variáveis correspondem ao que você viu no mmat?
  
  # Use flower_model para prever o número de flores. Adicione as previsões às flores como as previsões da coluna.

# Preencha os espaços em branco para traçar previsões versus flores reais (previsões no eixo x).

# flowers in is the workspace
str(flowers)

# fmla is in the workspace
fmla

# Fit a model to predict Flowers from Intensity and Time : flower_model
flower_model <- lm(fmla,data=flowers)

# Use summary on mmat to remind yourself of its structure
summary(mmat)

# Use summary to examine flower_model 
summary(flower_model)

# Predict the number of flowers on each plant
flowers$predictions <- predict(flower_model)

# Plot predictions vs actual flowers (predictions on x-axis)
ggplot(flowers, aes(x = predictions, y = Flowers)) + 
  geom_point() +
  geom_abline(color = "blue") 

# 3.4: Modeling an interaction

# Neste exercício, você usará interações para modelar o efeito do sexo e da atividade gástrica no metabolismo do álcool.

# O álcool do quadro de dados tem colunas:
  # Metabol: a taxa de metabolismo do álcool
  # Gástrico: a taxa de atividade da álcool desidrogenase gástrica
  # Sexo: o sexo do bebedor (masculino ou feminino)

# alcohol is in the workspace
alcohol<-read.csv("alcohol.csv")
summary(alcohol)

# Create the formula with main effects only
(fmla_add <- as.formula(Metabol~Gastric+Sex))

# Create the formula with interactions
(fmla_interaction <- as.formula(Metabol~Gastric+Gastric:Sex))

# Fit the main effects only model
model_add <- lm(fmla_add,data=alcohol)

# Fit the interaction model
model_interaction <- lm(fmla_interaction,data=alcohol)

# Call summary on both models and compare
summary(model_add)
summary(model_interaction)

# 3.5: Modeling an interaction (2)

# Neste exercício, você comparará o desempenho do modelo de interação que você ajustou no exercício anterior com o desempenho de um modelo somente de efeitos principais. 
# Como esse conjunto de dados é pequeno, usaremos validação cruzada para simular fazer previsões em dados fora da amostra.
# Começará a usar o pacote dplyr para fazer cálculos.
# mutate() adiciona novas colunas a um tbl (um tipo de quadro de dados) group_by() especifica como as linhas são agrupadas em um tbl resume() calcula as estatísticas de resumo de uma coluna Você também iráusar o método collect() 
# Que recebe várias colunas e recolhe em pares chave-valor.

# alcohol is in the workspace
summary(alcohol)

# Both the formulae are in the workspace
fmla_add
fmla_interaction

# Create the splitting plan for 3-fold cross validation
set.seed(34245)  # set the seed for reproducibility
splitPlan <- kWayCrossValidation(nrow(alcohol), 3, NULL, NULL)

# Sample code: Get cross-val predictions for main-effects only model
alcohol$pred_add <- 0  # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_add <- lm(fmla_add, data = alcohol[split$train, ])
  alcohol$pred_add[split$app] <- predict(model_add, newdata = alcohol[split$app, ])
}

# Get the cross-val predictions for the model with interactions
alcohol$pred_interaction <- 0 # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_interaction <- lm(fmla_interaction, data = alcohol[split$train,])
  alcohol$pred_interaction[split$app] <- predict(model_interaction, newdata = alcohol[split$app, ])
}

# Instalando magrittr
install.packages("magrittr")

library(magrittr) # for %>% filter function
library('tidyr')

example('gather')

# Get RMSE
alcohol %>% 
  gather(key = modeltype, value = pred, pred_add, pred_interaction) %>%
  mutate(residuals = Metabol-pred) %>%      
  group_by(modeltype) %>%
  summarize(rmse = sqrt(mean(residuals^2)))

# 3.6: Relative error

# Neste exercício, você comparará o erro relativo com o erro absoluto. 
# Para fins de modelagem, definiremos o erro relativo como

# rel=(pred−y)y

# ou seja, o erro é relativo ao resultado verdadeiro. 
# Você medirá o erro relativo geral de um modelo usando o erro relativo quadrático médio da raiz:
  
  # rmserel = rel2¯−−−√ onde rel2¯ é a média de rel2.

# O conjunto de dados de exemplo (brinquedo) fdata é carregado em seu espaço de trabalho. Inclui as colunas:
  
  # y: a saída real a ser prevista por algum modelo; 
  # Imagine que é a quantidade de dinheiro que um cliente gastará em uma visita à sua loja.

# pred: as previsões de um modelo que prevê y.
# label: categórico: se y vem de uma população que faz compras pequenas ou grandes.
# Você quer saber qual modelo se sai “melhor”: aquele que prevê as pequenas compras ou aquele que prevê as grandes.

# fdata is in the workspace
fdata<-read.csv("fdata.csv")
summary(fdata)

# Examine the data: generate the summaries for the groups large and small:
fdata %>% 
  group_by(label) %>%     # group by small/large purchases
  summarize(min  = min(y),   # min of y
            mean = mean(y),   # mean of y
            max  = max(y))   # max of y

# Fill in the blanks to add error columns
fdata2 <- fdata %>% 
  group_by(label) %>%       # group by label
  mutate(residual = pred-y,  # Residual
         relerr   = residual/y)  # Relative error

# Compare the rmse and rmse.rel of the large and small groups:
fdata2 %>% 
  group_by(label) %>% 
  summarize(rmse     = sqrt(mean((pred-y)^2)),   # RMSE
            
# Plot the predictions for both groups of purchases
ggplot(fdata2, aes(x = pred, y = y, color = label)) +
  geom_point() + 
  geom_abline() +
  facet_wrap(~ label, ncol = 1, scales = "free") + ggtitle("Outcome vs prediction") 

# Root mean squared relative error
rmse.rel = sqrt(mean(((pred-y)/y)^2)))   

# 3.7: Modeling log-transformed monetary output

# Neste exercício, você praticará a modelagem da saída monetária transformada em log e, em seguida, transformará as previsões de “dinheiro logarítmico” de volta em unidades monetárias. 
# Os dados carregados em seu espaço de trabalho registram os rendimentos dos sujeitos em 2005 (Income2005).
# Bem como os resultados de vários testes de aptidão feitos pelos sujeitos em 1981:

# Arith
# Palavra
# Pará
# Matemática
# AFQT (Percentil no Teste de Qualificação das Forças Armadas)

# Os dados já foram divididos em conjuntos de treinamento e teste (income_train e income_test respectivamente) 
# E estão no workspace. Você construirá um modelo de log(rendimento) a partir das entradas.
# Em seguida, converterá log(rendimento) de volta em renda.

# Examine Income2005 in the training set
income_train<-read.csv("income_train.csv")
income_test<-read.csv("income_test.csv")
summary(income_train$Income2005)

# Write the formula for log income as a function of the tests and print it
(fmla.log <- log(Income2005)~Arith+Word+Parag+Math+AFQT)

# Fit the linear model
model.log <-  lm(fmla.log,data=income_train)

# Make predictions on income_test
income_test$logpred <- predict(model.log,newdata=income_test)
summary(income_test$logpred)

# Convert the predictions to monetary units
income_test$pred.income <- exp(income_test$logpred)
summary(income_test$pred.income)

# Plot predicted income (x axis) vs income
ggplot(income_test, aes(x = pred.income, y = Income2005)) + 
  geom_point() + 
  geom_abline(color = "blue")

# 3.8: Comparing RMSE and root-mean-squared Relative Error

# Neste exercício, você mostrará que a transformação logarítmica de uma saída monetária antes da modelagem melhora 
# O erro relativo médio (mas aumenta o RMSE) em comparação com a modelagem direta da saída monetária. 
# Você comparará os resultados de model.log do exercício anterior com um modelo (model.abs) que se ajusta diretamente à renda.
# Os conjuntos de dados income_train e income_test são carregados em sua área de trabalho, juntamente com seu modelo, model.log.

# Também na área de trabalho:
  # model.abs: um modelo que ajusta diretamente a renda aos insumos usando a fórmula

# Renda2005 ~ Arith + Word + Parágrafo + Matemática + AFQT

# fmla.abs is in the workspace
fmla.abs<-as.formula(Income2005 ~ Arith + Word + Parag + Math + AFQT)
model.abs<-lm(formula = fmla.abs, data = income_train)

# model.abs is in the workspace
summary(model.abs)

# Add predictions to the test set
income_test <- income_test %>%
  mutate(pred.absmodel = predict(model.abs, income_test),        # predictions from model.abs
         pred.logmodel = exp(predict(model.log, income_test)))   # predictions from model.log

# Gather the predictions and calculate residuals and relative error
income_long <- income_test %>% 
  gather(key = modeltype, value = pred, pred.absmodel, pred.logmodel) %>%
  mutate(residual = pred-Income2005,   # residuals
         relerr   = residual/Income2005)   # relative error

# Calculate RMSE and relative RMSE and compare
income_long %>% 
  group_by(modeltype) %>%      # group by modeltype
  summarize(rmse     = sqrt(mean(residual^2)),    # RMSE
            rmse.rel = sqrt(mean(relerr^2)))    # Root mean squared relative error

# 3.9: Input transforms: the “hockey stick”

# Neste exercício, construiremos um modelo para prever o preço a partir de uma medida do tamanho da casa (área de superfície). 
# preço interno do conjunto de dados tem as colunas:
# preço : preço da casa em unidades de $ 1.000
# tamanho: área de superfície

# Um gráfico de dispersão dos dados mostra que os dados são bastante não lineares: 
# Uma espécie de “taco de hóquei” onde o preço é bastante estável para casas menores, mas aumenta acentuadamente à medida que a casa aumenta. 
# Quadráticas e tríticas geralmente são boas formas funcionais para expressar relacionamentos semelhantes a tacos de hóquei. Observe que pode não haver uma razão “física” para que o preço esteja relacionado ao quadrado do tamanho.
# Uma quadrática é simplesmente uma aproximação de forma fechada da relação observada.

# Gráfico de dispersão
# Você ajustará um modelo para prever o preço em função do tamanho quadrado e observará seu ajuste nos dados de treinamento.
# Como ^ também é um símbolo para expressar interações, use a função I() para tratar a expressão x^2 “como está”: ou seja, como o quadrado de x em vez da interação de x consigo mesmo.

# exemploFórmula = y ~ I(x^2)

# houseprice is in the workspace
houseprice<-readRDS("houseprice.rds")
summary(houseprice)

# Create the formula for price as a function of squared size
(fmla_sqr <- price~I(size^2))

# Fit a model of price as a function of squared size (use fmla_sqr)
model_sqr <- lm(fmla_sqr, data=houseprice)

# Fit a model of price as a linear function of size
model_lin <- lm(price~size, data=houseprice)

# Make predictions and compare
houseprice %>% 
  mutate(pred_lin = predict(model_lin),       # predictions from linear model
         pred_sqr = predict(model_sqr)) %>%   # predictions from quadratic model 
  gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>% # gather the predictions
  ggplot(aes(x = size)) + 
  geom_point(aes(y = price)) +                   # actual prices
  geom_line(aes(y = pred, color = modeltype)) + # the predictions
  scale_color_brewer(palette = "Dark2")

# 3.10: Input transforms: the “hockey stick” Part (2)

# houseprice is in the workspace
summary(houseprice)

# fmla_sqr is in the workspace
fmla_sqr

# Create a splitting plan for 3-fold cross validation
set.seed(34245)  # set the seed for reproducibility
splitPlan <-  kWayCrossValidation(nrow(houseprice),3,NULL,NULL)

# Sample code: get cross-val predictions for price ~ size
houseprice$pred_lin <- 0  # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_lin <- lm(price ~ size, data = houseprice[split$train,])
  houseprice$pred_lin[split$app] <- predict(model_lin, newdata = houseprice[split$app,])
}

# Get cross-val predictions for price as a function of size^2 (use fmla_sqr)
houseprice$pred_sqr <- 0 # initialize the prediction vector
for(i in 1:3) {
  split <- splitPlan[[i]]
  model_sqr <- lm(fmla_sqr, data = houseprice[split$train, ])
  houseprice$pred_sqr[split$app] <- predict(model_sqr, newdata = houseprice[split$app, ])
}

# Gather the predictions and calculate the residuals
houseprice_long <- houseprice %>%
  gather(key = modeltype, value = pred, pred_lin, pred_sqr) %>%
  mutate(residuals = pred-price)

# Compare the cross-validated RMSE for the two models
houseprice_long %>% 
  group_by(modeltype) %>% # group by modeltype
  summarize(rmse = sqrt(mean(residuals^2)))

