# Chapter 2: Training and Evaluating Regression Models

# Agora que aprendemos como ajustar modelos básicos de regressão linear, aprenderemos como avaliar o desempenho de nossos modelos. 
# Analisaremos a avaliação de um modelo graficamente e veremos duas métricas básicas para modelos de regressão. 
# Também aprenderemos como treinar um modelo que funcionará bem na natureza, não apenas em dados de treinamento. 
# Embora demonstremos essas técnicas usando regressão linear, todos esses conceitos se aplicam a modelos que se encaixam em qualquer algoritmo de regressão. 

# 2.1: Avalie graficamente o modelo de desemprego Neste exercício você avaliará graficamente o modelo de desemprego, desemprego_modelo, que você ajustou aos dados de desemprego no capítulo anterior. 

# Lembre-se de que o modelo prevê feminino_desemprego de masculino_desemprego. 
# Você plotará as previsões do modelo em relação ao desemprego feminino real; relembre o comando está no formato 

  # ggplot(dframe, aes(x = pred, y = resultado)) geom_point() geom_abline() 

# Então você calculará os resíduos: resíduos <- resultado real - resultado previsto 
# E plotará as previsões contra os resíduos. 
# O gráfico de resíduos terá uma forma ligeiramente diferente: 
# você compara os resíduos com a linha horizontal x=0 (usando geom_hline()) em vez da linha x=y. 
# O comando será fornecido. Instruções 100 XP A estrutura de dados desemprego e o modelo desemprego_modelo estão disponíveis na área de trabalho. 
# Use predict() para obter as previsões do modelo e adicioná-las ao desemprego como as previsões da coluna. 
# Previsões do gráfico (no eixo x) versus taxas reais de desemprego feminino. 
# As previsões estão perto da linha x=y? Calcule os resíduos entre as previsões e as taxas de desemprego reais. 
# Adicione esses resíduos ao desemprego como os resíduos da coluna. 
# Preencha os espaços em branco para traçar previsões (no eixo x) versus resíduos (no eixo y). 
# Isso oferece uma visão diferente das previsões do modelo em comparação com a verdade do terreno.

# unemployment is in the workspace
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Make predictions from the model
unemployment$predictions <- predict(unemployment_model)

# Fill in the blanks to plot predictions (on x-axis) versus the female_unemployment rates
ggplot(unemployment, aes(x = predictions, y = female_unemployment)) + 
  geom_point() + 
  geom_abline()

# Calculate residuals
unemployment$residuals <- unemployment$female_unemployment - unemployment$predictions

# Fill in the blanks to plot predictions (on x-axis) versus the residuals
ggplot(unemployment, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("residuals vs. linear model prediction")


# 2.2: A curva de ganho para avaliar o modelo de desemprego 
# No exercício anterior você fez previsões sobre o desemprego feminino e visualizou as previsões e os resíduos. 
# Agora, você também traçará a curva de ganho das previsões do desemprego_modelo em relação ao desemprego_feminino real usando a função WVPlots::GainCurvePlot(). 
# Para situações em que a ordem é mais importante do que valores exatos, a curva de ganho ajuda a verificar se as previsões do modelo são classificadas na mesma ordem do resultado real. 
# Chamadas para a função GainCurvePlot() se parecem com: GainCurvePlot(frame, xvar, truthvar, title) onde frame é um quadro de dados xvar e truthvar são strings que nomeiam as colunas de previsão.
# Resultado real do frame title é o título do gráfico Quando as previsões ordenar exatamente na mesma ordem, o coeficiente de Gini relativo é 1. 
# Quando o modelo ordena mal, o coeficiente de Gini relativo é próximo de zero, ou mesmo negativo. 
# Instruções 100 XP A estrutura de dados desemprego e o modelo desemprego_modelo estão na área de trabalho. 
# Carregue o pacote WVPlots. Trace a curva de ganho. Dê ao enredo o título “Modelo de desemprego”. 
# As previsões do modelo são classificadas corretamente?


# unemployment is in the workspace (with predictions)
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Load the package WVPlots
install.packages("WVPlots")

library(WVPlots)
# Plot the Gain Curve
GainCurvePlot(unemployment, "predictions", "female_unemployment", "Unemployment model")

# 2.3: Calculate RMSE

# Neste exercício, você calculará o RMSE do seu modelo de desemprego. 
# Nos exercícios de codificação anteriores, você adicionou duas colunas ao conjunto de dados de desemprego: 
# As previsões do modelo (coluna de previsões) os resíduos entre as previsões e o resultado (coluna de resíduos) 
# Você pode calcular o RMSE a partir de um vetor de resíduos, res, como: RMSE =sqrt{mean(res 2)} 
# Você quer que o RMSE seja pequeno. Quão pequeno é “pequeno”? 
# Uma heurística é comparar o RMSE com o desvio padrão do resultado. 
# Com um bom modelo, o RMSE deve ser menor. 
# Instruções 100 XP O desemprego do quadro de dados está em seu espaço de trabalho. 
# Revise os dados de desemprego do exercício anterior. 
# Por conveniência, atribua a coluna de resíduos do desemprego à variável res. 
# Calcule RMSE: quadrado res, tire sua média e, em seguida, faça a raiz quadrada. 
# Atribua isso à variável rmse e imprima. 

# Dica: você pode fazer isso em uma etapa colocando a atribuição entre parênteses: 
  # (rmse <- ___) Calcule o desvio padrão de female_unemployment 

# E atribua-o à variável sd_unemployment. 
# Imprima-o. Como o rmse do modelo se compara ao desvio padrão dos dados?

# unemployment is in the workspace
summary(unemployment)

# For convenience put the residuals in the variable res
res <- unemployment$predictions-unemployment$female_unemployment

# Calculate RMSE, assign it to the variable rmse and print it
(rmse <- sqrt(mean(res^2)))

# Calculate the standard deviation of female_unemployment and print it
(sd_unemployment <- sd(unemployment$female_unemployment))
sd_unemployment

# 2.4: Calculate R-Squared

# Agora que você calculou o RMSE das previsões do seu modelo, você examinará quão bem o modelo se ajusta aos dados: ou seja, quanta variação ele explica. 
# Você pode fazer isso usando R2.
# Suponha que y seja o resultado verdadeiro, p seja a previsão do modelo e res=y−p sejam os resíduos das previsões.
# Então a soma total dos quadrados tss (“variância total”) dos dados é: 

# tss = ∑(y−y¯)2 onde y¯ é o valor médio de y.

# A soma residual dos erros quadrados do modelo, rss é: rss = ∑res2

# R2 (R-Quadrado), a “variância explicada” pelo modelo, é então: 1 − rsstss

# Depois de calcular o R2 , você comparará o que calculou com o R2 informado por glance(). 
# olhada() retorna um quadro de dados de uma linha; 
# para um modelo de regressão linear, uma das colunas retornadas é o R2 do modelo nos dados de treinamento.

# O desemprego do quadro de dados está em seu espaço de trabalho, com as previsões de colunas e os resíduos que você calculou em um exercício anterior.

### Instruções

# 100 EXP

# O quadro de dados desemprego e o modelo desemprego_modelo estão no espaço de trabalho.

# Calcule a média feminina_desemprego e atribua-a à variável fe_mean.
# Calcule a soma total dos quadrados e atribua-a à variável tss.
# Calcule a soma dos quadrados dos resíduos e atribua-a à variável rss.
# Calcule R2. É um bom ajuste (R2 perto de 1)?
  # Use glance() para obter R2 do modelo. É o mesmo que você calculou?

# unemployment is in your workspace
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Calculate mean female_unemployment: fe_mean. Print it
(fe_mean <- mean(unemployment$female_unemployment))

# Calculate total sum of squares: tss. Print it
(tss <- sum((unemployment$female_unemployment - fe_mean)^2))

# Calculate residual sum of squares: rss. Print it
(rss <- sum((unemployment$female_unemployment - unemployment$predictions)^2))

# Calculate R-squared: rsq. Print it. Is it a good fit?
(rsq <- 1-rss/tss)

# Get R-squared from glance. Print it
(rsq_glance <- glance(unemployment_model)$r.squared)

# 2.5: Correlation and R-squared

# A correlação linear de duas variáveis, xey, mede a força da relação linear entre elas. 
# Quando x e y são respectivamente:
# os resultados de um modelo de regressão que minimiza o erro quadrado (como a regressão linear) e os resultados verdadeiros dos dados de treinamento, então o quadrado da correlação é o mesmo que R2. 
# Você verificará isso neste exercício.

# unemployment is in your workspace
summary(unemployment)

# unemployment_model is in the workspace
summary(unemployment_model)

# Get the correlation between the prediction and true outcome: rho and print it
(rho <- cor(unemployment$predictions, unemployment$female_unemployment))

# Square rho: rho2 and print it
(rho2 <- rho^2)

# Get R-squared from glance and print it
(rsq_glance <- glance(unemployment_model)$r.squared)

# 2.6: Generating a random test/train split

# Para os próximos exercícios, você usará os dados mpg do pacote ggplot2. 
# Os dados descrevem as características de várias marcas e modelos de carros de diferentes anos. 
# O objetivo é prever a eficiência de combustível da cidade a partir da eficiência de combustível da rodovia.

# Neste exercício, você dividirá mpg em um conjunto de treinamento mpg_train (75% dos dados) e um conjunto de teste mpg_test (25% dos dados). 
# Uma maneira de fazer isso é gerar uma coluna de números aleatórios uniformes entre 0 e 1, usando a função runif().
# Se você tiver um conjunto de dados dframe de tamanho N e desejar um subconjunto aleatório de tamanho aproximado de 100∗X%. 
# N (onde X está entre 0 e 1), então:
  
  # Gere um vetor de números aleatórios uniformes: gp = runif(N). dframe[gp < X,] terá o tamanho certo. 
  # dframe[gp >= X,] será o complemento.

# Instruções

# 100 EXP
# O data frame mpg está na área de trabalho.
# Use a função nrow para obter o número de linhas no quadro de dados mpg. Atribua esta contagem à variável N e imprima-a.
# Calcule sobre quantas linhas 75% de N deve ser. Atribua-o à variável alvo e imprima-o.
# Use runif() para gerar um vetor de N números aleatórios uniformes, chamado gp.
# Use gp para dividir mpg em mpg_train e mpg_test (com mpg_train contendo aproximadamente 75% dos dados).
# Use nrow() para verificar o tamanho de mpg_train e mpg_test. Eles são do tamanho certo?

# mpg is in the workspace
summary(mpg)
dim(mpg)

# Use nrow to get the number of rows in mpg (N) and print it
(N <- nrow(mpg))

# Calculate how many rows 75% of N should be and print it
# Hint: use round() to get an integer
(target <- round(0.75*N))

# Create the vector of N uniform random variables: gp
gp <- runif(N, min = 0, max = 1)
select.training<-gp<0.75

# Use gp to create the training set: mpg_train (75% of data) and mpg_test (25% of data)
mpg_train <- mpg[select.training,]
mpg_test <- mpg[!select.training,]

# Use nrow() to examine mpg_train and mpg_test
nrow(mpg_train)
nrow(mpg_test)

# 2.7: Train a model using test/train split

# Agora que você dividiu o conjunto de dados mpg em mpg_train e mpg_test, você usará mpg_train para treinar um modelo para prever a eficiência de combustível da cidade (cty) 
# A partir da eficiência de combustível da rodovia (hwy).

# Instruções

# 100 EXP
# O quadro de dados mpg_train está na área de trabalho.
# Crie uma fórmula fmla que expresse a relação cty em função de hwy. Imprima-o.
# Treine um modelo mpg_model em mpg_train para prever cty de hwy usando fmla e lm().
# Use summary() para examinar o modelo.

# mpg_train is in the workspace
summary(mpg_train)

# Create a formula to express cty as a function of hwy: fmla and print it.
(fmla <- cty~hwy)

# Now use lm() to build a model mpg_model from mpg_train that predicts cty from hwy 
mpg_model <- lm(fmla,data=mpg_train)

# Use summary() to examine the model
summary(mpg_model)

# 2.8: Evaluate a model using test/train split

# Examine the objects in the workspace
#ls.str()
# predict cty from hwy for the training set
mpg_train$pred <- predict(mpg_model)

# predict cty from hwy for the test set
mpg_test$pred <- predict(mpg_model,newdata=mpg_test)

install.packages("Metrics")
library(Metrics)

# Evaluate the rmse on both training and test data and print them
(rmse_train <- rmse(mpg_train$cty,mpg_train$pred))

(rmse_test <- rmse(mpg_test$cty,mpg_test$pred))

# Evaluate the r-squared on both training and test data.and print them
(rsq_train <- r_squared(mpg_train$pred,mpg_train$cty))
(rsq_test <- r_squared(mpg_test$pred,mpg_test$cty))
(rsq_train <- 1-sse(mpg_train$pred,mpg_train$cty)/sse(mean(mpg_train$cty),mpg_train$cty))

(rsq_test <- 1-sse(mpg_test$pred,mpg_test$cty)/sse(mean(mpg_test$cty),mpg_test$cty))

# Plot the predictions (on the x-axis) against the outcome (cty) on the test data
ggplot(mpg_test, aes(x = pred, y = cty)) + 
  geom_point() + 
  geom_abline()


# 2.9: Create a cross validation plan

# Existem várias maneiras de implementar um plano de validação cruzada de n vezes. 
# Neste exercício, você criará esse plano usando vtreat::kWayCrossValidation() e o examinará.
# kWayCrossValidation() cria um plano de validação cruzada com a seguinte chamada:
  
  # splitPlan <- kWayCrossValidation(nRows, nSplits, dframe, y) onde nRows é o número de linhas de dados 

# A serem divididos e nSplits é o número desejado de dobras de validação cruzada.
# Estritamente falando, dframe e y não são usados por kWayCrossValidation; 
# Eles estão lá para compatibilidade com outras funções de particionamento de dados vtreat. 
# Você pode definir ambos como NULL.

# Load the package vtreat
install.packages("vtreat")

library(vtreat)
# mpg is in the workspace
summary(mpg)

# Get the number of rows in mpg
nRows <- nrow(mpg)

# Implement the 3-fold cross-fold plan with vtreat
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)

# Examine the split plan
str(splitPlan)


# 2.10: Evaluate a modeling procedure using n-fold cross-validation

# Neste exercício, você usará splitPlan, o plano de validação cruzada 3 vezes do exercício anterior, para fazer previsões de um modelo que prevê mpgctyfrommpghwy.
# Se dframe for os dados de treinamento, uma maneira de adicionar uma coluna de previsões de validação cruzada ao quadro é a seguinte:
# Inicializar uma coluna de comprimento apropriado dframe$pred.cv <- 0

# k é o número de dobras

# splitPlan é o plano de validação cruzada

# for(i in 1:k) { # Obtém a ª divisão da divisão <- splitPlan[[i]]

# Construa um modelo nos dados de treinamento # desta divisão # (lm, neste caso) modelo <- lm(fmla, data = dframe[split$train,])
# fazer previsões sobre os # dados do aplicativo dessa divisão dframepred.cv[splitapp] <- predizer(model, newdata = dframe[split$app,]) }
# A validação cruzada prevê o desempenho de um modelo construído a partir de todos os dados em novos dados. Assim como na divisão teste/treinamento, para um bom procedimento de modelagem.
# O desempenho da validação cruzada e o desempenho do treinamento devem ser próximos.

# mpg is in the workspace
summary(mpg)

# splitPlan is in the workspace
str(splitPlan)

# Run the 3-fold cross validation plan from splitPlan
k <- 3 # Number of folds
mpg$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(cty~hwy, data = mpg[split$train,])
  mpg$pred.cv[split$app] <- predict(model, newdata = mpg[split$app,])
}

# Predict from a full model
mpg$pred <- predict(lm(cty ~ hwy, data = mpg))

# Get the rmse of the full model's predictions
rmse(mpg$pred, mpg$cty)

