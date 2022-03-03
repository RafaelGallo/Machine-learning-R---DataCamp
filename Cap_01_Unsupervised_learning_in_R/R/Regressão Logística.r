# Regressão Logística

# Construindo modelos de regressão logística simples
# Examine o conjunto de dados para identificar possíveis variáveis "Independentes"
str(doadores)

# Explorar a variável dependente
table(donors$donated)

# Construir o modelo regressão logística
donation_model <- glm(doado ~ bad_address + interest_religion + interest_veterans, dados = doadores, família = "binomial")

# Resuma os resultados do modelo
summary(donation_model)

# Estimar a probabilidade de doação
donors$donation_prob <- predict(donation_model, type = "response")
donors

# Encontre a probabilidade de doação da perspectiva média
mean(donors$donated)

# Carrega o pacote pROC
library(pROC)

# Cria uma curva ROC
ROC <- roc(donors$donated, donors$donation_prob)
ROC

# Plote a curva ROC
plot(ROC, col = "blue")

# Calcular a área sob a curva (AUC) 
auc(ROC)

# Codificando recursos categóricos
# Converter a classificação de riqueza em um fator
donors$wealth_levels <- factor(donors$wealth_rating, levels = c(0, 1, 2, 3), labels = c("Unknown", "Low", "Medium", "High"))

# Use relevel() para alterar a categoria de referência
donors$wealth_levels <- relevel(donors$wealth_levels, ref = "Medium")

# Veja como nossa codificação de fatores afeta o modelo
summary(glm(donated ~ wealth_levels, data = donors, family = "binomial"))

# Encontre a idade média entre os valores não omissos
summary(donors$age)


# Imputar valores de idade ausentes com a idade média
donors$imputed_age <- ifelse(is.na(donors$age), round(mean(donors$age, na.rm = TRUE), 2), donors$age)

# Indicador de valor ausente para a idade
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

# Carrega o pacote pROC
library(pROC)

# Cria uma curva ROC
ROC <- roc(donors$donated, donors$donation_prob)
ROC

# Plote a curva ROC
plot(ROC, col = "blue")

# Calcular a área sob a curva (AUC) 
auc(ROC)