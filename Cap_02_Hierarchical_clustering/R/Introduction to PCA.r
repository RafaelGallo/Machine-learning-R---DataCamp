# Introduction to PCA

# Bibliotecas

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Base dados
data("iris")
head(data)

summary(iris)

# PCA
pr.iris <- prcomp(x = iris[-5], scale = F, center = T)
summary(pr.iris)
pr.iris

# Perform scaled PCA: pr.out
pr.out <- prcomp(pokemon, scale = TRUE)

# Inspect model output
summary(pr.out)

# Variability of each principal component: pr.var
pr.var <- pr.out$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

# creating a biplot
# this does not look as nice as the one he had in the video
biplot(pr.iris)
biplot(pr.iris)

# Getting proportion of variance for a scree plot
pr.var <- pr.iris$sdev^2
pve <- pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve, 
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0,1), 
     type = "b")

biplot(pr.pokemon)

# Variance explained
# Variability of each principal component: pr.var
pr.var <- pr.pokemon$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)
pve

# Visualize variance explained

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Practical issues with PCA

data(mtcars)
head(mtcars)

round(colMeans(mtcars), 2)

round(apply(mtcars, 2, sd), 2)

# grafico
pr.mtcars_no_scale <- prcomp(x = mtcars, scale = F, center = F)
pr.mtcars_scale <- prcomp(x = mtcars, scale = T, center = T)
biplot(pr.mtcars_no_scale)     

PCbiplot(pr.mtcars_scale)

# Mean of each variable
colMeans(pokemon)

# Standard deviation of each variable
apply(pokemon, 2, sd)

# PCA model with scaling: pr.with.scaling
pr.with.scaling <- prcomp(pokemon, scale = TRUE)

# PCA model without scaling: pr.without.scaling
pr.without.scaling <- prcomp(pokemon, scale = FALSE)

# Create biplots of both for comparison
biplot(pr.with.scaling)
biplot(pr.without.scaling)

