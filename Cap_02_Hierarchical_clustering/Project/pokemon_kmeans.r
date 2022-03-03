### Project K-Means - Pokemon

library(readr)
library(readr)
pokemon <- read_csv("Machine learning - R/Unsupervised Learning in R/Cap_02_Hierarchical_clustering/pokemon_data.csv")
head(pokemon )

tail(pokemon)

dim(pokemon)

pokemon <- pokemon_raw %>% select(6:11)
head(pokemon)

# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters
k <- 4

# Build model with k clusters: km.out
km.pokemon <- kmeans(pokemon, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.pokemon

# Plot of Defense vs. Speed by cluster membership
plot(pokemon[, c("Defense", "Speed")],
     col = km.pokemon$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")

