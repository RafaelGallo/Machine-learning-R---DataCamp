# Putting it all together with a case study

# Data
url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

# Download the data: wisc.df
wisc.df <- read.csv(url)
str(wisc.df)

# Convert the features of the data: wisc.data
wisc.data <- as.matrix(wisc.df[, 3:32])
str(wisc.data)

head(wisc.data)

# Set the row names of wisc.data
row.names(wisc.data) <- wisc.df$id
head(wisc.data)

# Create diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")
diagnosis

# Exploratory data analysis
str(wisc.data)
colnames(wisc.data)
str_match(colnames(wisc.data), "_mean")
table(diagnosis)

# Performing PCA
# Check column means and standard deviations
colMeans(wisc.data)
apply(wisc.data, 2, sd)

# Execute PCA, scaling if appropriate: wisc.pr
wisc.pr <- prcomp(wisc.data, scale = TRUE)

# Look at summary of results
summary(wisc.pr)
summary(wisc.pr)

# Create a biplot of wisc.pr
biplot(wisc.pr)

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")

# Do additional data exploration of your choosing below (optional)
plot(wisc.pr$x[, c(2, 3)], col = (diagnosis + 1), 
     xlab = "PC2", ylab = "PC3")

# Variance explained
# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var <- wisc.pr$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Communicating PCA results
wisc.pr$rotation[1:10,1:2]

# PCA review and next steps

# Scale the wisc.data data: data.scaled
head(wisc.data)

data.scaled <- scale(wisc.data)
head(data.scaled)

# Hierarchical clustering of case data
# Calculate the (Euclidean) distances: data.dist
data.dist <- dist(data.scaled)

# Create a hierarchical clustering model: wisc.hclust
wisc.hclust <- hclust(data.dist, method = "complete")

# Results of hierarchical clustering
plot(wisc.hclust)

# Selecting number of clusters
# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters <- cutree(wisc.hclust, k = 4)

# Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters, diagnosis)

# count out of place observations based on cluster 
# basically just summing the row mins here
sum(apply(table(wisc.hclust.clusters, diagnosis), 1, min))

# k-means clustering and comparing results

# Create a k-means model on wisc.data: wisc.km
head(wisc.data)

# Create a k-means model on wisc.data: wisc.km
head(wisc.data)
wisc.km <- kmeans(scale(wisc.data), centers = 2, nstart = 20)

# Compare k-means to actual diagnoses
table(wisc.km$cluster, diagnosis)
sum(apply(table(wisc.km$cluster, diagnosis), 1, min))

# Compare k-means to hierarchical clustering
table(wisc.hclust.clusters, wisc.km$cluster)
sum(apply(table(wisc.hclust.clusters, wisc.km$cluster), 1, min))

# Clustering on PCA results
# Create a hierarchical clustering model: wisc.pr.hclust
summary(wisc.pr)

wisc.pr.hclust <- hclust(dist(wisc.pr$x[, 1:7]), method = "complete")

# Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters <- cutree(wisc.pr.hclust, k = 4)

# Compare to actual diagnoses
t <- table(wisc.pr.hclust.clusters, diagnosis)
t

sum(apply(t, 1, min))

# Compare to k-means and hierarchical
t <- table(wisc.hclust.clusters, diagnosis)
t

sum(apply(t, 1, min))

t <- table(wisc.km$cluster, diagnosis)
t

sum(apply(t, 1, min))
