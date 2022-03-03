# Supervised Learning - Classification (Chapter 2 - Understanding Bayesian Methods)

# library
library(tidyverse)
library(class)
library(data.table)
library(naivebayes)

# Dataset
location <- read_csv("https://assets.datacamp.com/production/course_2906/datasets/locations.csv")

where9am <- location %>%
  filter(hour == 9, hourtype == "morning")

head(location)
head(where9am)

# Compute P(A) 
p_A <- nrow(subset(where9am, location == "office")) / 91

# Compute P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / 91

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, where9am$location == "office" & where9am$daytype == "weekday")) / 91

# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
p_A_given_B

thursday9am <- where9am %>%
  filter(weekday == "thursday") 

saturday9am <- where9am %>%
  filter(weekday == "saturday")

# Load the naivebayes package
library(naivebayes)

# Build the location prediction model
locmodel <- naive_bayes(location ~ daytype, data = where9am)

# Predict Thursday's 9am location
predict(locmodel, thursday9am)

# Predict Saturdays's 9am location
predict(locmodel, saturday9am)

# Examine the location prediction model
print(locmodel)

# Obtain the predicted probabilities for Thursday at 9am
predict(locmodel, thursday9am , type = "prob")

# Obtain the predicted probabilities for Saturday at 9am
predict(locmodel, saturday9am, type = "prob")

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

weekend_afternoon <- locations %>%
  filter(daytype == "weekend", hourtype == "afternoon")

# Observe the predicted probabilities for a weekend afternoon
predict(locmodel, weekend_afternoon, type = "prob")

# Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location ~ daytype + hourtype, data = locations, laplace = 1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2, weekend_afternoon, type = "prob")

