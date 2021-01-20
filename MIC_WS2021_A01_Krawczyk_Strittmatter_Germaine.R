rm(list = ls())

library(foreign)
library(tidyverse)

titanic <- read.csv("titanic_train_data.csv", sep = ",", header = T)

### Problem 1: Logit Model ###

## 1a: Estimation of polynomial model

# For our formula model, we want to use the poly function and therefore 
# remove observations with NAs for Age
titanic1 <- filter(titanic, !is.na(Age))
model1 <- Survived ~ poly(Age, 2, raw = T)

# Estimating the model
log_reg1 <- glm(model1, family = "binomial", data = titanic1)
summary(log_reg1)

## 1b: Plotting the polynomial model
ggplot(data = titanic1, aes(x = Age, y = Survived)) +
  geom_point(alpha = 0.4) +
  geom_smooth(
    method = "glm",
    formula = y ~ poly(x,2, raw = T),
    method.args=list(family="binomial"),
    se = F
  )
# TODO: LABELING

## 1c: Univariate Logit Model & Plotting
model2 <- Survived ~ Fare
log_reg2 <- glm(model2, family = "binomial", data = titanic)

plot1c <- ggplot(data = titanic, aes(x = Fare, y = Survived)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "glm",
    formula = y ~ x,
    method.args=list(family="binomial"),
    se = F
  )

plot1c
# TODO: Labeling

## 1d: bootstrap

# We store the number of replications and observations we use in the bootstrap
reps <- 100
obs <- nrow(titanic)

# We create a matrix to store the coefficients from the bootstrap; each row is one
# replication, the two columns for the two coefficients
boot_coeffs  <- matrix(data = 0, nrow = reps, ncol = 2)

for (i in 1:reps) {
  # to make it replicable, we use set.seed to have replicable randomness
  set.seed(i)
  # We take a random sample to be used as indices for the bootstrap observations
  boot_sample <- sample(obs, obs, replace = T)
  boot_observations <- titanic[boot_sample, ]
  # We do the logistic regression on the bootstrap observations
  boot_reg <- glm(model2, family = "binomial", data = boot_observations)
  # store the coefficients of the bootstrapped regression
  boot_coeffs[i, ] <- boot_reg$coefficients
}

# create a vector of all possible fare prices in the range of the given fare prices
fare_poss = seq(min(titanic$Fare), max(titanic$Fare), by = 0.1)
# create a matrix and append a column of 1s for multiplication further on
fare_poss_mat <- cbind(rep(1, length(fare_poss)), fare_poss)

# We calculate the linear prediction values for all possible fare values
# in all replications: beta_0 + beta_1 * Fare
# each column represents a value for Fare, so we append these as the column names
lin_pred <- boot_coeffs %*% t(fare_poss_mat)
colnames(lin_pred) <- as.character(fare_poss)
# the logistic model function: pdf_logistic(lin_pred)
log_pred <- plogis(lin_pred)

# we now create the confidence intervals by taking the 2.5% and 97.5% quantile
# for all values of Fare, i.e. for all columns of our log_pred matrix.
# To store the value, we create a matrix where each row represents the two
# quantiles for each given value
conf <- matrix(0, nrow = ncol(log_pred), ncol = 2)

for (i in 1:ncol(log_pred)) {
  conf[i,] <- quantile(log_pred[,i], probs = c(.025, .975))
}  

# for plotting purposes, we create a new data frame with the value for Fare
# and the lower and upper bound of the confidence intervals
conf_plot <- as.data.frame(cbind(fare_poss, conf))
colnames(conf_plot) <- c("Fare", "Lower", "Upper")

# we plot the confidence intervals in the new plots
plot1c +
  geom_line(data = conf_plot, aes(x = Fare, y = Lower), color = "firebrick1") +
  geom_line(data = conf_plot, aes(x = Fare, y = Upper), color = "firebrick1")
# TODO: Labeling

# optional: Compare with built-in se estimate of geom_smooth function            
ggplot(data = titanic, aes(x = Fare, y = Survived)) +
  geom_point() +
  geom_smooth(
    method = "glm",
    formula = y ~ x,
    method.args=list(family="binomial"),
    se = T
  )

# 1e: Interpretation
# have a look at the value:
filter(conf_plot, Fare == 100)

# Assuming our model does indeed capture the true data-generating process,
# with a probability of 95%, the true (population) probability of having survived the sinking of the titanic,
# conditional on having paid a fare of 100 dollars, lies between ~upper~ and ~lower~

### Problem 2: Probit Model ###

titanic <- read.dta("titanic.dta")
str(titanic)
summary(titanic)


model3 <- survived ~ class + age + sex

prob_reg <- glm(model3, family = binomial(link = "probit"), data = titanic)
summary(prob_reg)
