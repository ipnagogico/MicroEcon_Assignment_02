rm(list = ls())

library(foreign)
library(plyr)
library(tidyverse)
library(mfx)

library(sjmisc)
library(stats)
library(haven)
library(xtable)

##############################
### Problem 1: Logit Model ###
##############################

titanic <- read.csv("titanic_train_data.csv", sep = ",", header = T)

### 1a: Estimation of polynomial model ###

# For our formula model, we want to use the poly function and therefore 
# remove observations with NAs for Age
titanic1 <- filter(titanic, !is.na(Age))
lm1 <- Survived ~ poly(Age, 2, raw = T)

# Estimating the model
log_reg1 <- glm(lm1, family = "binomial", data = titanic1)
summary(log_reg1)

### 1b: Plotting the polynomial model

ggplot(data = titanic1, aes(x = Age, y = Survived)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "glm",
    formula = y ~ poly(x,2, raw = T),
    method.args=list(family="binomial"),
    se = F
  ) +
  ylab("Esitmated Probability of Survival")

# Interleaved histogram
# getting the median for age for the two groups survived and not survived
mu1 <- ddply(titanic1, "Survived", summarise, grp.median=median(Age))

ggplot(titanic1, aes(x = Age)) +
  geom_histogram(aes(fill = as.factor(Survived), y=..density..), position = "dodge", binwidth = 5, alpha=0.6)+
  geom_vline(data=mu1, aes(xintercept=grp.median, color=Survived), size=1.5, show.legend = FALSE) +
  scale_fill_discrete(name="Survived:", breaks=c("0", "1"), labels=c("no", "yes")) +
  theme(legend.position="top") +
  labs(caption = "vertical lines show median for each group (both at 28)")

### 1c: Univariate Logit Model & Plotting

lm2 <- Survived ~ Fare
log_reg2 <- glm(lm2, family = "binomial", data = titanic)
summary(log_reg2)

plot1c <- ggplot(data = titanic, aes(x = Fare, y = Survived)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "glm",
    formula = y ~ x,
    method.args=list(family="binomial"),
    se = F
  ) +
  ylab("Esitmated Probability of Survival")

plot1c

### 1d: bootstrap

# Preliminary: We store the number of replications and observations we use in the bootstrap
reps <- 100
obs <- nrow(titanic)

# Preliminary: We create a matrix to store the coefficients from the bootstrap; each row is one
# replication, the two columns for the two coefficients
boot_coeffs  <- matrix(data = 0, nrow = reps, ncol = 2)

# obtaining the coefficients from the bootstrap
for (i in 1:reps) {
  # to make it replicable, we use set.seed to have replicable randomness
  set.seed(i)
  # We take a random sample to be used as indices for the bootstrap observations
  boot_sample <- sample(obs, obs, replace = T)
  boot_observations <- titanic[boot_sample, ]
  # We do the logistic regression on the bootstrap observations
  boot_reg <- glm(lm2, family = "binomial", data = boot_observations)
  # store the coefficients of the bootstrapped regression
  boot_coeffs[i, ] <- boot_reg$coefficients
}

# Now we want to obtain the prediction values, given the 100 bootstrap coefficients
# We create a vector of all possible fare prices in the range of the given fare prices
fare_poss = seq(min(titanic$Fare), max(titanic$Fare), by = 0.1)
# We create a matrix and append a column of 1s for multiplication further on
fare_poss_mat <- cbind(rep(1, length(fare_poss)), 
                       fare_poss)

# We calculate the linear prediction values for all possible fare values
# in all replications: beta_0 + beta_1 * Fare
lin_pred <- boot_coeffs %*% t(fare_poss_mat)
# each column represents a value for Fare, so we append these as the column names
colnames(lin_pred) <- as.character(fare_poss)
# We use those to obtain the prediction values for the logistic model
log_pred <- plogis(lin_pred)

# we now create the confidence intervals by taking the 2.5% and 97.5% quantile
# for all values of Fare, i.e. for all columns of our log_pred matrix.
# To store the value, we create a matrix where each row represents the two
# quantiles for each given value
conf <- apply(X = log_pred, MARGIN = 2, FUN = quantile, probs = c(.025, .975))
conf <- t(conf)

# for plotting purposes, we create a new data frame with the value for Fare
# and the lower and upper bound of the confidence intervals
conf_plot <- as.data.frame(cbind(fare_poss, conf))
colnames(conf_plot) <- c("Fare", "Lower", "Upper")

# we plot the confidence intervals in the new plots
plot1c +
  geom_line(data = conf_plot, aes(x = Fare, y = Lower), color = "firebrick1") +
  geom_line(data = conf_plot, aes(x = Fare, y = Upper), color = "firebrick1")

### 1e: Interpretation

# have a look at the value:
filter(conf_plot, Fare == 100)

### Problem 1 with removed outliers:

# We notice some outliers (with a fare value of >500) and create a second data frame without them and repeat the calculations 
# with them new data frame again, i.e. do the same calculations with new data
filter(titanic, Fare > 500)
titanic_no <- filter(titanic, Fare < 500)

log_reg2_no <- glm(lm2, family = "binomial", data = titanic_no)
summary(log_reg2_no)

plot1c_no <- ggplot(data = titanic_no, aes(x = Fare, y = Survived)) +
  geom_point(alpha = 0.3) +
  geom_smooth(
    method = "glm",
    formula = y ~ x,
    method.args=list(family="binomial"),
    se = F
  ) +
  ylab("Esitmated Probability of Survival")

plot1c_no

reps <- 100
obs <- nrow(titanic_no)
boot_coeffs  <- matrix(data = 0, nrow = reps, ncol = 2)

for (i in 1:reps) {
  set.seed(i)
  boot_sample <- sample(obs, obs, replace = T)
  boot_observations <- titanic_no[boot_sample, ]
  boot_reg <- glm(lm2, family = "binomial", data = boot_observations)
  boot_coeffs[i, ] <- boot_reg$coefficients
}

fare_poss = seq(min(titanic_no$Fare), max(titanic_no$Fare), by = 0.1)
fare_poss_mat <- cbind(rep(1, length(fare_poss)), 
                       fare_poss)
lin_pred <- boot_coeffs %*% t(fare_poss_mat)
colnames(lin_pred) <- as.character(fare_poss)
log_pred <- plogis(lin_pred)

conf <- apply(X = log_pred, MARGIN = 2, FUN = quantile, probs = c(.025, .975))
conf <- t(conf)
conf_plot_no <- as.data.frame(cbind(fare_poss, conf))
colnames(conf_plot_no) <- c("Fare", "Lower", "Upper")

plot1c_no +
  geom_line(data = conf_plot_no, aes(x = Fare, y = Lower), color = "firebrick1") +
  geom_line(data = conf_plot_no, aes(x = Fare, y = Upper), color = "firebrick1")

filter(conf_plot_no, Fare == 100)

###############################
### Problem 2: Probit Model ###
###############################

#Preproc data:
df = as.data.frame(read_dta(file = "titanic.dta"))
#create dummy for class factor
df %<>% 
  to_dummy(class, suffix = "label") %>% 
  bind_cols(df)
colnames(df) = c("class1", "class2", "class3", "classcrew", "class", "ageadult", "man", "survived")

### 2a
Probit_Model_Estimate = glm(survived ~ class1 + class2 + class3 + ageadult  + man, family = binomial(link = "probit"), data = df)
summary(Probit_Model_Estimate )

#output latex table
xtable(summary(Probit_Model_Estimate ))

### 2b i
#Marginal Effect (with formula from Lecture)

Beta_Man = Probit_Model_Estimate[["coefficients"]]["man"]
ME = function(x) dnorm(x) * Beta_Man

x_val <- seq(-5, 5, by = .01)
ME_val <- ME(x_val)

df2b <- data.frame(cbind(x_val, ME_val))

ggplot(df2b, aes(x=x_val, y = ME_val)) +
  geom_line() +
  labs(x =bquote(x^T * beta), y = bquote(MPE[man]~(x^T~beta)))

# Calculate the domain of x'b
x_mat <- as.matrix(cbind(
  rep(1, times = nrow(df)), 
  df[, c("class1", "class2", "class3", "ageadult", "man")]
  ))

xb <- x_mat %*% Probit_Model_Estimate[["coefficients"]] 
summary(xb)

## ii
#maximum MPE for xb = 0:
ME(0)

### iii ' Effect of gender (not marginal)
DE = function(x){pnorm(x + Beta_Man) - pnorm(x) }

DE_val <- DE(x_val)
df2b2 <- data.frame(cbind(x_val, DE_val))

# Maximal discrete effect value
DE(-Beta_Man/2)
min(DE_val)

ggplot(df2b2, aes(x=x_val, y = DE_val)) +
  geom_line() +
  labs(x = bquote(z), y = bquote(DPE[Man]~(z)))

### 2c
# ADPE in case of nominal class variable
probitmfx(survived ~ class1 + class2 + class3 + ageadult  + man, 
          data = df, atmean = FALSE)

# Probit regression in case of binary class1 variable (in the first class vs not)
lm_first <- survived ~ class1 + ageadult + man

Probit_first <- glm(lm_first, family = binomial(link = "probit"), data = df)
summary(Probit_first)

# Estimated discrete probability effects
probitmfx(lm_first, data = df, atmean = FALSE)

### 2d.test Hypothesis

#perform regression with interaction terms.:
Probit_Model_Estimate_Interact = glm(survived ~ class1 + class2 + class3 + ageadult  + man + class1*man + class2*man + class3*man, family = binomial(link = "probit"), data = df)
summary(Probit_Model_Estimate_Interact )
#output latex table
xtable(summary(Probit_Model_Estimate_Interact ))

#get log likelihoods from restricted and unrestriced models:

# log likelihood from model in b (restricted)
Log_Likelihood_Restricted = logLik(Probit_Model_Estimate)
# log likelihood from model in d (unrestricted)
Log_Likelihood_Unrestricted = logLik(Probit_Model_Estimate_Interact)

#compute test-statistics
Test_stat = -2*(Log_Likelihood_Restricted -  Log_Likelihood_Unrestricted)

#compute p_value (under H0 the test stat is asymptotically Chi squared distributed with degrees of freedom = number of restrictions)
p_value = 1- pchisq(Test_stat[1], df = 3)