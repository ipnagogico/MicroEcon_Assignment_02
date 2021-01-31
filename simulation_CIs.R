# Confidence Interval

## Basics: Construction of confidence interval (for mu for X ~ N(mu, sigma^2)):
# define population parameter (RV is normally distributed):
mu = 10
sigma = 5

# draw a sample of size n from population distribution
n = 1000
sample <- rnorm(n, mean = mu, sd = sigma)

# estimation:
x_bar <- mean(sample)
se <- sd(sample)/sqrt(n)

ci_lower <- x_bar - qnorm(.975) * se
ci_upper <- x_bar + qnorm(.975) * se

## Correct interpretation of confidence intervall: If we draw 1000 samples,  ~95% of them will have confidence intervall that contain the population parameter mu
# Construct 1000 confidence intervals and check if CIs contain mu. Store answer in vector
ci1 <- function(mu = 10, sigma = 5, n = 1000, number_simulations = 1000) {
  contains <- numeric(number_simulations)
  
  for (i in 1:number_simulations) {
    sample <- rnorm(n, mean = mu, sd = sigma)
    x_bar <- mean(sample)
    se <- sd(sample)/sqrt(n)
    
    ci_lower <- x_bar - qnorm(.975) * se
    ci_upper <- x_bar + qnorm(.975) * se
    
    contains[i] <- ci_lower < mu & ci_upper > mu
  }
  
  print(paste("After", number_simulations, "simulations, ",round(mean(contains),3), "Confidence intervals contain the population mean mu =", mu, "with sigma =", sigma, "and sample size", n))
  return(round(mean(contains),3))
}

## call function:
ci1()

# plot the results of 100 executions of this function
plot1 <- numeric(100)
for (i in 1:100) plot1[i] <- ci1()

plot(plot1, ylim = c(0,1), type = "l")
abline(h = .95, col = "red")
# line always pretty close to 95%

## Incorrect interpretation: If we create one confidence interval and draw 1000 additional samples, their mean will be contained in the confidence interval ~95% of the times
# Construct 1 confidence intervall
ci2 <- function(mu = 10, sigma = 5, n = 1000, number_simulations = 1000) {
  sample <- rnorm(n, mean = mu, sd = sigma)
  x_bar <- mean(sample)
  se <- sd(sample)/sqrt(n)
  
  ci_lower <- x_bar - qnorm(.975) * se
  ci_upper <- x_bar + qnorm(.975) * se
  
  contains <- numeric(number_simulations)
  
  # draw new sample and check if its sample mean is contained in CI
  for (i in 1:number_simulations) {
    sample <- rnorm(n, mean = mu, sd = sigma)
    x_bar <- mean(sample)
    
    contains[i] <- ci_lower < x_bar & ci_upper > x_bar
  }
  print(paste("After", number_simulations, "simulations, ",round(mean(contains),3), "sample means are contained in the confidence intervall", ci_lower, "and ", ci_upper))
  return(round(mean(contains),3))
}

ci2()

# plot the result of 100 executions of the function
plot2 <- numeric(100)
for (i in 1:100) plot2[i] <- ci2()

plot(plot2, ylim = c(0,1), type = "l")
abline(h = .95, col = "red")

# very jittery, often far away from 95%

### draw sample of 1 and check if its value lies in the ci
# Construct 1 confidence intervall
ci3 <- function(mu = 10, sigma = 5, n = 1000, number_simulations = 1000) {
  sample <- rnorm(n, mean = mu, sd = sigma)
  x_bar <- mean(sample)
  se <- sd(sample)/sqrt(n)
  
  ci_lower <- x_bar - qnorm(.975) * se
  ci_upper <- x_bar + qnorm(.975) * se
  
  contains <- numeric(number_simulations)
  
  # draw new sample and check if its sample mean is contained in CI
  sample <- rnorm(number_simulations, mean = mu, sd = sigma)
  contains <- ci_lower < sample & ci_upper > sample
  
  print(paste("After", number_simulations, "simulations,",round(mean(contains),3), "randomly picked persons are contained in the confidence intervall", ci_lower, "and ", ci_upper))
  
  return(round(mean(contains),3))
}

ci3()

plot3 <- numeric(100)
for (i in 1:100) plot3[i] <- ci3()
plot(plot3, ylim = c(0,1), type = "l")
abline(h = .95, col = "red")
