library(bbmle)
lm(mpg ~ wt, data= mtcars)

y = mtcars$mpg
x = mtcars$wt

mle = function(a, b){
  resid = y - (a * x) - b
  ll = dnorm(resid, 0, 1)
  return(-sum(log(ll)))
}

mle2(mle, start = list(a=0, b=0 ))

#############
# Example 1 #
#############
set.seed(1001)

N = 1000
x = rnorm(N, mean = 3, sd = 2)

mean(x)
sd(x)

# formulate the log-likeihood function
LL = function(mu, sigma) {
  R = dnorm(x, mu, sigma)
  -sum(log(R))
}

library(stats4)

mle(LL, start = list(mu = 1, sigma =1))


#############
# Example 2 #
#############
set.seed(1)
times = 10
# Flip coin A 10 times per round, a total of 10 rounds
# p(A | head) is 0.6
coin_A_sample = rbinom(100, times, 0.95)
hist(coin_A_sample)
library("bbmle");
LL = function(prob, mu, sigma) {
  resid = dbinom(coin_A_sample, times, prob)
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  ll = dnorm(resid, mu, sigma);
  #
  # Sum the log likelihoods for all of the data points
  #
  return(-sum(log(ll)));
}
mle2(LL, start = list(prob = 1, mu = 0, sigma = 1), method="Nelder-Mead");


#############
# Example 3 #
#############
# Maximium likelihood Estimation example
# @author Yihuang Kang

# Use bbmle package
library("bbmle");

# Simple Linear regression parameter estimation

data(mtcars);
lm_m = lm(data = mtcars, formula= mpg~wt);
# lm(formula = mpg ~ wt, data = mtcars)
#
# Coefficients:
#   (Intercept)           wt
# 37.285       -5.344

LL = function(beta0, beta1) {
  
  # Find residuals
  #
  resid = mtcars$mpg - (mtcars$wt * beta1) - beta0;
  #
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  ll = dnorm(resid, 0, 1);
  #
  # Sum the log likelihoods for all of the data points
  #
  return(-sum(log(ll)));
}
mle2(LL, start = list(beta0 = 0, beta1 = 0), method="Nelder-Mead")