## An Expectation-Maximization (EM) algorithm example of coin flipping for students. 
# Suppose we have 2 coins (1 and 2). 
# One of them is unfair (high chance to get Head). 
# We don't know which coin is the one, but we can still guess by:
#
# 1. Flipping an unknown coin 10 times for each trial for n trials. 
# Apply EM to estimate the probability

set.seed(1);
# Generate samples

# Actual probability of getting "Head" for coin 1 and coin 2
pH = c(0.8,0.5)

# n is the number of trials/samples 
n = 200
flip_rand_prob = sample(pH, n, replace=T);
flip_rand_sample = rbinom(n, 10, flip_rand_prob); #  flip 10 times for each trial

# Tutorial flipping coin example
# flip_rand_sample = c(5, 9, 8, 4, 7)

pH_init_guess = c(0.6,0.4);# Randomly assign/guess the probabilities of getting "Head" for coin 1 and 2

pH_EM = pH_init_guess; # parameter/prob of both coins after 1 step of EM

for(i in 1:200){
  
  # E step (Expected probability)
  p_coin1 = dbinom(x = flip_rand_sample, size=10, prob=pH_EM[1]);  
  p_coin2 = dbinom(x = flip_rand_sample, size=10, prob=pH_EM[2]);
  
  posterior_prob_coin1 = p_coin1 / (p_coin1 + p_coin2);
  posterior_prob_coin2 = p_coin2 / (p_coin1 + p_coin2);
  
  # M step ()
  pH_coin1 = sum(flip_rand_sample * posterior_prob_coin1) /
    ( sum(flip_rand_sample * posterior_prob_coin1) + sum((10 - flip_rand_sample) *  posterior_prob_coin1));
  
  pH_coin2 = sum(flip_rand_sample * posterior_prob_coin2) /
    ( sum(flip_rand_sample * posterior_prob_coin2) + sum((10 - flip_rand_sample) * posterior_prob_coin2));
  
  pH_EM = c(pH_coin1,pH_coin2);
  
  cat('Probabilities to get Head for two coins = (', pH_EM, ")\n");
}
