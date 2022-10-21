# MT5763_22013309
# Individual Assignment 2

# Include libraries
library(tidyverse)
library(ggplot2)

# Problem A
# Sample size
n <- 10000
# Pre-allocate memory for storing random samples
X <- rep(NA, n)
Y <- rep(NA, n)
prob <- NA
# For reproducibility
set.seed(777)
# Simple Monte Carlo function to calculate probability Pr(X > Y)
my_mc <- function(n) {
  # Generate samples with given boundaries
  X <- rnorm(n, mean = 4, sd = sqrt(10))
  Y <- runif(n, min = 2, max = 8)
  # Check how many x variables are greater than y variables
  # http://ditraglia.com/Econ103Public/Rtutorials/Rtutorial4.html
  prob <- sum(X > Y) / length(X + Y)
  return(prob)
}

# Bootstrapping to derive the sampling distribution of Pr(X > Y)
# Initialisation
# For reproducibility
# set.seed(555)
# Sample size
n_repeat <- 100
# Pre-allocate memory for storing results
boot_res <- rep(NA, n_repeat)
prob_res <- rep(NA, n_repeat)
# Loop across all samples
my_bts <- function(n_repeat) {
  for (i in n_repeat) {
  prob_res <- my_mc(n)
  }
  # Resample with replacement
  boot_res <- sample(x = prob_res, size = n_repeat, replace = TRUE)
  # Store results
  return(boot_res)
}
# Save data to plot
my_bts_result <- my_bts(n_repeat)
# Plot the bootstrap results
hist(my_bts_result)

# Bootstrap variance changes when sample size for Monte Carlo simulation is
# 10, 100, 1000, 10000



# ---------------------------------------------------------------------------
# Problem B
# Consider the following football tournament format: a team keeps playing until 
# they accrue 7 wins or 3 losses (whichever comes first - no draws allowed). 
# Assume a fixed win rate p ∈ [0, 1] across all rounds (they are paired at random).
w <- 7
l <- 3
samp <- c(0, 1)
games <- NA
wins <- 0
loses <- 0
n_rep <- 1000

tournament <- function(n_rep) {
  while(!wins == w || !loses == l) {
  outcome <- sample(x = samp, size = 1, replace = TRUE)
  if (outcome == 0) {
    loses <- loses + 1
  }
  else (outcome == 1) {
    wins <- wins + 1
  }
  games <- wins + loses
  }
  games
}

# Plot how the total number of matches played (i.e. wins + losses) varies as a function of p.
hist(tournament(n_rep))








