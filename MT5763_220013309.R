# MT5763_22013309
# Individual Assignment 2

# Include libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)

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
  prob
}

# Bootstrapping to derive the sampling distribution of Pr(X > Y)
# Initialisation
# For reproducibility
# set.seed(555)
# Sample size
n_repeat <- 1000
# Pre-allocate memory for storing results
boot_res <- rep(NA, n_repeat)
prob_res <- rep(NA, n_repeat)
# Loop across all samples
my_bts <- function(n_repeat) {
  for (i in seq(n_repeat)) {
  prob_res[i] <- my_mc(n)
  }
  # Resample with replacement
  boot_res <- sample(x = prob_res, size = n_repeat, replace = TRUE)
  # Store results
  boot_res
}
# Save data to plot
my_bts_result <- my_bts(n_repeat)
# Plot the bootstrap results
hist(my_bts_result)

# Bootstrap variance changes when sample size for Monte Carlo simulation is
# 500, 5000, 20000. Minimum recommended - 5k, max recommended - 20k.
n <- 500
my_bts_500 <- my_bts(n_repeat)
ggplot() + 
  aes(my_bts_500) + 
  geom_histogram(colour = "black", fill = "white")

n <- 5000
my_bts_5000 <- my_bts(n_repeat)
ggplot() + 
  aes(my_bts_5000) + 
  geom_histogram(colour = "black", fill = "white")

n <- 20000
my_bts_20000 <- my_bts(n_repeat)
ggplot() + 
  aes(my_bts_20000) + 
  geom_histogram(colour = "black", fill = "white")

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
losses <- 0
n_reps <- 100

# NOT WORKINGGGGGGGGGGGGGGGGGGGGGGGGGGGG
tournament <- function() {
  while(!wins == w || !losses == l) {
  outcome <- sample(x = samp, size = 1, replace = TRUE)
  if (outcome == 0) {
    losses <- losses + 1
  }
  else {
    wins <- wins + 1
  }
  games <- wins + losses
  return(games)
  }
}


# Plot how the total number of matches played (i.e. wins + losses) varies as a 
# function of p.
hist(tournament(n_rep))










