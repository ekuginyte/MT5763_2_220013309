# MT5763_22013309
# Individual Assignment 2

# Include libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

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
  if (n <= 0 || n %% 1 != 0 || is.na(n) || 
      is.numeric(n) == FALSE || length(n) > 1) {
    stop("invalid arguments") 
  }
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
prob_res <- rep(NA)
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





