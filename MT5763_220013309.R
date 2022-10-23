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
# Input:
#  X - samples from normal distribution,
#  Y - samples from uniform distribution.
# Output:
#  prob - probability Pr(X > Y).
my_mc <- function(n) {
  # Generate samples with given boundaries
  X <- rnorm(n, mean = 4, sd = sqrt(10))
  Y <- runif(n, min = 2, max = 8)
  # Check how many x variables are greater than y variables
  # http://ditraglia.com/Econ103Public/Rtutorials/Rtutorial4.html
  prob <- sum(X > Y) / length(X + Y)
  # I wouldn't use return command normally in this place, since the Tidyverse
  # Style Guide advice against it, unless I wanted the return earlier in the 
  # function. However, I have heard this sometimes gets penalised at the 
  # university, hence, the return command at the end of the function.
  return(prob)
}

# Bootstrapping to derive the sampling distribution of Pr(X > Y)
# Sample size
n_repeat <- 1000
# Pre-allocate memory for storing results
boot_res <- rep(NA, n_repeat)
prob_res <- rep(NA, n_repeat)
# For reproducibility
set.seed(555)
# Loop across all samples
# Output:
#  boot_res - store vector of the Pr(X > Y) distribution.
my_bts <- function(n_repeat) {
  for (i in seq(n_repeat)) {
  prob_res[i] <- my_mc(n)
  }
  # Resample with replacement
  boot_res <- sample(x = prob_res, size = n_repeat, replace = TRUE)
  # Store results
  return(boot_res)
}
# Save data to plot
my_bts_result <- data_frame(my_bts(n_repeat))
# Plot the bootstrap results
hist(my_bts_result)
# FIXXXXXXXXXXXXXXXXXXXXXXXXXX -----------------------------------------
ggplot(my_bts_result) + 
  aes(x = my_bts_result[, 1], y = my_bts_result[, 2]) +
  geom_bar()
# FIXXXXXXXXXXXXXXXXXXXXXXXXXX -----------------------------------------

# Bootstrap variance changes when sample size for Monte Carlo simulation is
# 500, 5000, 20000. Minimum recommended - 5k, max recommended - 20k.
n <- 500
ggplot() + 
  aes(my_bts(n_repeat)) + 
  geom_histogram(colour = "firebrick", fill = "white", bins = 40) +
  xlab("Bootstrap Values") +
  ylab("Count") +
  ggtitle("Bootstrap Values With 500 Monte Carlo Simulations") +
  theme_minimal()

n <- 5000
ggplot() + 
  aes(my_bts(n_repeat)) + 
  geom_histogram(colour = "firebrick", fill = "white", bins = 40) +
  xlab("Bootstrap Values") +
  ylab("Count") +
  ggtitle("Bootstrap Values With 5000 Monte Carlo Simulations") +
  theme_minimal()

n <- 20000
ggplot() + 
  aes(my_bts(n_repeat)) + 
  geom_histogram(colour = "firebrick", fill = "white", bins = 40) +
  xlab("Bootstrap Values") +
  ylab("Count") +
  ggtitle("Bootstrap Values With 20000 Monte Carlo Simulations") +
  theme_minimal()

# ---------------------------------------------------------------------------
# Problem B
# Count number of games the team needs to play to win or loose the match.
# The team needs to accrue 7 wins or 3 losses (no draws). 
# Pre-allocate memory
games <- NA
wins <- 0
losses <- 0
# To check the distribution
n_reps <- 10000
# For reproducibility
set.seed(333)
# Input:
#  w - wins to finish the match,
#  l - losses to finish the match.
# Output:
#  game_n - number of games played for the match to be finished.
tournament <- function(w, l, n_reps) {
  w <- 7
  l <- 3
  # Pre-allocated memory
  game_n <- rep(NA, n_reps)
  # Looping through samples of winning (w) or loosing (l)
  for (i in 1:n_reps) {
  outcome <- sample(x = c("w", "l"), size = 3, replace = TRUE)
  # Looping while the outcome of the game reaches w or l value
  while (all(c((sum(outcome == "w") < w), (sum(outcome == "l") < l)))) {
    outcome <- c(outcome, sample(x = c("w", "l"), size = 1, replace = TRUE))
  }
  # Store number of games played when match is over
  game_n[i] <- length(outcome)
  }
  return(game_n)
}

# Plot how the total number of matches played (i.e. wins + losses) varies as a 
# function of p.
tournament_plot <- data_frame(val = tournament(w, l, n_reps)) %>%
  ggplot(., aes(val)) + 
  xlab("Number of Games Played") +
  ylab("Count") +
  ggtitle("Games Played to Finish The Match") +
  theme_minimal() +
  geom_bar(colour = "firebrick", fill = "white")










