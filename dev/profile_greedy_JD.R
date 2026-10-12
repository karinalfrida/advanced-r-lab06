# Profiling the greedy knapsack functions with profvis


library(profvis)

# Load the functions
source("R/random_numbers_for_knapsack.R", keep.source = TRUE)
source("R/greedy_knapsack.R", keep.source = TRUE)
source("R/greedy_knapsack_tidyverse.R", keep.source = TRUE)

# Test data
knapsack_objects <- suppressWarnings(random_numbers_for_knapsack(n = 1e6))
W <- 3500


n_reps <- 20


# Vectorised version
p_vectorised <- profvis({
  for (i in seq_len(n_reps)) {
    greedy_knapsack(x = knapsack_objects, W = W)
  }
})
print(p_vectorised)


# dplyr version
p_dplyr <- profvis({
  for (i in seq_len(n_reps)) {
    greedy_knapsack_tidyverse(x = knapsack_objects, W = W)
  }
})
print(p_dplyr)


