# profiling

library(profvis)

objects_and_values <- random_numbers_for_knapsack(2000)
brute_force_knapsack(objects_and_values[1:12,], 10000)

pvis1 <-
  profvis(
    for (i in 1:10000){
    brute_force_knapsack(objects_and_values[1:12,], 1000)
    }
  )

pvis_greedy <-
    profvis(
  for (i in 1:10000){
    greedy_knapsack(objects_and_values[1:12,], 1000)
  }
)

pvis3 <-
  profvis(
    {for (i in 1:1000){
      brute_force_knapsack_tidyverse2(objects_and_values[1:12,], 10000)
    }
    },
    simplify =TRUE
  )

pvis4 <-
  profvis(
    for (i in 1:10000){
      knapsack_dynamic(objects_and_values[1:12,], 1000)
    }
  )
