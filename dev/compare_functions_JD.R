# Wrapper for comparing two functions that are meant to do the same thing



library(bench)
library(profvis)

compare_functions <- function(fun_a, fun_b, args,
                              names = c("a", "b"),
                              min_iterations = 10,
                              check = TRUE,
                              profile = FALSE,
                              profile_reps = 20) {

  # Benchmark both functions using the same arguments.
  # read ?beanch::mark for more details on the arguments.
  timings <- bench::mark(
    a = do.call(fun_a, args), # "Do call function with these arguments"
    b = do.call(fun_b, args),
    min_iterations = min_iterations,
    check = check,
    filter_gc = FALSE
  )

  # Same table, but relative to the fastest function.
  # So the fastest is 1 and everything else is relative to that.
  relative <- summary(timings, relative = TRUE, filter_gc = FALSE)

  # Optional profiling, Same thing as above, but with profvis.
  # This will open a flame graph in the viewer.
  profiles <- NULL
  if (profile) {
    profiles <- lapply(list(fun_a, fun_b), function(f) {
      profvis::profvis({
        for (i in seq_len(profile_reps)) do.call(f, args)
      })
    })
    names(profiles) <- names
  }

  # Return a list with timings, relative timings, and profiles (if requested)
  list(timings  = timings[, c("expression", "min", "median", "itr/sec",
                              "mem_alloc", "n_itr")],
       relative = relative[, c("expression", "min", "median", "itr/sec",
                               "mem_alloc")],
       profiles = profiles)
}


# Example: greedy knapsack vs tidyverse greedy knapsack

devtools::load_all()

knapsack_objects <- suppressWarnings(random_numbers_for_knapsack(n = 1e6))

result <- compare_functions(
  fun_a = greedy_knapsack,
  fun_b = greedy_knapsack_tidyverse,
  args = list(x = knapsack_objects, W = 3500),
  names = c("base", "tidyverse"),
  profile = TRUE
)

result$timings
result$relative

# Opens the flame graphs in the viewer
print(result$profiles$base)
print(result$profiles$tidyverse)
