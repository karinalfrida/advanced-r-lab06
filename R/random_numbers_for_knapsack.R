#' Random number generation for the knapsack problem
#' https://en.wikipedia.org/wiki/Knapsack_problem
#'
#' @param n Integer deciding size of sample
#'
#' @returns A data frame
#'
#' @export
#'
random_numbers_for_knapsack <-  function(n = 2000){
  RNGversion(min(as.character(getRversion()),"3.5.3"))

  # Warning expected:
  # Warning in RNGkind("Mersenne-Twister", "Inversion", "Rounding"): non-uniform ’Rounding’
  # sampler used
  # ##old sampler used for backward compatibility
  # ## suppressWarnings() can be used so that the above warning is not displayed


  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  # n <- 2000
  knapsack_objects <-
    data.frame(w = sample(1:4000, size = n, replace = TRUE),
               v = stats::runif(n = n, 0, 10000))
  return(knapsack_objects)
}

