#' Brute force search for the knapsack problem
#'
#' @param x A \code{data.frame} with exactly two numeric columns: \code{v}
#'   (the value of each item) and \code{w} (the weight of each item). All
#'   values must be positive and non-missing.
#' @param W A single positive number giving the weight capacity of the
#'   knapsack.
#'
#' @returns A list with two elements:
#'   \describe{
#'     \item{\code{value}}{The maximum total value of the knapsack, rounded to
#'       the nearest integer.}
#'     \item{\code{elements}}{An integer vector with the row indices of
#'       \code{x} for the items in the optimal knapsack.}
#'   }
#'
#' @references 
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#' @export
#'
#' @examples
#' # Four items. The best knapsack with capacity 10 is items 2 and 4
#' # (total weight 4 + 3 = 7, total value 40 + 50 = 90).
#' items <- data.frame(v = c(10, 40, 30, 50),
#'                     w = c(5, 4, 6, 3))
#' brute_force_knapsack(x = items, W = 10)
brute_force_knapsack <- function(x,W) {
    # Validate inputs
     stopifnot(
    "`x` must be a data.frame." =
      is.data.frame(x),
    "`x` must have exactly two columns named `v` and `w`." =
      ncol(x) == 2 && all(c("v", "w") %in% names(x)),
    "Columns `v` and `w` must be numeric." =
      is.numeric(x$v) && is.numeric(x$w),
    "Columns `v` and `w` must not contain missing values." =
      !anyNA(x$v) && !anyNA(x$w),
    "Columns `v` and `w` must only contain positive values." =
      all(x$v > 0) && all(x$w > 0),
    "`W` must be a positive number." =
      is.numeric(W) && length(W) == 1 && !is.na(W) && W > 0
  )

  # n items
  n <- nrow(x)

  # Extract the item weights and values as plain vectors
  weights <- x$w
  values <- x$v

  # variables to keep track of the best solution
  best_value    <- 0
  best_elements <- integer(0)

  # Generate all possible combinations of items
  for (i in 0:(2^n - 1)) {
    # Logical combination in binary representation ex: {000, 001, ..., 111} for n = 3
    combination <- as.logical(intToBits(i)[seq_len(n)])
    total_weight <- sum(weights[combination])   # sum all TRUE values
    total_value  <- sum(values[combination])    # sum all TRUE values

    # Check if the combination is valid and better than the best found so far
    if (total_weight <= W && total_value > best_value) {
      best_value    <- total_value
      best_elements <- which(combination)
    }
  }

  # Return the result in a list
  list(value = round(best_value), elements = best_elements)
}
