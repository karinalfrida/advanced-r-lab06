#' Brute force search for the knapsack problem, storing all combinations
#'
#' Same as \code{brute_force_knapsack()}, but saves every combination in a
#' \code{data.frame} and picks the best one at the end.
#'
#' @inheritParams brute_force_knapsack
#'
#' @returns A list with \code{value} and \code{elements}, as in
#'   \code{brute_force_knapsack()}.
#'
#' @export
brute_force_knapsack_df <- function(x,W) {
    # Validate inputs
     stopifnot(
    "`x` must be a data.frame." =
      is.data.frame(x),
    "`x` must have columns named `v` and `w`." =
      all(c("v", "w") %in% names(x)),
    "Columns `v` and `w` must be numeric." =
      is.numeric(x$v) & is.numeric(x$w),
    "Columns `v` and `w` must not contain missing values." =
      !anyNA(x$v) & !anyNA(x$w),
    "Columns `v` and `w` must only contain positive values." =
      all(x$v > 0) & all(x$w > 0),
    "`W` must be a positive number." =
      is.numeric(W) & length(W) == 1 & !is.na(W) & W > 0
  )

  # n items
  n <- nrow(x)

  # Extract the item weights and values as plain vectors
  weights <- x$w
  values <- x$v

  # data.frame with one row per combination, filled in the loop
  combinations <- data.frame(id     = 0:(2^n - 1),
                             weight = 0,
                             value  = 0)

  # Generate all possible combinations of items
  for (i in 0:(2^n - 1)) {
    # Logical combination in binary representation ex: {000, 001, ..., 111} for n = 3
    combination <- as.logical(intToBits(i)[seq_len(n)])
    combinations$weight[i + 1] <- sum(weights[combination])   # sum all TRUE values
    combinations$value[i + 1]  <- sum(values[combination])    # sum all TRUE values
  }

  # Keep the valid combinations and pick the one with the highest value
  valid <- combinations[combinations$weight <= W, ]
  best  <- valid[which.max(valid$value), ]

  # Turn the id of the best combination back into item indices
  best_elements <- which(as.logical(intToBits(best$id)[seq_len(n)]))

  # Return the result in a list
  list(value = round(best$value), elements = best_elements)
}
