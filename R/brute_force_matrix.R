#' Brute force search for the knapsack problem, matrix
#'
#'
#' @inheritParams brute_force_knapsack
#'
#' @returns A list with \code{value} and \code{elements}, as in
#'   \code{brute_force_knapsack()}.
#'
#' @export
brute_force_knapsack_matrix <- function(x,W) {
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

  # Number of each combination: 0, 1, ..., 2^n - 1
  nr <- 0:(2^n - 1)

  # Binary matrix with one row per combination and one column per item.
  # Column j is bit j of nr, ex: {000, 001, ..., 111} for n = 3
  combinations <- sapply(seq_len(n), function(j) {
    (nr %/% 2^(j - 1)) %% 2
    })
  combinations <- matrix(combinations, ncol = n)   # ensure matrix

  # Total weight and value of every combination in one matrix product each
  total_weights <- as.vector(combinations %*% x$w)
  total_values  <- as.vector(combinations %*% x$v)

  # Make all combos higher then W set to -inf (invalid)
  total_values[total_weights > W] <- -Inf

  # Pick highest value position from all combinations
  best <- which.max(total_values)

  # Return the result in a list
  list(value    = round(total_values[best]),
       elements = which(combinations[best, ] == 1))
}
