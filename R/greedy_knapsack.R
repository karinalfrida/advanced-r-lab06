#' Greedy heuristic for the knapsack problem
#'
#' Approximates the solution to the 0/1 knapsack problem by sorting the items
#' by value per unit of weight (\code{v / w}) and adding them in that order
#' until the next item no longer fits. The result is not guaranteed to be
#' optimal, but the running time is \eqn{O(n \log n)}, dominated by the
#' sorting algorithm.
#'
#' @param x A \code{data.frame} with positive, non-missing numeric columns
#'   \code{v} (item values) and \code{w} (item weights).
#' @param W A single positive number giving the weight capacity of the
#'   knapsack.
#'
#' @returns A list with two elements:
#'   \describe{
#'     \item{\code{value}}{The total value of the selected items, rounded to
#'       the nearest integer.}
#'     \item{\code{elements}}{An integer vector with the row indices of
#'       \code{x} for the selected items, in the order they were added.}
#'   }
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#' @export
#'
#' @examples
#' # Four items. Sorted by value per weight the order is 4, 2, 3, 1.
#' # Items 4 and 2 fit (total weight 3 + 4 = 7), item 3 would exceed W = 10.
#' items <- data.frame(v = c(10, 40, 30, 50),
#'                     w = c(5, 4, 6, 3))
#' greedy_knapsack(x = items, W = 10)
greedy_knapsack <- function(x, W) {
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
      is.numeric(W) & length(W) == 1 && !is.na(W) & W > 0
  )

  # Value per unit of weight for every item sorted best to worst
  ratio <- x$v / x$w
  item_order <- order(ratio, decreasing = TRUE)

  # Running total weight for the items added in that order
  running_weight <- cumsum(x$w[item_order])

  # All items less than or equal to the capacity fit in the knapsack
  fits <- running_weight <= W

  # Keep the rows that fit and add up their values
  selected_items <- item_order[fits]
  total_value    <- sum(x$v[selected_items])

  return(list(value = round(total_value), elements = selected_items))
}
