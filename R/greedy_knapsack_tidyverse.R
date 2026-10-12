#' Greedy heuristic for the knapsack problem (tidyverse)
#'
#' Approximates the solution to the 0/1 knapsack problem by sorting the items
#' by value per unit of weight (\code{v / w}) and adding them in that order
#' until the next item no longer fits. The result is not guaranteed to be
#' optimal, but the running time is \eqn{O(n \log n)}, dominated by the
#' sorting algorithm. This version is written as a \pkg{dplyr} pipeline.
#'
#' @param x A \code{data.frame} with exactly two numeric columns: \code{v}
#'   (the value of each item) and \code{w} (the weight of each item). All
#'   values must be positive and non-missing.
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
#' greedy_knapsack_tidyverse(x = items, W = 10)
greedy_knapsack_tidyverse <- function(x, W) {
  # Validate inputs
     stopifnot(
    "`x` must be a data.frame." =
      is.data.frame(x),
    "`x` must have exactly two columns named `v` and `w`." =
      ncol(x) == 2 & all(c("v", "w") %in% names(x)),
    "Columns `v` and `w` must be numeric." =
      is.numeric(x$v) & is.numeric(x$w),
    "Columns `v` and `w` must not contain missing values." =
      !anyNA(x$v) & !anyNA(x$w),
    "Columns `v` and `w` must only contain positive values." =
      all(x$v > 0) & all(x$w > 0),
    "`W` must be a positive number." =
      is.numeric(W) & length(W) == 1 && !is.na(W) & W > 0
  )

  selected <- x |>
    dplyr::mutate(
      item = dplyr::row_number(),
      ratio = v / w
    ) |>
    dplyr::arrange(dplyr::desc(ratio)) |>
    dplyr::mutate(running_weight = cumsum(w)) |>
    dplyr::filter(running_weight <= W)

  selected_items <- selected$item
  total_value    <- sum(selected$v)

  return(list(value = round(total_value), elements = selected_items))
}
