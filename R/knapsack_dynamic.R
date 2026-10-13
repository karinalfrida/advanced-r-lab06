#' knapsack problem solver, dynamic programming version
#' <
#'
#' @param x A \code{data.frame} with positive, non-missing numeric columns
#'   \code{v} (item values) and \code{w} (item weights).
#' @param W A single positive number giving the weight capacity of the
#'   knapsack.
#'
#' @returns A list with two elements:
#'   \describe{
#'     \item{\code{value}}{The maximum total value of the knapsack, rounded to
#'       the nearest integer.}
#'     \item{\code{items}}{An integer vector with the row indices of
#'       \code{x} for the items in the optimal knapsack.}
#'   }
#'
#'#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm>}
#' @export
#'
knapsack_dynamic <- function(x,W){

  # pack_knapsack_recursive <- function(i,j){
  #
  # }

  stopifnot(
    "`x` must be a data.frame." =
      is.data.frame(x),
    "`x` must have columns named `v` and `w`." =
      all(c("v", "w") %in% names(x)),
    "Columns `v` and `w` must be numeric." =
      is.numeric(x$v) & is.numeric(x$w),
    "Columns `v` and `w` must not contain missing values." =
      !anyNA(x[, c("v", "w")]),
    "Columns `v` and `w` must only contain finite, positive values." =
      all((x$v>0), (x$w > 0), is.finite(x$v), is.finite(x$v)),
    "`W` must be a positive number." =
      is.numeric(W) & length(W) == 1 && !is.na(W) & W > 0
  )

  # n items
  n <- nrow(x)

  # row and column for zero objects/capacity
  M <- matrix(0, nrow = n + 1, ncol = W + 1)

  for (i in seq_len(n)) {
    for (w in 0:W) {

      # row i+1 in matrix corresponds to object i in x
      if (x$w[i] > w) {
        M[i + 1, w + 1] <- M[i, w + 1]
      } else {
        exclude <- M[i, w + 1]
        include <- x$v[i] + M[i, w - x$w[i] + 1]

        M[i + 1, w + 1] <- max(exclude, include)
      }
    }
  }

  best_value <- M[n+1, W+1]

  # trace back to find which items are selected
  items_selected <- integer(0)
  for (i in n:1) {

    if (M[i + 1, w + 1] != M[i, w + 1]) {

      # object i was included
      items_selected <- c(i, items_selected)

      # subtract its weight
      w <- w - x$w[i]
    }
  }

  return(list(value = best_value, items = items_selected))


}
