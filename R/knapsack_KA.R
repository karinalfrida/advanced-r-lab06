library(dplyr)

#' knapsack problem solver, brute force version
#' using tidyverse functions (and sorting to find the best combination)
#'
#' @param x A \code{data.frame} with exactly two numeric columns: \code{v}
#'   (the value of each item) and \code{w} (the weight of each item). All
#'   values must be finite positive and non-missing.
#'
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
#' @importFrom dplyr tibble mutate select filter arrange
#' @importFrom purrr map map_dbl
#' @export
#'
#' @examples
#' # Four items. The best knapsack with capacity 10 is items 2 and 4
#' # (total weight 4 + 3 = 7, total value 40 + 50 = 90).
#' items <- data.frame(v = c(10, 40, 30, 50),
#'                     w = c(5, 4, 6, 3))
#' brute_force_knapsack(x = items, W = 10)
brute_force_knapsack_tidyverse <- function(x,W){
  stopifnot(
    "`x` must be a data.frame." =
      is.data.frame(x),
    "`x` must have two columns named `v` and `w`." =
      all(c("v", "w") %in% names(x)),
    "Columns `v` and `w` must be numeric." =
      is.numeric(x$v) & is.numeric(x$w),
    "Columns `v` and `w` must not contain missing values." =
      !anyNA(x$v) & !anyNA(x$w),
    "Columns `v` and `w` must only contain finite, positive values." =
      all((x$v>0), (x$w > 0), is.finite(x$v), is.finite(x$v)),
    "`W` must be a positive number." =
      is.numeric(W) & length(W) == 1 & !is.na(W) & W > 0
  )

  # n items
  n = nrow(x)

  # Extract the item weights and values as plain vectors
  weights <- x$w
  values <- x$v


  knapsack_packed <-
    dplyr::tibble(nr = 0:(2^n - 1)) |>
    dplyr::mutate(combination = purrr::map(nr, \(i) as.logical(intToBits(i)[1:n]))) |>
    dplyr::mutate(
      weight = purrr::map_dbl(combination, \(selected) sum(weights[selected])),
      value = purrr::map_dbl(combination, \(selected) sum(values[selected]))
    ) |>
    dplyr::filter(weight <= W) |>
    dplyr::arrange(desc(value)) |>
    head(1) |>
    dplyr::select(combination, value) |>
    (function(result) list(
      value = round(result$value[[1]]),
      elements = which(result$combination[[1]])
    ))()

  return(knapsack_packed)
}

#' knapsack problem solver, dynamic programming version
#' <
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
    "`x` must have two columns named `v` and `w`." =
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
    print(i)
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
