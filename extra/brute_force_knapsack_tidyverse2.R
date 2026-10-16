library(dplyr)

#' knapsack problem solver, brute force version
#' using tidyverse functions (and sorting to find the best combination)
#'
#' @param x A \code{data.frame} with positive, non-missing numeric columns
#'   \code{v} (item values) and \code{w} (item weights).
#'
#' @param W A single positive number giving the weight capacity of the
#'   knapsack.
#'
#' @param fast A logical value. If \code{TRUE}, the function will use a C++.
#'   Default to \code{FALSE}
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
brute_force_knapsack_tidyverse2 <- function(x,W, fast = FALSE){
  stopifnot(
    "`x` must be a data.frame." =
      is.data.frame(x),
    "`x` must have columns named `v` and `w`." =
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


  knapsack_packed_1 <-
    dplyr::tibble(nr = 0:(2^n - 1)) |>
    dplyr::mutate(combination = purrr::map(nr, \(i) as.logical(intToBits(i)[1:n])))

  knapsack_packed_2 <-
    knapsack_packed_1|>
    dplyr::mutate(
      weight = purrr::map_dbl(combination, \(selected) sum(weights[selected])),
      value = purrr::map_dbl(combination, \(selected) sum(values[selected]))
    )

  knapsack_packed_3 <-
    knapsack_packed_2|>
    dplyr::filter(weight <= W)

  knapsack_packed_4 <-
    knapsack_packed_3|>
    dplyr::arrange(desc(value)) |>
    head(1)

  knapsack_packed_5 <-
    knapsack_packed_4|>
    dplyr::select(combination, value) |>
    (function(result) list(
      value = round(result$value[[1]]),
      elements = which(result$combination[[1]])
    ))()

  return(knapsack_packed_5)
}
