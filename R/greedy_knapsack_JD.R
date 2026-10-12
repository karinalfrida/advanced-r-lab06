

greedy_knapsack_vectorised <- function(x, W) {
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


# Version 3: pipe-oriented, working on the data.frame --------------------------
greedy_knapsack_dplyr <- function(x, W) {
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
