library(dplyr)

knapsack_brute_force_KA <- function(x,W){
  stopifnot("x must be a data frame" =is.data.frame(x),
            "columns names v and w must be present in the data frame" =
              all(c("v", "w") %in% names(testdf)),
            "Only positive values are allowed in c and v" = all((x$c>0), (x$w > 0)))
  n = nrow(x)

  knapsack_packed <-
    tibble(rows=2^n) |>
    mutate(bits = purrr::map(row_number() - 1,
                      \(x) as.integer(intToBits(x))[1:4]))

   # mutate(bits = row_number() |>intToBits() |> as.integer )

  # bits <-
  #   vector(length = n) |>
  #   intToBits()

  return(knapsack_packed)
}


bits =
  8 |>
  intToBits() |>
  as.integer() |>
  (\(x) x[1:n])() |>
  vector()
