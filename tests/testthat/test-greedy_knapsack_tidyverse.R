test_that("greedy_knapsack_tidyverse rejects non-positive values in x", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, -4, 6, 3))

  expect_error(greedy_knapsack_tidyverse(x = items, W = 10),
               "Columns `v` and `w` must only contain positive values.", fixed = TRUE)
})

test_that("greedy_knapsack_tidyverse returns the expected value and elements", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  res <- greedy_knapsack_tidyverse(x = items, W = 10)

  expect_equal(res$value, 90)
  expect_equal(res$elements, c(4, 2))
})

test_that("greedy_knapsack_tidyverse gives the same result as greedy_knapsack", {
  items <- data.frame(w = c(23, 31, 29, 44, 53, 38, 63, 85),
                      v = c(92, 57, 49, 68, 60, 43, 67, 84))

  for (W in c(50, 165, 300)) {
    res_tidy <- greedy_knapsack_tidyverse(x = items, W = W)
    res_base <- greedy_knapsack(x = items, W = W)

    expect_equal(res_tidy$value, res_base$value)          # expect same total value
    expect_equal(res_tidy$elements, res_base$elements)    # expect same items in same order
  }
})
