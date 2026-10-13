test_that("brute_force_knapsack_tidyverse rejects a negative W", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  expect_error(brute_force_knapsack_tidyverse(x = items, W = -10),
               "`W` must be a positive number.", fixed = TRUE)
})

test_that("brute_force_knapsack_tidyverse returns the optimal value and elements", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  res <- brute_force_knapsack_tidyverse(x = items, W = 10)

  expect_equal(res$value, 90)
  expect_equal(res$elements, c(2, 4))
})

test_that("brute_force_knapsack_tidyverse solution stays within capacity and matches brute_force_knapsack", {
  items <- data.frame(w = c(23, 31, 29, 44, 53, 38, 63, 85),
                      v = c(92, 57, 49, 68, 60, 43, 67, 84))
  W <- 165

  res <- brute_force_knapsack_tidyverse(x = items, W = W)

  expect_lte(sum(items$w[res$elements]), W)                       # expect less than or equal to W
  expect_equal(res$value, round(sum(items$v[res$elements])))      # expect value to match sum of selected items

  # Should give the same result as the original brute force solution
  expect_equal(res, brute_force_knapsack(x = items, W = W))        # expect same value and elements
})
