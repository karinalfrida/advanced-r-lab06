test_that("brute_force_knapsack rejects a negative W", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  expect_error(brute_force_knapsack(x = items, W = -10),
               "`W` must be a positive number.", fixed = TRUE)
})

test_that("brute_force_knapsack returns the optimal value and elements", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  res <- brute_force_knapsack(x = items, W = 10)

  expect_equal(res$value, 90)
  expect_equal(res$elements, c(2, 4))
})

test_that("brute_force_knapsack solution stays within capacity and matches its value", {
  items <- data.frame(w = c(23, 31, 29, 44, 53, 38, 63, 85),
                      v = c(92, 57, 49, 68, 60, 43, 67, 84))
  W <- 165

  res <- brute_force_knapsack(x = items, W = W)

  expect_lte(sum(items$w[res$elements]), W)                       # expect less than or equal to W
  expect_equal(res$value, round(sum(items$v[res$elements])))      # expect value to match sum of selected items

  # The brute force solution is optimal, so it can never be worse than greedy
  expect_gte(res$value, greedy_knapsack(x = items, W = W)$value)  # expect greater than or equal to greedy solution
})
