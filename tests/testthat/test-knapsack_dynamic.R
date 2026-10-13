test_that("knapsack_dynamic rejects a negative W", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  expect_error(knapsack_dynamic(x = items, W = -10),
               "`W` must be a positive number.", fixed = TRUE)
})

test_that("knapsack_dynamic returns the optimal value and items", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  res <- knapsack_dynamic(x = items, W = 10)

  expect_equal(res$value, 90)
  expect_equal(res$items, c(2, 4))
})

test_that("knapsack_dynamic solution stays within capacity and matches brute_force_knapsack", {
  items <- data.frame(w = c(23, 31, 29, 44, 53, 38, 63, 85),
                      v = c(92, 57, 49, 68, 60, 43, 67, 84))
  W <- 165

  res <- knapsack_dynamic(x = items, W = W)

  expect_lte(sum(items$w[res$items]), W)                          # expect less than or equal to W
  expect_equal(res$value, sum(items$v[res$items]))                # expect value to match sum of selected items

  # Dynamic programming is exact, so it should find the same optimum as brute force
  expect_equal(res$value, brute_force_knapsack(x = items, W = W)$value)  # expect same value as brute force
})
