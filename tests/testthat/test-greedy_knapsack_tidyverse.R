suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))

# Test suites provided by course
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- greedy_knapsack_tidyverse(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(greedy_knapsack_tidyverse("hej", 3500))
  expect_error(greedy_knapsack_tidyverse(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- greedy_knapsack_tidyverse(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack_tidyverse(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack_tidyverse(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack_tidyverse(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  st <- system.time(gk <- greedy_knapsack_tidyverse(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- greedy_knapsack_tidyverse(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 192647)
  
  gk <- greedy_knapsack_tidyverse(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 270290)
})

# Our test suites

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
