suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))

# Test suites provided by course

set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(bfk <- brute_force_knapsack_matrix(x = knapsack_objects[1:8,], W = 3500))
  expect_named(bfk, c("value", "elements"))
})


test_that("functions rejects errounous input.", {
  expect_error(brute_force_knapsack_matrix("hej", 3500))
  expect_error(brute_force_knapsack_matrix(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  bfk <- brute_force_knapsack_matrix(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))
  
  bfk <- brute_force_knapsack_matrix(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))
  
  bfk <- brute_force_knapsack_matrix(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))
  
  bfk <- brute_force_knapsack_matrix(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))
  
  st <- system.time(bfk <- brute_force_knapsack_matrix(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})

# Our test suites

test_that("brute_force_knapsack_matrix rejects a negative W", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  expect_error(brute_force_knapsack_matrix(x = items, W = -10),
               "`W` must be a positive number.", fixed = TRUE)
})

test_that("brute_force_knapsack_matrix returns the optimal value and elements", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  res <- brute_force_knapsack_matrix(x = items, W = 10)

  expect_equal(res$value, 90)
  expect_equal(res$elements, c(2, 4))
})

test_that("brute_force_knapsack_matrix solution stays within capacity and matches brute_force_knapsack", {
  items <- data.frame(w = c(23, 31, 29, 44, 53, 38, 63, 85),
                      v = c(92, 57, 49, 68, 60, 43, 67, 84))
  W <- 165

  res <- brute_force_knapsack_matrix(x = items, W = W)

  expect_lte(sum(items$w[res$elements]), W)                       # expect less than or equal to W
  expect_equal(res$value, round(sum(items$v[res$elements])))      # expect value to match sum of selected items

  # Should give the same result as the original brute force solution
  expect_equal(res, brute_force_knapsack(x = items, W = W))        # expect same value and elements
})
