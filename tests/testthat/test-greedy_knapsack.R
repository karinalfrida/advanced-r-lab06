suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))

# Test suites provided by course
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(greedy_knapsack("hej", 3500))
  expect_error(greedy_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  st <- system.time(gk <- greedy_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 192647)
  
  gk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 270290)
})


# Out test suites
test_that("greedy_knapsack rejects missing values in x", {
  items <- data.frame(v = c(10, NA, 30, 50),
                      w = c(5, 4, 6, 3))

  expect_error(greedy_knapsack(x = items, W = 10),
               "Columns `v` and `w` must not contain missing values.", fixed = TRUE)
})

test_that("greedy_knapsack returns the expected value and elements", {
  items <- data.frame(v = c(10, 40, 30, 50),
                      w = c(5, 4, 6, 3))

  res <- greedy_knapsack(x = items, W = 10)

  expect_equal(res$value, 90)
  expect_equal(res$elements, c(4, 2))
})

test_that("greedy_knapsack adds items by value per weight and stays within capacity", {
  items <- data.frame(w = c(23, 31, 29, 44, 53, 38, 63, 85),
                      v = c(92, 57, 49, 68, 60, 43, 67, 84))
  W <- 165

  res <- greedy_knapsack(x = items, W = W)
  ratio <- items$v[res$elements] / items$w[res$elements]

  expect_lte(sum(items$w[res$elements]), W)   # expect less than or equal to W
  expect_false(is.unsorted(rev(ratio)))       # expect items added in decreasing v / w order

  # expect less than or equal to brute force solution
  expect_lte(res$value, brute_force_knapsack(x = items, W = W)$value)
})
