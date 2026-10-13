stress_test_knapsack_solver <-  function (f,max_objects, W =1000, start = 1){

  knapsack_objects <- suppressWarnings(
    random_numbers_for_knapsack(n = 2000)
  )

  print(paste("Testing", deparse1(substitute(f))))

    n <- start
    while (n<= max_objects) {
      knapsack <- knapsack_objects[1:n,]
      print(n)
      failed <- tryCatch(
        {
          f(knapsack, W)
          FALSE
        },
        error = function(e) TRUE
      )

      if (failed) return(paste("Failed at , n = ", n))

      n <- n + 1
    }
    return(paste("Successful, n = ", n-1))

}
