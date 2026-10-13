##### knapsack objects, in total 2000 #####
knapsack_objects <- random_numbers_for_knapsack(n=2000)

##### brute force #####
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
# $value
# [1] 16770
# $elements
# [1] 5 8
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
# $value
# [1] 16770
# $elements
# [1] 5 8
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
# $value
# [1] 15428
# $elements
# [1] 3 8
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
# $value
# [1] 15428
# $elements
# [1] 3 8

##### brute force "tidyverse" #####
brute_force_knapsack_tidyverse(x = knapsack_objects[1:8,], W = 3500)
# $value
# [1] 16770
# $elements
# [1] 5 8
brute_force_knapsack_tidyverse(x = knapsack_objects[1:12,], W = 3500)
# $value
# [1] 16770
# $elements
# [1] 5 8
brute_force_knapsack_tidyverse(x = knapsack_objects[1:8,], W = 2000)
# $value
# [1] 15428
# $elements
# [1] 3 8
brute_force_knapsack_tidyverse(x = knapsack_objects[1:12,], W = 2000)
# $value
# [1] 15428
# $elements
# [1] 3 8

##### greedy #####
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
# $value
# [1] 192647
# $elements
# [1] 92 574 472 80 110 537 332 117 37 776 577 288 234 255 500 794 55 290 436
# [20] 346 282 764 599 303 345 300 243 43 747 35 77 229 719 564

greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
# $value
# [1] 212337
# $elements
# [1] 92 574 472 80 110 840 537 1000 332 117 37 1197 1152 947 904
# [16] 776 577 288 1147 1131 234 255 1006 833 1176 1092 873 828 1059 500
# [31] 1090 794 1033

