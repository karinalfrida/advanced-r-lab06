#include <Rcpp.h>
#include <cstdint>
using namespace Rcpp;

// Make this function callable from R through Rcpp
// [[Rcpp::export]]
List knapsack_cpp(SEXP x_input, SEXP W_input) {

  // Validate inputs 
  if (!Rf_inherits(x_input, "data.frame")) {
    stop("`x` must be a data.frame.");
  }
  DataFrame x(x_input);

  if (!x.containsElementNamed("v") || !x.containsElementNamed("w")) {
    stop("`x` must have columns named `v` and `w`.");
  }

  // Equivalent to is.numeric() in R: integer (not factor) or double
  auto is_numeric = [](SEXP s) {
    return (TYPEOF(s) == INTSXP && !Rf_isFactor(s)) || TYPEOF(s) == REALSXP;
  };

  if (!is_numeric(x["v"]) || !is_numeric(x["w"])) {
    stop("Columns `v` and `w` must be numeric.");
  }

  // Extract the item weights and values as vectors
  NumericVector weights = x["w"];
  NumericVector values  = x["v"];

  if (is_true(any(is_na(values))) || is_true(any(is_na(weights)))) {
    stop("Columns `v` and `w` must not contain missing values.");
  }

  if (!is_true(all(values > 0)) || !is_true(all(weights > 0))) {
    stop("Columns `v` and `w` must only contain positive values.");
  }

  if (!is_numeric(W_input) || Rf_length(W_input) != 1 ||
      ISNAN(Rf_asReal(W_input)) || !(Rf_asReal(W_input) > 0)) {
    stop("`W` must be a positive number.");
  }




  double W = Rf_asReal(W_input);

  // n items
  int n = x.nrows();

  // Variables to keep track of the best solution
  double best_value = 0;
  IntegerVector best_elements(0);

  // Number of possible combinations: 2^n
  std::uint64_t number_of_combinations = std::uint64_t(1) << n;

  // Generate all possible combinations of items
  for (std::uint64_t i = 0; i < number_of_combinations; i++) {

    // Logical combination in binary representation ex: {000, 001, ..., 111} for n = 3
    LogicalVector combination(n);
    for (int j = 0; j < n; j++) {
      combination[j] = ((i >> j) & 1) != 0;
    }
    NumericVector selected_weights = weights[combination];
    NumericVector selected_values  = values[combination];
    double total_weight = sum(selected_weights);   // sum all TRUE values
    double total_value  = sum(selected_values);    // sum all TRUE values

    // Check if the combination is valid and better than the best found so far
    if (total_weight <= W && total_value > best_value) {
      best_value = total_value;
      // Equivalent to which(combination): R row indices start at 1
      IntegerVector row_indices = seq_len(n);
      best_elements = row_indices[combination];
    }
  }

  // Return the result in a list, using R's rounding convention
  return List::create(
    Named("value")    = R::fround(best_value, 0),
    Named("elements") = best_elements
  );
}