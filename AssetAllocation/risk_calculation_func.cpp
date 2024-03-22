#include <Rcpp.h>
using namespace Rcpp;

// Helper function to get sorting indexes (0-based)
IntegerVector sort_indexes(NumericVector x) {
  IntegerVector idx = seq_along(x) - 1;
  std::sort(idx.begin(), idx.end(), [&](int i, int j){return x[i] < x[j];});
  return idx;
}

// [[Rcpp::export]]
double value_at_risk(NumericVector vec, double p_rsk_conf_level) {
  int n = vec.size();
  if (n == 1) {
    return vec[0];
  }
	
  // Sort the vector in non-decreasing order
  NumericVector sorted_vec = clone(vec).sort();
  
  // Calculate the VaR scenario index
  int var_scenarie = ceil((1 - p_rsk_conf_level) * n) - 1; // Using ceil and adjusting index
  
  // Compute and return the Value-at-Risk
  return sorted_vec[var_scenarie];
}

// [[Rcpp::export]]
double expected_shortfall(NumericVector vec, double p_rsk_conf_level) {
  int n = vec.size();
  if (n == 1) {
    return vec[0];
  }
	
  // Sort the vector in non-decreasing order
  NumericVector sorted_vec = clone(vec).sort();
  
  // Calculate the VaR scenario index
  int var_scenarie = ceil((1 - p_rsk_conf_level) * n) - 1; // Using ceil and adjusting index
  
  // Compute and return the Expected Shortfall
  return mean(sorted_vec[Range(0, var_scenarie)]);
}

// [[Rcpp::export]]
double expected_shortfall_component(NumericVector vec, double p_rsk_conf_level, IntegerVector sort_order) {
  int n = vec.size();
  if (n == 1) {
    return vec[0];
  }

  // Apply the sort order. Note that R is 1-based and C++ is 0-based
  NumericVector component_vec(n);
  for (int i = 0; i < n; ++i) {
    component_vec[i] = vec[sort_order[i] - 1]; // Adjusting since R indices start at 1
  }

  // Calculate the VaR scenario index
  int var_scenarie = ceil((1 - p_rsk_conf_level) * n) - 1; // Using ceil and adjusting index

  // Compute and return the Expected Shortfall
  return mean(component_vec[Range(0, var_scenarie)]);
}

// [[Rcpp::export]]
SEXP portfolio_es_components(NumericMatrix sim_pnl, NumericVector w, bool do_component = false, double confidence_level = 0.975) {
  int ncol = sim_pnl.ncol();
  int nrow = sim_pnl.nrow();
  NumericVector pnl_port(nrow);
  
  // Compute weighted P&L
  for (int j = 0; j < ncol; ++j) {
    pnl_port += sim_pnl(_, j) * w[j];
  }
  
  double es = expected_shortfall(pnl_port, confidence_level);
  
  if (do_component) {
    List out_comp(ncol);
    
    // Calculate portfolio ES
    IntegerVector sort_order = sort_indexes(pnl_port) + 1; // Adjust for 1-based indexing in R
    
    for (int i = 0; i < ncol; ++i) {
      NumericVector pnl_comp = sim_pnl(_, i);
      out_comp[i] = expected_shortfall_component(pnl_comp, confidence_level, sort_order) * w(i); // Adjust for 0-based indexing
    }
    
    // Combine ES with component ES
    return List::create(Named("es_portfolio") = es, _["es_components"] = out_comp);
  } 
  
  return wrap(es);
}

// Helper function to normalize weights
NumericVector normalize_weights(NumericVector weights) {
  double sum_weights = sum(weights);
  return weights / sum_weights;
}

// Main function to adjust weights
// [[Rcpp::export]]
NumericMatrix adjust_weights(NumericMatrix weights, NumericVector min_weights, NumericVector max_weights, bool do_adjust_zero_weight = true, double step_size = 0.5, double delta = 0.0005, int iter_max = 100) {
  int n_rows = weights.nrow();
  int n_cols = weights.ncol();
  
  for (int i = 0; i < n_rows; ++i) {
    NumericVector row = weights(i, _);
    //NumericVector min_row = min_weights;
    //NumericVector max_row = max_weights;
    int iter = 0;
    
    while (iter < iter_max) {
      bool adjusted = false;
      for (int j = 0; j < n_cols; ++j) {
        if ((row[j] == 0) & (!do_adjust_zero_weight)) {		  
		  adjusted = true;		  
        }
		if ((row[j] < min_weights[j]) & (do_adjust_zero_weight)) {
		  row[j] += std::max((min_weights[j] - row[j]) / (1 / step_size), delta);
		  adjusted = true;		  
        }
        if (row[j] > max_weights[j]) {
          row[j] -= std::max((row[j] - max_weights[j]) / (1 / step_size), delta);
          adjusted = true;
        }
      }
      row = normalize_weights(row);
      if (!adjusted) break;
      iter++;
    }
    
    weights(i, _) = row;
  }
  
  return weights;
}

