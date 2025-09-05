# Clean Implementation of Horvitz-Thompson Approach
# Following the exact prescription from the problem statement

source("synthetic_data.R")

# Simple HT estimator implementation
ht_estimator_simple <- function(cij, adult_domains, selected_domains, pi_j) {
  
  n <- nrow(cij)
  y_hat <- numeric(n)
  
  for (i in 1:n) {
    Ti <- sum(cij[i, ])  # Total visits (known from logs)
    
    if (Ti == 0) {
      y_hat[i] <- 0
      next
    }
    
    # HT numerator: N̂_i = Σ_{j∈S} (c_ij * A_j) / π_j  
    Ni_hat <- 0
    for (j in selected_domains) {
      A_j <- as.numeric(j %in% adult_domains)
      Ni_hat <- Ni_hat + (cij[i, j] * A_j) / pi_j[j]
    }
    
    # HT proportion estimate: ŷ_i = N̂_i / T_i
    y_hat[i] <- Ni_hat / Ti
  }
  
  return(y_hat)
}

# Compute domain importance weights w_j = Σ_i ℓ_i * (c_ij/T_i)^2
compute_importance_weights <- function(cij, X_matrix) {
  
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Total visits per user
  Ti <- rowSums(cij)
  
  # Regression leverage scores ℓ_i = x_i' (X'X)^{-1} x_i
  X_design <- X_matrix  # Assume it already has intercept
  if (!all(X_design[, 1] == 1)) {
    X_design <- cbind(1, X_matrix[, -1])  # Add intercept if missing
  }
  
  XtX_inv <- solve(t(X_design) %*% X_design + diag(1e-10, ncol(X_design)))  # Add regularization
  leverage <- diag(X_design %*% XtX_inv %*% t(X_design))
  
  # Domain weights: w_j = Σ_i ℓ_i * (c_ij / T_i)^2
  w_j <- numeric(m)
  for (j in 1:m) {
    weight <- 0
    for (i in 1:n) {
      if (Ti[i] > 0) {
        proportion_ij <- cij[i, j] / Ti[i]
        weight <- weight + leverage[i] * proportion_ij^2
      }
    }
    w_j[j] <- weight
  }
  
  return(list(
    weights = w_j,
    leverage = leverage,
    total_visits = Ti
  ))
}

# Set inclusion probabilities π_j ∝ √w_j 
set_inclusion_probabilities <- function(w_j, budget, min_prob = 1e-6) {
  
  # Add small floor to avoid zeros
  w_floored <- pmax(w_j, min_prob * max(w_j, na.rm = TRUE))
  
  # Square root allocation: π_j ∝ √w_j
  pi_j <- sqrt(w_floored)
  
  # Scale to meet budget: Σ π_j = budget
  pi_j <- pi_j * budget / sum(pi_j)
  
  # Cap at 1 (domains can't be selected with prob > 1)
  pi_j <- pmin(pi_j, 1)
  
  return(pi_j)
}

# Test the clean HT implementation with different budgets
test_clean_ht <- function(budget_fraction = 0.3) {
  
  cat("🎯 TESTING CLEAN HORVITZ-THOMPSON IMPLEMENTATION\n")
  cat(rep("=", 70), "\n")
  
  # Generate test data
  data <- generate_regression_data(n_respondents = 150, n_domains = 200, seed = 1234)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- which(data$domain_types == "adult")
  
  n <- nrow(cij)
  m <- ncol(cij)
  budget <- round(m * budget_fraction)
  
  cat("Setup:\n")
  cat("- Users:", n, "| Domains:", m, "| Adult domains:", length(adult_domains), "\n")
  cat("- Budget:", budget, "domains (", round(100 * budget_fraction, 1), "% of total)\n\n")
  
  # Ground truth regression
  lm_true <- lm(y_true ~ X_matrix[, -1])
  beta_true <- coef(lm_true)
  
  cat("Ground truth coefficients:\n")
  print(round(beta_true, 4))
  
  # Compute importance weights
  weight_info <- compute_importance_weights(cij, X_matrix)
  w_j <- weight_info$weights
  
  cat("\nDomain weight statistics:\n")
  cat("- Non-zero weights:", sum(w_j > 0), "/", m, "\n")
  cat("- Weight range: [", round(min(w_j), 6), ",", round(max(w_j), 6), "]\n")
  cat("- Mean weight:", round(mean(w_j), 6), "\n")
  
  # Set inclusion probabilities
  pi_j <- set_inclusion_probabilities(w_j, budget)
  
  cat("- Inclusion prob range: [", round(min(pi_j), 4), ",", round(max(pi_j), 4), "]\n")
  cat("- Expected sample size:", round(sum(pi_j), 1), "\n\n")
  
  # Multiple trials to show stability
  n_trials <- 5
  results <- list()
  
  for (trial in 1:n_trials) {
    cat("TRIAL", trial, ":\n")
    
    # Sample domains (Poisson sampling)
    set.seed(1000 + trial)  # For reproducibility
    selected <- which(runif(m) < pi_j)
    
    cat("- Selected", length(selected), "domains\n")
    
    if (length(selected) == 0) {
      cat("- No domains selected, skipping\n\n")
      next
    }
    
    # Compute HT estimates  
    y_hat <- ht_estimator_simple(cij, adult_domains, selected, pi_j)
    
    # Basic sanity checks
    cat("- Mean ŷ:", round(mean(y_hat), 4), "| Mean y_true:", round(mean(y_true), 4), "\n")
    cat("- Correlation(ŷ, y_true):", round(cor(y_hat, y_true, use = "complete.obs"), 4), "\n")
    
    # Run regression
    tryCatch({
      lm_hat <- lm(y_hat ~ X_matrix[, -1])
      beta_hat <- coef(lm_hat)
      
      # Calculate bias
      bias <- beta_hat - beta_true
      pct_bias <- 100 * abs(bias) / abs(beta_true)
      pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
      
      max_bias <- max(pct_bias, na.rm = TRUE)
      cat("- Max coefficient bias:", round(max_bias, 1), "%\n")
      
      # Error correlation test
      errors <- y_hat - y_true
      if (length(unique(errors)) > 1) {
        error_lm <- lm(errors ~ X_matrix[, -1])
        f_test <- summary(error_lm)$fstatistic
        if (!is.null(f_test)) {
          f_pvalue <- pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)
          cat("- Error correlation p-value:", round(f_pvalue, 4), "\n")
        }
      }
      
      results[[trial]] <- list(
        selected_domains = selected,
        n_selected = length(selected),
        beta_hat = beta_hat,
        max_bias = max_bias,
        y_hat = y_hat
      )
      
    }, error = function(e) {
      cat("- Regression failed:", e$message, "\n")
    })
    
    cat("\n")
  }
  
  # Summary across trials
  if (length(results) > 0) {
    cat("SUMMARY ACROSS", length(results), "TRIALS:\n")
    
    domains_used <- sapply(results, function(x) x$n_selected)
    max_biases <- sapply(results, function(x) x$max_bias)
    
    cat("- Domains used: [", paste(range(domains_used), collapse = "-"), "], mean =", 
        round(mean(domains_used), 1), "\n")
    cat("- Max bias range: [", round(min(max_biases), 1), "-", round(max(max_biases), 1), 
        "]%, mean =", round(mean(max_biases), 1), "%\n")
    
    # Compare to naive baseline
    source("main_methods_interface.R")
    naive_domains <- naive_method_simple(cij, 0.1)
    
    y_naive <- numeric(n)
    for (i in 1:n) {
      visits_selected <- sum(cij[i, naive_domains])
      if (visits_selected > 0) {
        adult_visits_selected <- sum(cij[i, intersect(naive_domains, adult_domains)])
        y_naive[i] <- adult_visits_selected / visits_selected
      }
    }
    
    lm_naive <- lm(y_naive ~ X_matrix[, -1])
    beta_naive <- coef(lm_naive)
    bias_naive <- 100 * abs(beta_naive - beta_true) / abs(beta_true)
    bias_naive[is.infinite(bias_naive) | is.na(bias_naive)] <- 0
    
    cat("\nCOMPARISON:\n")
    cat("- HT method: ~", round(mean(domains_used)), "domains, ~", 
        round(mean(max_biases), 1), "% max bias\n")
    cat("- Naive method:", length(naive_domains), "domains,", 
        round(max(bias_naive), 1), "% max bias\n")
    
    efficiency_gain <- length(naive_domains) / mean(domains_used)
    cat("- Efficiency gain:", round(efficiency_gain, 1), "x fewer domains\n")
    
    if (mean(max_biases) < 30) {
      cat("✅ HT method shows promise!\n")
    } else {
      cat("⚠️ Still significant bias - may need more domains or better implementation\n")
    }
  }
  
  return(results)
}

# Test with different budget levels
test_budget_levels <- function() {
  
  cat("🔬 TESTING MULTIPLE BUDGET LEVELS\n")
  cat(rep("=", 70), "\n")
  
  budgets <- c(0.2, 0.3, 0.4, 0.5)
  
  for (budget_frac in budgets) {
    cat("\n", rep("-", 40), "\n")
    cat("BUDGET:", round(100 * budget_frac), "% of domains\n")
    cat(rep("-", 40), "\n")
    
    results <- test_clean_ht(budget_frac)
    
    if (length(results) > 0) {
      max_biases <- sapply(results, function(x) x$max_bias)
      mean_bias <- mean(max_biases)
      
      status <- if (mean_bias < 25) "✅ GOOD" else if (mean_bias < 50) "⚠️ MODERATE" else "❌ POOR"
      cat("Status:", status, "(", round(mean_bias, 1), "% avg bias)\n")
    }
  }
}

# Run the clean implementation test
if (!interactive()) {
  cat("🚀 TESTING CLEAN HORVITZ-THOMPSON IMPLEMENTATION\n\n")
  
  # Test single budget level first
  test_clean_ht(budget_fraction = 0.3)
  
  cat("\n")
  
  # Test multiple budget levels
  test_budget_levels()
  
  cat("\n✅ CLEAN HT IMPLEMENTATION TESTING COMPLETE!\n")
}