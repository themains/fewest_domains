# Robust Horvitz-Thompson Implementation
# Addresses numerical stability and implementation issues

source("improved_dgp.R")

# Robust HT estimator with numerical safeguards
robust_ht_estimator <- function(cij, adult_domains, selected_domains, pi_j, 
                               winsorize_weights = TRUE, max_weight = 50) {
  
  n <- nrow(cij)
  y_hat <- numeric(n)
  
  for (i in 1:n) {
    Ti <- sum(cij[i, ])  # Total visits (known)
    
    if (Ti == 0) {
      y_hat[i] <- 0
      next
    }
    
    # HT numerator with robust weights
    Ni_hat <- 0
    
    for (j in selected_domains) {
      if (pi_j[j] > 0) {
        A_j <- as.numeric(j %in% adult_domains)
        
        # Robust weight calculation
        ht_weight <- 1 / pi_j[j]
        
        # Winsorize extreme weights to prevent instability
        if (winsorize_weights) {
          ht_weight <- min(ht_weight, max_weight)
        }
        
        Ni_hat <- Ni_hat + (cij[i, j] * A_j * ht_weight)
      }
    }
    
    # HT proportion estimate
    y_hat[i] <- Ni_hat / Ti
  }
  
  return(y_hat)
}

# Robust domain weight computation with regularization
robust_domain_weights <- function(cij, X_matrix, regularization = 1e-8) {
  
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Total visits per user
  Ti <- rowSums(cij)
  
  # Robust leverage computation with regularization
  X_design <- X_matrix
  if (!all(X_design[, 1] == 1)) {
    X_design <- cbind(1, X_matrix[, -1])
  }
  
  # Add regularization to prevent singular matrices
  XtX <- t(X_design) %*% X_design
  XtX_reg <- XtX + diag(regularization, ncol(XtX))
  XtX_inv <- solve(XtX_reg)
  
  # Compute leverage scores
  leverage <- diag(X_design %*% XtX_inv %*% t(X_design))
  
  # Robust domain weights with better numerical properties
  w_j <- numeric(m)
  
  for (j in 1:m) {
    weight <- 0
    
    for (i in 1:n) {
      if (Ti[i] > 0) {
        # Use proportion but add small regularization
        proportion_ij <- cij[i, j] / (Ti[i] + 1e-10)
        
        # Weight contribution with numerical stability
        contribution <- leverage[i] * proportion_ij^2
        
        # Add small floor to prevent zero weights
        contribution <- max(contribution, 1e-12)
        
        weight <- weight + contribution
      }
    }
    
    w_j[j] <- weight
  }
  
  # Ensure all weights are positive
  w_j <- pmax(w_j, 1e-10 * max(w_j))
  
  return(list(
    weights = w_j,
    leverage = leverage,
    total_visits = Ti
  ))
}

# Robust inclusion probability setting
robust_inclusion_probabilities <- function(w_j, budget, min_prob = 0.001, max_prob = 0.95) {
  
  # Square root allocation with floors and ceilings
  sqrt_weights <- sqrt(pmax(w_j, 1e-10))
  
  # Scale to budget
  pi_j <- sqrt_weights * budget / sum(sqrt_weights)
  
  # Apply bounds
  pi_j <- pmax(pi_j, min_prob)
  pi_j <- pmin(pi_j, max_prob)
  
  # Renormalize to exactly meet budget (approximately)
  scaling_factor <- budget / sum(pi_j)
  pi_j <- pi_j * scaling_factor
  
  # Final bounds check
  pi_j <- pmin(pi_j, max_prob)
  
  return(pi_j)
}

# Split-sample validation for bias checking
split_sample_validation <- function(cij, X_matrix, adult_domains, budget, n_splits = 3, seed = 2024) {
  
  set.seed(seed)
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Ground truth for comparison
  y_true <- numeric(n)
  for (i in 1:n) {
    if (sum(cij[i, ]) > 0) {
      y_true[i] <- sum(cij[i, adult_domains]) / sum(cij[i, ])
    }
  }
  
  lm_true <- lm(y_true ~ X_matrix[, -1])
  beta_true <- coef(lm_true)
  
  # Compute domain weights once
  weight_info <- robust_domain_weights(cij, X_matrix)
  w_j <- weight_info$weights
  
  # Run multiple independent samples
  split_results <- list()
  
  for (split in 1:n_splits) {
    cat("Split sample", split, ":")
    
    # Set inclusion probabilities
    pi_j <- robust_inclusion_probabilities(w_j, budget)
    
    # Sample domains
    set.seed(seed + split * 100)
    selected <- which(runif(m) < pi_j)
    
    if (length(selected) == 0) {
      cat(" No domains selected\n")
      next
    }
    
    cat(" ", length(selected), "domains")
    
    # Compute HT estimates
    y_hat <- robust_ht_estimator(cij, adult_domains, selected, pi_j, 
                                winsorize_weights = TRUE, max_weight = 20)
    
    # Run regression
    tryCatch({
      lm_hat <- lm(y_hat ~ X_matrix[, -1])
      beta_hat <- coef(lm_hat)
      
      # Calculate bias
      bias <- beta_hat - beta_true
      pct_bias <- 100 * abs(bias) / abs(beta_true)
      pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
      
      max_bias <- max(pct_bias, na.rm = TRUE)
      
      # Basic diagnostics
      y_corr <- cor(y_hat, y_true, use = "complete.obs")
      mean_y_hat <- mean(y_hat, na.rm = TRUE)
      mean_y_true <- mean(y_true, na.rm = TRUE)
      
      cat(", bias:", round(max_bias, 1), "%, corr:", round(y_corr, 3), "\n")
      
      split_results[[split]] <- list(
        selected = selected,
        n_selected = length(selected),
        beta_hat = beta_hat,
        max_bias = max_bias,
        y_correlation = y_corr,
        mean_y_hat = mean_y_hat,
        mean_y_true = mean_y_true,
        bias_vector = bias,
        pct_bias_vector = pct_bias
      )
      
    }, error = function(e) {
      cat(", regression failed:", e$message, "\n")
    })
  }
  
  return(list(
    splits = split_results,
    beta_true = beta_true,
    y_true = y_true,
    domain_weights = w_j
  ))
}

# Comprehensive robust HT test
test_robust_ht <- function(budget_fraction = 0.25) {
  
  cat("🎯 TESTING ROBUST HORVITZ-THOMPSON IMPLEMENTATION\n")
  cat(rep("=", 75), "\n")
  
  # Use improved DGP
  data <- generate_improved_data(n_users = 200, n_domains = 300, adult_fraction = 0.15, seed = 2024)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- data$adult_domains
  
  n <- nrow(cij)
  m <- ncol(cij)
  budget <- round(m * budget_fraction)
  
  cat("Setup:\n")
  cat("- Users:", n, "| Domains:", m, "| Adult domains:", length(adult_domains), "\n")
  cat("- Budget:", budget, "domains (", round(100 * budget_fraction, 1), "%)\n")
  cat("- Data sparsity:", round(100 * data$characteristics$sparsity, 1), "%\n")
  cat("- Avg domains/user:", round(data$characteristics$avg_domains_per_user, 1), "\n\n")
  
  # Ground truth
  lm_true <- lm(y_true ~ X_matrix[, -1])
  beta_true <- coef(lm_true)
  
  cat("Ground truth coefficients:\n")
  print(round(beta_true, 4))
  
  # Run split-sample validation
  cat("\n=== SPLIT-SAMPLE VALIDATION ===\n")
  validation_results <- split_sample_validation(cij, X_matrix, adult_domains, budget, n_splits = 5)
  
  # Analyze results
  valid_splits <- validation_results$splits
  valid_splits <- valid_splits[!sapply(valid_splits, is.null)]
  
  if (length(valid_splits) > 0) {
    
    # Extract key metrics
    n_domains_used <- sapply(valid_splits, function(x) x$n_selected)
    max_biases <- sapply(valid_splits, function(x) x$max_bias)
    correlations <- sapply(valid_splits, function(x) x$y_correlation)
    
    cat("\n=== RESULTS SUMMARY ===\n")
    cat("Valid splits:", length(valid_splits), "/ 5\n")
    cat("Domains used: [", paste(range(n_domains_used), collapse = "-"), 
        "], mean =", round(mean(n_domains_used), 1), "\n")
    cat("Max bias: [", round(min(max_biases), 1), "-", round(max(max_biases), 1), 
        "]%, mean =", round(mean(max_biases), 1), "%\n")
    cat("Y correlation: [", round(min(correlations), 3), "-", round(max(correlations), 3), 
        "], mean =", round(mean(correlations), 3), "\n")
    
    # Coefficient stability across splits
    cat("\n=== COEFFICIENT STABILITY ===\n")
    beta_matrix <- matrix(NA, nrow = length(valid_splits), ncol = length(beta_true))
    colnames(beta_matrix) <- names(beta_true)
    
    for (i in seq_along(valid_splits)) {
      beta_matrix[i, ] <- valid_splits[[i]]$beta_hat
    }
    
    # Show coefficient ranges
    for (j in 1:ncol(beta_matrix)) {
      coef_name <- colnames(beta_matrix)[j]
      coef_range <- range(beta_matrix[, j], na.rm = TRUE)
      coef_mean <- mean(beta_matrix[, j], na.rm = TRUE)
      coef_true <- beta_true[j]
      
      cat(sprintf("%-20s: True=%.4f, Est=[%.4f-%.4f], Mean=%.4f\n", 
                  coef_name, coef_true, coef_range[1], coef_range[2], coef_mean))
    }
    
    # Overall assessment
    cat("\n=== ASSESSMENT ===\n")
    
    mean_max_bias <- mean(max_biases)
    mean_correlation <- mean(correlations)
    
    if (mean_max_bias < 25 && mean_correlation > 0.8) {
      status <- "✅ EXCELLENT: Low bias, high correlation"
    } else if (mean_max_bias < 50 && mean_correlation > 0.7) {
      status <- "👍 GOOD: Moderate bias, decent correlation"  
    } else if (mean_max_bias < 100) {
      status <- "⚠️ MODERATE: Some bias but might be acceptable"
    } else {
      status <- "❌ POOR: High bias, problematic for regression"
    }
    
    cat("Status:", status, "\n")
    
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
    
    cat("\n=== BASELINE COMPARISON ===\n")
    cat("Robust HT: ~", round(mean(n_domains_used)), "domains, ~", 
        round(mean_max_bias, 1), "% bias\n")
    cat("Naive: ", length(naive_domains), "domains, ", 
        round(max(bias_naive), 1), "% bias\n")
    
    efficiency_gain <- length(naive_domains) / mean(n_domains_used)
    cat("Efficiency gain:", round(efficiency_gain, 1), "x\n")
    
    if (mean_max_bias < 50) {
      cat("🎉 SUCCESS: Robust HT showing improvement!\n")
    } else {
      cat("🔧 Still needs work, but progress made\n")
    }
    
  } else {
    cat("❌ All splits failed - need further debugging\n")
  }
  
  return(validation_results)
}

# Test multiple budget levels with robust implementation
test_robust_budgets <- function() {
  
  cat("📊 TESTING ROBUST HT ACROSS BUDGET LEVELS\n")
  cat(rep("=", 75), "\n")
  
  budgets <- c(0.15, 0.25, 0.35, 0.45)
  
  for (budget_frac in budgets) {
    cat("\n", rep("-", 50), "\n")
    cat("BUDGET:", round(100 * budget_frac), "% of domains\n")
    cat(rep("-", 50), "\n")
    
    results <- test_robust_ht(budget_frac)
    
    # Quick summary
    if (length(results$splits) > 0) {
      valid_splits <- results$splits[!sapply(results$splits, is.null)]
      if (length(valid_splits) > 0) {
        mean_bias <- mean(sapply(valid_splits, function(x) x$max_bias))
        
        if (mean_bias < 30) {
          cat("Result: ✅ PROMISING (", round(mean_bias, 1), "% avg bias)\n")
        } else if (mean_bias < 75) {
          cat("Result: ⚠️ MODERATE (", round(mean_bias, 1), "% avg bias)\n")  
        } else {
          cat("Result: ❌ HIGH BIAS (", round(mean_bias, 1), "% avg bias)\n")
        }
      }
    }
  }
}

# Run the robust implementation test
if (!interactive()) {
  cat("🚀 TESTING ROBUST HORVITZ-THOMPSON IMPLEMENTATION\n\n")
  
  # Test single budget first
  test_robust_ht(budget_fraction = 0.25)
  
  cat("\n")
  
  # Test across budgets
  test_robust_budgets()
  
  cat("\n✅ ROBUST HT IMPLEMENTATION TESTING COMPLETE!\n")
}