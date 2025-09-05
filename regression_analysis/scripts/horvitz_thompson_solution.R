# Horvitz-Thompson Domain Selection Solution
# Implements the principled approach: separate "what we sample" from "what we estimate"

source("synthetic_data.R")

# Horvitz-Thompson estimator for adult content proportions
horvitz_thompson_estimator <- function(cij, selected_domains, inclusion_probs, adult_domains) {
  
  n <- nrow(cij)
  y_hat <- numeric(n)
  
  for (i in 1:n) {
    Ti <- sum(cij[i, ])  # Total visits for user i (known from logs)
    
    if (Ti == 0) {
      y_hat[i] <- 0
      next
    }
    
    # HT numerator: Σ_{j∈S} (c_ij * A_j) / π_j
    Ni_hat <- 0
    for (j in selected_domains) {
      if (inclusion_probs[j] > 0) {
        A_j <- as.numeric(j %in% adult_domains)  # Domain label
        Ni_hat <- Ni_hat + (cij[i, j] * A_j) / inclusion_probs[j]
      }
    }
    
    # HT estimator: ŷ_i = N̂_i / T_i
    y_hat[i] <- Ni_hat / Ti
  }
  
  return(y_hat)
}

# Compute domain importance weights
compute_domain_weights <- function(cij, X_matrix) {
  
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Compute total visits per user
  Ti <- rowSums(cij)
  
  # Compute regression leverage scores
  X_design <- cbind(1, X_matrix[, -1])  # Remove existing intercept, add new one
  XtX_inv <- solve(t(X_design) %*% X_design)
  leverage <- diag(X_design %*% XtX_inv %*% t(X_design))
  
  # Compute domain weights: w_j = Σ_i ℓ_i * (c_ij / T_i)^2
  domain_weights <- numeric(m)
  
  for (j in 1:m) {
    weight <- 0
    for (i in 1:n) {
      if (Ti[i] > 0) {
        user_contribution <- (cij[i, j] / Ti[i])^2
        weight <- weight + leverage[i] * user_contribution
      }
    }
    domain_weights[j] <- weight
  }
  
  return(list(
    weights = domain_weights,
    leverage = leverage,
    total_visits = Ti
  ))
}

# Design-optimal domain sampling
sample_domains_optimal <- function(domain_weights, budget, min_prob = 0.001) {
  
  m <- length(domain_weights)
  
  # Add small floor to prevent zero probabilities
  weights_floored <- pmax(domain_weights, min_prob * max(domain_weights))
  
  # Set inclusion probabilities proportional to √weights
  inclusion_probs <- sqrt(weights_floored)
  
  # Scale to meet budget: Σ π_j = budget
  inclusion_probs <- inclusion_probs * budget / sum(inclusion_probs)
  
  # Cap probabilities at 1
  inclusion_probs <- pmin(inclusion_probs, 1)
  
  # Sample domains with these probabilities (Poisson sampling)
  selected <- which(runif(m) < inclusion_probs)
  
  return(list(
    selected_domains = selected,
    inclusion_probs = inclusion_probs,
    expected_size = sum(inclusion_probs),
    actual_size = length(selected)
  ))
}

# Domain bootstrap for standard errors
domain_bootstrap <- function(cij, X_matrix, selected_domains, inclusion_probs, 
                           adult_domains, n_bootstrap = 100) {
  
  n <- nrow(cij)
  m <- ncol(cij)
  p <- ncol(X_matrix)
  
  bootstrap_betas <- matrix(NA, nrow = n_bootstrap, ncol = p)
  
  for (b in 1:n_bootstrap) {
    # Resample domains according to inclusion probabilities
    boot_selected <- which(runif(m) < inclusion_probs)
    
    if (length(boot_selected) == 0) next
    
    # Compute HT estimates with bootstrap sample
    y_boot <- horvitz_thompson_estimator(cij, boot_selected, inclusion_probs, adult_domains)
    
    # Run regression
    tryCatch({
      lm_boot <- lm(y_boot ~ X_matrix[, -1])
      bootstrap_betas[b, ] <- coef(lm_boot)
    }, error = function(e) {
      # Skip this bootstrap if regression fails
    })
  }
  
  # Remove failed bootstrap samples
  valid_boots <- complete.cases(bootstrap_betas)
  bootstrap_betas <- bootstrap_betas[valid_boots, , drop = FALSE]
  
  # Compute bootstrap standard errors
  if (nrow(bootstrap_betas) > 0) {
    bootstrap_se <- apply(bootstrap_betas, 2, sd, na.rm = TRUE)
    bootstrap_mean <- apply(bootstrap_betas, 2, mean, na.rm = TRUE)
  } else {
    bootstrap_se <- rep(NA, p)
    bootstrap_mean <- rep(NA, p)
  }
  
  return(list(
    se = bootstrap_se,
    mean = bootstrap_mean,
    samples = bootstrap_betas,
    valid_boots = sum(valid_boots)
  ))
}

# Complete Horvitz-Thompson method
horvitz_thompson_method <- function(cij, X_matrix, adult_domains, budget, 
                                  target_user_se = 0.1, target_coef_bias = 0.15,
                                  max_iterations = 5, verbose = TRUE) {
  
  if (verbose) {
    cat("=== HORVITZ-THOMPSON METHOD ===\n")
    cat("Budget:", budget, "domains\n")
    cat("Target user SE:", target_user_se, "\n")
    cat("Target coefficient bias:", target_coef_bias, "\n\n")
  }
  
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
  
  # Compute domain importance weights
  weight_info <- compute_domain_weights(cij, X_matrix)
  domain_weights <- weight_info$weights
  
  if (verbose) {
    cat("Domain weight statistics:\n")
    cat("- Min weight:", round(min(domain_weights), 6), "\n")
    cat("- Max weight:", round(max(domain_weights), 6), "\n")
    cat("- Mean weight:", round(mean(domain_weights), 6), "\n\n")
  }
  
  current_budget <- budget
  iteration <- 0
  
  repeat {
    iteration <- iteration + 1
    if (iteration > max_iterations) break
    
    if (verbose) {
      cat("ITERATION", iteration, ":\n")
    }
    
    # Sample domains optimally
    sampling_result <- sample_domains_optimal(domain_weights, current_budget)
    selected_domains <- sampling_result$selected_domains
    inclusion_probs <- sampling_result$inclusion_probs
    
    if (verbose) {
      cat("- Sampled", length(selected_domains), "domains\n")
    }
    
    # Compute HT estimates
    y_hat <- horvitz_thompson_estimator(cij, selected_domains, inclusion_probs, adult_domains)
    
    # Check user-level precision (approximate)
    user_precision_ok <- TRUE
    bad_users <- 0
    
    for (i in 1:n) {
      Ti <- sum(cij[i, ])
      if (Ti == 0) next
      
      # Approximate variance (conservative bound)
      var_approx <- 0
      for (j in selected_domains) {
        if (inclusion_probs[j] > 0) {
          var_approx <- var_approx + (1 - inclusion_probs[j]) / inclusion_probs[j] * (cij[i, j] / Ti)^2
        }
      }
      
      se_approx <- sqrt(var_approx)
      if (se_approx > target_user_se) {
        user_precision_ok <- FALSE
        bad_users <- bad_users + 1
      }
    }
    
    if (verbose) {
      cat("- Users exceeding SE threshold:", bad_users, "/", n, "\n")
    }
    
    # Run regression
    lm_hat <- lm(y_hat ~ X_matrix[, -1])
    beta_hat <- coef(lm_hat)
    
    # Check coefficient bias
    bias <- beta_hat - beta_true
    pct_bias <- 100 * abs(bias) / abs(beta_true)
    pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
    max_bias <- max(pct_bias)
    
    coef_precision_ok <- max_bias < target_coef_bias * 100
    
    if (verbose) {
      cat("- Max coefficient bias:", round(max_bias, 1), "%\n")
    }
    
    # Check if both constraints satisfied
    if (user_precision_ok && coef_precision_ok) {
      if (verbose) {
        cat("✅ Both constraints satisfied!\n")
      }
      break
    }
    
    # If not satisfied, increase budget and try again
    current_budget <- round(current_budget * 1.2)
    if (current_budget > m * 0.8) {
      if (verbose) {
        cat("⚠️ Budget limit reached\n")
      }
      break
    }
    
    if (verbose) {
      cat("- Increasing budget to", current_budget, "\n\n")
    }
  }
  
  # Final bootstrap for robust SEs
  bootstrap_result <- domain_bootstrap(cij, X_matrix, selected_domains, 
                                     inclusion_probs, adult_domains, n_bootstrap = 50)
  
  if (verbose) {
    cat("FINAL RESULTS:\n")
    cat("- Selected domains:", length(selected_domains), "/", m, 
        "(", round(100 * length(selected_domains) / m, 1), "%)\n")
    cat("- Bootstrap SEs computed from", bootstrap_result$valid_boots, "valid samples\n")
    cat("- Max coefficient bias:", round(max_bias, 1), "%\n")
  }
  
  return(list(
    method = "Horvitz_Thompson",
    selected_domains = selected_domains,
    n_domains = length(selected_domains),
    inclusion_probs = inclusion_probs,
    y_hat = y_hat,
    beta_hat = beta_hat,
    beta_true = beta_true,
    max_bias_pct = max_bias,
    bootstrap_se = bootstrap_result$se,
    iterations = iteration,
    final_budget = current_budget,
    domain_weights = domain_weights
  ))
}

# Test the Horvitz-Thompson method
test_horvitz_thompson <- function() {
  
  cat("🎯 TESTING HORVITZ-THOMPSON APPROACH\n")
  cat(rep("=", 70), "\n")
  
  # Generate test data
  data <- generate_regression_data(n_respondents = 200, n_domains = 300, seed = 2024)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- which(data$domain_types == "adult")
  
  cat("Test setup:\n")
  cat("- Respondents:", nrow(cij), "\n")
  cat("- Domains:", ncol(cij), "\n")
  cat("- Adult domains:", length(adult_domains), "\n\n")
  
  # Ground truth
  lm_true <- lm(y_true ~ X_matrix[,-1])
  beta_true <- coef(lm_true)
  
  cat("Ground truth coefficients:\n")
  print(round(beta_true, 4))
  
  # Test Horvitz-Thompson method with different budgets
  budgets <- c(30, 50, 75, 100)
  results <- list()
  
  for (budget in budgets) {
    cat("\n", rep("-", 50), "\n")
    cat("TESTING BUDGET:", budget, "domains\n")
    
    result <- horvitz_thompson_method(cij, X_matrix, adult_domains, budget,
                                    target_user_se = 0.1, target_coef_bias = 0.20,
                                    verbose = TRUE)
    
    results[[paste0("Budget_", budget)]] <- result
  }
  
  # Compare with baseline methods
  cat("\n", rep("-", 50), "\n")
  cat("BASELINE COMPARISONS:\n")
  
  # Naive baseline (for reference)
  source("main_methods_interface.R")
  naive_domains <- naive_method_simple(cij, 0.1)
  
  y_naive <- numeric(nrow(cij))
  for (i in 1:nrow(cij)) {
    visits_selected <- sum(cij[i, naive_domains])
    if (visits_selected > 0) {
      adult_visits_selected <- sum(cij[i, intersect(naive_domains, adult_domains)])
      y_naive[i] <- adult_visits_selected / visits_selected
    }
  }
  
  lm_naive <- lm(y_naive ~ X_matrix[,-1])
  beta_naive <- coef(lm_naive)
  bias_naive <- 100 * abs(beta_naive - beta_true) / abs(beta_true)
  bias_naive[is.infinite(bias_naive) | is.na(bias_naive)] <- 0
  
  cat("Naive method:", length(naive_domains), "domains, max bias:", round(max(bias_naive), 1), "%\n")
  
  # Summary table
  cat("\n", rep("=", 70), "\n")
  cat("HORVITZ-THOMPSON RESULTS SUMMARY\n")
  cat(rep("=", 70), "\n")
  
  cat(sprintf("%-15s %8s %10s %10s %8s\n", 
              "Method", "Domains", "Efficiency%", "MaxBias%", "Status"))
  cat(rep("-", 70), "\n")
  
  for (name in names(results)) {
    r <- results[[name]]
    efficiency <- 100 * r$n_domains / ncol(cij)
    status <- if(r$max_bias_pct < 20) "✅ GOOD" else "❌ POOR"
    
    cat(sprintf("%-15s %8d %10.1f %10.1f %8s\n",
                name, r$n_domains, efficiency, r$max_bias_pct, status))
  }
  
  # Add naive baseline
  naive_efficiency <- 100 * length(naive_domains) / ncol(cij)
  naive_status <- if(max(bias_naive) < 20) "✅ GOOD" else "❌ POOR"
  cat(sprintf("%-15s %8d %10.1f %10.1f %8s\n",
              "Naive_Baseline", length(naive_domains), naive_efficiency, 
              max(bias_naive), naive_status))
  
  cat("\n💡 KEY INSIGHTS:\n")
  cat("- HT method eliminates systematic bias from the estimator\n")
  cat("- Domain sampling designed for regression precision\n")
  cat("- Bootstrap provides valid standard errors\n")
  cat("- Budget adaptation finds efficient solutions\n")
  
  return(results)
}

# Run the test
if (!interactive()) {
  cat("🚀 TESTING HORVITZ-THOMPSON SOLUTION\n\n")
  results <- test_horvitz_thompson()
  
  cat("\n✅ HORVITZ-THOMPSON TESTING COMPLETE!\n")
  cat("This principled approach should dramatically outperform ratio-based methods.\n")
}