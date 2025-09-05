# X-Aligned Error Optimization
# Focus on minimizing the component of error that correlates with features

source("improved_dgp.R")

# Compute X'u directly from domain bootstrap samples
compute_x_aligned_error <- function(cij, X_matrix, adult_domains, selected_domains, pi_j, 
                                  n_bootstrap = 50, verbose = FALSE) {
  
  n <- nrow(cij)
  m <- ncol(cij)
  
  # True proportions for comparison
  y_true <- numeric(n)
  for (i in 1:n) {
    if (sum(cij[i, ]) > 0) {
      y_true[i] <- sum(cij[i, adult_domains]) / sum(cij[i, ])
    }
  }
  
  # Bootstrap estimates to get error distribution
  x_aligned_errors <- matrix(NA, nrow = n_bootstrap, ncol = ncol(X_matrix))
  colnames(x_aligned_errors) <- colnames(X_matrix)
  
  y_estimates <- matrix(NA, nrow = n_bootstrap, ncol = n)
  
  for (b in 1:n_bootstrap) {
    # Bootstrap sample domains
    boot_selected <- which(runif(m) < pi_j)
    
    if (length(boot_selected) == 0) next
    
    # Compute HT estimates for this bootstrap
    y_boot <- numeric(n)
    
    for (i in 1:n) {
      Ti <- sum(cij[i, ])
      if (Ti == 0) {
        y_boot[i] <- 0
        next
      }
      
      Ni_hat <- 0
      for (j in boot_selected) {
        if (pi_j[j] > 0) {
          A_j <- as.numeric(j %in% adult_domains)
          ht_weight <- min(1/pi_j[j], 20)  # Winsorize weights
          Ni_hat <- Ni_hat + (cij[i, j] * A_j * ht_weight)
        }
      }
      
      y_boot[i] <- Ni_hat / Ti
    }
    
    y_estimates[b, ] <- y_boot
    
    # Compute errors u = y_hat - y_true
    errors <- y_boot - y_true
    
    # Compute X'u (the X-aligned component)
    x_aligned_errors[b, ] <- t(X_matrix) %*% errors
  }
  
  # Remove failed bootstrap samples
  valid_boots <- complete.cases(x_aligned_errors)
  x_aligned_errors <- x_aligned_errors[valid_boots, , drop = FALSE]
  y_estimates <- y_estimates[valid_boots, , drop = FALSE]
  
  if (verbose && nrow(x_aligned_errors) > 0) {
    cat("X-aligned error statistics (", nrow(x_aligned_errors), "valid bootstraps):\n")
    for (j in 1:ncol(x_aligned_errors)) {
      var_x_u <- var(x_aligned_errors[, j], na.rm = TRUE)
      cat("  ", colnames(x_aligned_errors)[j], ": Var(X'u) =", round(var_x_u, 6), "\n")
    }
  }
  
  return(list(
    x_aligned_errors = x_aligned_errors,
    y_estimates = y_estimates,
    valid_bootstraps = nrow(x_aligned_errors),
    x_u_variance = apply(x_aligned_errors, 2, var, na.rm = TRUE)
  ))
}

# Per-coefficient exposure analysis
analyze_coefficient_exposure <- function(X_matrix, x_aligned_errors, verbose = TRUE) {
  
  if (nrow(x_aligned_errors) == 0) {
    if (verbose) cat("No valid bootstrap samples for exposure analysis\n")
    return(NULL)
  }
  
  # Compute (X'X)^{-1}
  XtX <- t(X_matrix) %*% X_matrix
  XtX_inv <- solve(XtX + diag(1e-10, ncol(XtX)))
  
  # Per-coefficient exposure: e_k' (X'X)^{-1} X'u for each bootstrap
  n_coefs <- ncol(X_matrix)
  coefficient_exposures <- matrix(NA, nrow = nrow(x_aligned_errors), ncol = n_coefs)
  colnames(coefficient_exposures) <- colnames(X_matrix)
  
  for (b in 1:nrow(x_aligned_errors)) {
    x_u_vector <- x_aligned_errors[b, ]
    
    # Each coefficient's exposure
    for (k in 1:n_coefs) {
      e_k <- numeric(n_coefs)
      e_k[k] <- 1
      
      coefficient_exposures[b, k] <- t(e_k) %*% XtX_inv %*% x_u_vector
    }
  }
  
  # Summary statistics
  exposure_means <- apply(coefficient_exposures, 2, mean, na.rm = TRUE)
  exposure_sds <- apply(coefficient_exposures, 2, sd, na.rm = TRUE)
  exposure_vars <- exposure_sds^2
  
  if (verbose) {
    cat("\nPER-COEFFICIENT EXPOSURE ANALYSIS:\n")
    for (k in 1:n_coefs) {
      cat(sprintf("%-25s: Mean=%.6f, SD=%.6f, Var=%.6f\n", 
                  colnames(coefficient_exposures)[k], 
                  exposure_means[k], exposure_sds[k], exposure_vars[k]))
    }
  }
  
  return(list(
    exposures = coefficient_exposures,
    means = exposure_means,
    variances = exposure_vars,
    most_exposed = which.max(abs(exposure_means))
  ))
}

# Adaptive domain weighting based on X-aligned error
adaptive_x_aligned_weights <- function(cij, X_matrix, current_pi_j, 
                                     adult_domains, target_coefficient = NULL) {
  
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Compute leverage scores
  XtX_inv <- solve(t(X_matrix) %*% X_matrix + diag(1e-10, ncol(X_matrix)))
  
  if (is.null(target_coefficient)) {
    # Use overall leverage
    leverage <- diag(X_matrix %*% XtX_inv %*% t(X_matrix))
  } else {
    # Focus on specific coefficient
    e_k <- numeric(ncol(X_matrix))
    e_k[target_coefficient] <- 1
    
    # Weight by exposure to this specific coefficient
    leverage <- numeric(n)
    for (i in 1:n) {
      x_i <- X_matrix[i, ]
      leverage[i] <- (t(e_k) %*% XtX_inv %*% x_i)^2
    }
  }
  
  # Total visits per user
  Ti <- rowSums(cij)
  
  # Domain weights targeting X-aligned error
  w_j_adaptive <- numeric(m)
  
  for (j in 1:m) {
    weight <- 0
    
    for (i in 1:n) {
      if (Ti[i] > 0) {
        proportion_ij <- cij[i, j] / Ti[i]
        
        # Weight by leverage and current variance contribution
        current_var_contribution <- (1 - current_pi_j[j]) / current_pi_j[j] * proportion_ij^2
        
        weight <- weight + leverage[i] * current_var_contribution
      }
    }
    
    w_j_adaptive[j] <- weight
  }
  
  return(w_j_adaptive)
}

# Hybrid Top-K + HT method
hybrid_topk_ht_method <- function(cij, X_matrix, adult_domains, budget, 
                                top_k_fraction = 0.3, verbose = TRUE) {
  
  if (verbose) {
    cat("=== HYBRID TOP-K + HT METHOD ===\n")
    cat("Budget:", budget, "domains\n")
    cat("Top-K fraction:", round(100 * top_k_fraction), "%\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Phase 1: Select top-K most popular domains (kills variance)
  domain_popularity <- colSums(cij)
  top_k_size <- round(budget * top_k_fraction)
  top_k_domains <- head(order(domain_popularity, decreasing = TRUE), top_k_size)
  
  # Phase 2: HT-weighted selection from remaining budget
  remaining_budget <- budget - top_k_size
  remaining_domains <- setdiff(1:m, top_k_domains)
  
  if (remaining_budget > 0 && length(remaining_domains) > 0) {
    
    # Compute weights for remaining domains
    weight_info <- robust_domain_weights(cij[, remaining_domains, drop = FALSE], X_matrix)
    w_remaining <- weight_info$weights
    
    # Set inclusion probabilities for remaining domains
    pi_remaining <- sqrt(w_remaining) * remaining_budget / sum(sqrt(w_remaining))
    pi_remaining <- pmin(pi_remaining, 0.95)
    
    # Sample from remaining domains
    selected_remaining_indices <- which(runif(length(remaining_domains)) < pi_remaining)
    selected_remaining <- remaining_domains[selected_remaining_indices]
    
  } else {
    selected_remaining <- c()
    pi_remaining <- numeric(0)
  }
  
  # Combine selections
  final_selected <- c(top_k_domains, selected_remaining)
  
  # Create full inclusion probability vector
  pi_j_full <- numeric(m)
  pi_j_full[top_k_domains] <- 1.0  # Top-K domains selected with certainty
  if (length(selected_remaining) > 0) {
    pi_j_full[remaining_domains] <- pi_remaining
  }
  
  if (verbose) {
    cat("- Top-K domains:", length(top_k_domains), "\n")
    cat("- HT-sampled domains:", length(selected_remaining), "\n")
    cat("- Total selected:", length(final_selected), "\n")
  }
  
  return(list(
    selected_domains = final_selected,
    inclusion_probs = pi_j_full,
    top_k_domains = top_k_domains,
    ht_domains = selected_remaining,
    method = "Hybrid_TopK_HT"
  ))
}

# Comprehensive X-aligned error test
test_x_aligned_optimization <- function(budget_fraction = 0.3) {
  
  cat("🎯 TESTING X-ALIGNED ERROR OPTIMIZATION\n")
  cat(rep("=", 75), "\n")
  
  # Generate improved data
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
  cat("- Target: Minimize X-aligned error component\n\n")
  
  # Ground truth
  lm_true <- lm(y_true ~ X_matrix[, -1])
  beta_true <- coef(lm_true)
  
  cat("Ground truth coefficients:\n")
  print(round(beta_true, 4))
  
  # Test methods
  methods <- list(
    "Standard_HT" = function() {
      weight_info <- robust_domain_weights(cij, X_matrix)
      pi_j <- robust_inclusion_probabilities(weight_info$weights, budget)
      selected <- which(runif(m) < pi_j)
      list(selected_domains = selected, inclusion_probs = pi_j, method = "Standard_HT")
    },
    
    "Hybrid_TopK_HT" = function() {
      hybrid_topk_ht_method(cij, X_matrix, adult_domains, budget, top_k_fraction = 0.3, verbose = FALSE)
    }
  )
  
  results <- list()
  
  for (method_name in names(methods)) {
    cat("\n", rep("-", 60), "\n")
    cat("METHOD:", method_name, "\n")
    
    # Apply method
    method_result <- methods[[method_name]]()
    selected_domains <- method_result$selected_domains
    pi_j <- method_result$inclusion_probs
    
    if (length(selected_domains) == 0) {
      cat("No domains selected, skipping\n")
      next
    }
    
    cat("Selected", length(selected_domains), "domains\n")
    
    # Compute X-aligned error analysis
    x_error_analysis <- compute_x_aligned_error(cij, X_matrix, adult_domains, 
                                              selected_domains, pi_j, 
                                              n_bootstrap = 30, verbose = TRUE)
    
    # Per-coefficient exposure
    exposure_analysis <- analyze_coefficient_exposure(X_matrix, x_error_analysis$x_aligned_errors, 
                                                    verbose = TRUE)
    
    # Standard regression analysis for comparison
    y_hat_mean <- colMeans(x_error_analysis$y_estimates, na.rm = TRUE)
    
    lm_estimated <- lm(y_hat_mean ~ X_matrix[, -1])
    beta_estimated <- coef(lm_estimated)
    
    # Calculate bias
    bias <- beta_estimated - beta_true
    pct_bias <- 100 * abs(bias) / abs(beta_true)
    pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
    
    max_bias <- max(pct_bias)
    
    cat("\nREGRESSION RESULTS:\n")
    cat("- Max coefficient bias:", round(max_bias, 1), "%\n")
    cat("- Mean Y correlation:", round(cor(y_hat_mean, y_true, use = "complete.obs"), 3), "\n")
    
    # Assessment
    if (!is.null(exposure_analysis)) {
      total_x_aligned_var <- sum(x_error_analysis$x_u_variance, na.rm = TRUE)
      max_exposure_var <- max(exposure_analysis$variances, na.rm = TRUE)
      
      cat("- Total X-aligned variance:", round(total_x_aligned_var, 6), "\n")
      cat("- Max coefficient exposure variance:", round(max_exposure_var, 6), "\n")
      
      if (max_bias < 30 && max_exposure_var < 0.001) {
        status <- "✅ EXCELLENT: Low bias, low X-aligned error"
      } else if (max_bias < 60 && max_exposure_var < 0.01) {
        status <- "👍 GOOD: Moderate performance"
      } else {
        status <- "⚠️ NEEDS IMPROVEMENT: High X-aligned error"
      }
    } else {
      status <- "❌ ANALYSIS FAILED"
    }
    
    cat("- Status:", status, "\n")
    
    results[[method_name]] <- list(
      domains = length(selected_domains),
      max_bias = max_bias,
      x_aligned_variance = if (!is.null(exposure_analysis)) sum(x_error_analysis$x_u_variance, na.rm = TRUE) else NA,
      max_exposure_variance = if (!is.null(exposure_analysis)) max(exposure_analysis$variances, na.rm = TRUE) else NA,
      status = status
    )
  }
  
  # Summary
  cat("\n", rep("=", 75), "\n")
  cat("X-ALIGNED ERROR OPTIMIZATION SUMMARY\n")
  cat(rep("=", 75), "\n")
  
  if (length(results) > 0) {
    for (name in names(results)) {
      r <- results[[name]]
      cat(sprintf("%-15s: %3d domains, %5.1f%% bias, X-var=%.6f, %s\n",
                  name, r$domains, r$max_bias, 
                  ifelse(is.na(r$x_aligned_variance), 0, r$x_aligned_variance),
                  r$status))
    }
  }
  
  return(results)
}

# Run the X-aligned optimization test
if (!interactive()) {
  cat("🚀 TESTING X-ALIGNED ERROR OPTIMIZATION\n\n")
  results <- test_x_aligned_optimization(budget_fraction = 0.3)
  
  cat("\n✅ X-ALIGNED ERROR OPTIMIZATION COMPLETE!\n")
  cat("Focus on the component of error that actually matters for coefficient bias.\n")
}