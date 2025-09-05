# Complete X-Aligned Error Optimization
# Focuses on minimizing ‖X'u‖² - the component of error that biases coefficients

source("improved_dgp.R")

# Robust domain weight computation (imported function)
robust_domain_weights <- function(cij, X_matrix, regularization = 1e-8) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  Ti <- rowSums(cij)
  
  X_design <- X_matrix
  if (!all(X_design[, 1] == 1)) {
    X_design <- cbind(1, X_matrix[, -1])
  }
  
  XtX <- t(X_design) %*% X_design
  XtX_reg <- XtX + diag(regularization, ncol(XtX))
  XtX_inv <- solve(XtX_reg)
  
  leverage <- diag(X_design %*% XtX_inv %*% t(X_design))
  
  w_j <- numeric(m)
  for (j in 1:m) {
    weight <- 0
    for (i in 1:n) {
      if (Ti[i] > 0) {
        proportion_ij <- cij[i, j] / (Ti[i] + 1e-10)
        contribution <- leverage[i] * proportion_ij^2
        contribution <- max(contribution, 1e-12)
        weight <- weight + contribution
      }
    }
    w_j[j] <- weight
  }
  
  w_j <- pmax(w_j, 1e-10 * max(w_j))
  
  return(list(weights = w_j, leverage = leverage, total_visits = Ti))
}

# Robust inclusion probabilities  
robust_inclusion_probabilities <- function(w_j, budget, min_prob = 0.001, max_prob = 0.95) {
  sqrt_weights <- sqrt(pmax(w_j, 1e-10))
  pi_j <- sqrt_weights * budget / sum(sqrt_weights)
  pi_j <- pmax(pi_j, min_prob)
  pi_j <- pmin(pi_j, max_prob)
  scaling_factor <- budget / sum(pi_j)
  pi_j <- pi_j * scaling_factor
  pi_j <- pmin(pi_j, max_prob)
  return(pi_j)
}

# Direct computation of X'u from bootstrap samples
compute_xu_directly <- function(cij, X_matrix, adult_domains, selected_domains, pi_j, 
                               n_bootstrap = 40, verbose = TRUE) {
  
  n <- nrow(cij)
  m <- ncol(cij)
  
  # True proportions
  y_true <- numeric(n)
  for (i in 1:n) {
    if (sum(cij[i, ]) > 0) {
      y_true[i] <- sum(cij[i, adult_domains]) / sum(cij[i, ])
    }
  }
  
  # Bootstrap to get distribution of X'u
  xu_bootstrap <- matrix(NA, nrow = n_bootstrap, ncol = ncol(X_matrix))
  colnames(xu_bootstrap) <- colnames(X_matrix)
  
  y_bootstrap <- matrix(NA, nrow = n_bootstrap, ncol = n)
  
  for (b in 1:n_bootstrap) {
    # Resample domains with their inclusion probabilities
    boot_selected <- which(runif(m) < pi_j)
    
    if (length(boot_selected) == 0) next
    
    # HT estimates for this bootstrap sample
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
          ht_weight <- min(1/pi_j[j], 25)  # Winsorize
          Ni_hat <- Ni_hat + (cij[i, j] * A_j * ht_weight)
        }
      }
      y_boot[i] <- Ni_hat / Ti
    }
    
    y_bootstrap[b, ] <- y_boot
    
    # Compute u = y_hat - y_true
    errors <- y_boot - y_true
    
    # Compute X'u (the critical quantity)
    xu_bootstrap[b, ] <- t(X_matrix) %*% errors
  }
  
  # Remove failed bootstraps
  valid_boots <- complete.cases(xu_bootstrap)
  xu_bootstrap <- xu_bootstrap[valid_boots, , drop = FALSE]
  y_bootstrap <- y_bootstrap[valid_boots, , drop = FALSE]
  
  if (verbose && nrow(xu_bootstrap) > 0) {
    cat("X'u analysis (", nrow(xu_bootstrap), "valid bootstraps):\n")
    
    xu_norms <- sqrt(rowSums(xu_bootstrap^2))
    cat("  ‖X'u‖ range: [", round(min(xu_norms), 4), ", ", round(max(xu_norms), 4), "]\n")
    cat("  ‖X'u‖ mean: ", round(mean(xu_norms), 4), "\n")
    
    for (j in 1:ncol(xu_bootstrap)) {
      xu_var <- var(xu_bootstrap[, j], na.rm = TRUE)
      cat("  ", colnames(xu_bootstrap)[j], " component: Var=", round(xu_var, 6), "\n")
    }
  }
  
  return(list(
    xu_matrix = xu_bootstrap,
    y_bootstrap = y_bootstrap,
    valid_bootstraps = nrow(xu_bootstrap),
    xu_component_vars = if (nrow(xu_bootstrap) > 0) apply(xu_bootstrap, 2, var, na.rm = TRUE) else rep(NA, ncol(X_matrix)),
    xu_norm_mean = if (nrow(xu_bootstrap) > 0) mean(sqrt(rowSums(xu_bootstrap^2))) else NA
  ))
}

# Per-coefficient bias analysis: e_k' (X'X)^{-1} X'u
per_coefficient_bias_analysis <- function(X_matrix, xu_matrix, verbose = TRUE) {
  
  if (nrow(xu_matrix) == 0) {
    if (verbose) cat("No valid bootstrap samples for bias analysis\n")
    return(NULL)
  }
  
  # (X'X)^{-1} for coefficient extraction
  XtX <- t(X_matrix) %*% X_matrix
  XtX_inv <- solve(XtX + diag(1e-10, ncol(XtX)))
  
  # For each coefficient k, compute e_k' (X'X)^{-1} X'u across bootstraps
  n_coefs <- ncol(X_matrix)
  coef_biases <- matrix(NA, nrow = nrow(xu_matrix), ncol = n_coefs)
  colnames(coef_biases) <- colnames(X_matrix)
  
  for (b in 1:nrow(xu_matrix)) {
    xu_vector <- xu_matrix[b, ]
    
    # Coefficient bias vector: (X'X)^{-1} X'u
    bias_vector <- XtX_inv %*% xu_vector
    coef_biases[b, ] <- bias_vector
  }
  
  # Summary statistics
  bias_means <- apply(coef_biases, 2, mean, na.rm = TRUE)
  bias_sds <- apply(coef_biases, 2, sd, na.rm = TRUE)
  bias_vars <- bias_sds^2
  
  if (verbose) {
    cat("\nPER-COEFFICIENT BIAS FROM X-ALIGNED ERROR:\n")
    for (k in 1:n_coefs) {
      cat(sprintf("  %-25s: Mean=%.6f, SD=%.6f, Var=%.8f\n", 
                  colnames(coef_biases)[k], bias_means[k], bias_sds[k], bias_vars[k]))
    }
    
    # Identify most problematic coefficient
    worst_coef_idx <- which.max(bias_vars)
    cat(sprintf("  Most exposed: %s (Var=%.8f)\n", 
                colnames(coef_biases)[worst_coef_idx], bias_vars[worst_coef_idx]))
  }
  
  return(list(
    coefficient_biases = coef_biases,
    bias_means = bias_means,
    bias_variances = bias_vars,
    worst_coefficient = which.max(bias_vars)
  ))
}

# Hybrid Top-K + HT approach to minimize X-aligned error
hybrid_approach_xu <- function(cij, X_matrix, adult_domains, budget, 
                              top_k_fraction = 0.4, verbose = TRUE) {
  
  if (verbose) {
    cat("=== HYBRID TOP-K + HT (X-ALIGNED FOCUS) ===\n")
  }
  
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Phase 1: Top-K popular domains (variance reduction)
  domain_popularity <- colSums(cij)
  top_k_size <- round(budget * top_k_fraction)
  top_k_domains <- head(order(domain_popularity, decreasing = TRUE), top_k_size)
  
  # Phase 2: HT-weighted from remainder (bias protection)
  remaining_budget <- budget - top_k_size
  remaining_domains <- setdiff(1:m, top_k_domains)
  
  if (remaining_budget > 0 && length(remaining_domains) > 0) {
    # Focus on X-aligned error for remaining selection
    weight_info <- robust_domain_weights(cij[, remaining_domains, drop = FALSE], X_matrix)
    w_remaining <- weight_info$weights
    
    pi_remaining <- robust_inclusion_probabilities(w_remaining, remaining_budget)
    selected_remaining_idx <- which(runif(length(remaining_domains)) < pi_remaining)
    selected_remaining <- remaining_domains[selected_remaining_idx]
    
    # Full inclusion probability vector
    pi_j_full <- numeric(m)
    pi_j_full[top_k_domains] <- 1.0
    pi_j_full[remaining_domains] <- pi_remaining
  } else {
    selected_remaining <- c()
    pi_j_full <- numeric(m)
    pi_j_full[top_k_domains] <- 1.0
  }
  
  final_selected <- c(top_k_domains, selected_remaining)
  
  if (verbose) {
    cat("  Top-K:", length(top_k_domains), " | HT-sampled:", length(selected_remaining), 
        " | Total:", length(final_selected), "\n")
  }
  
  return(list(
    selected_domains = final_selected,
    inclusion_probs = pi_j_full,
    top_k_domains = top_k_domains,
    ht_domains = selected_remaining
  ))
}

# Main test function
test_xu_minimization <- function(budget_fraction = 0.3) {
  
  cat("🎯 TESTING X'u MINIMIZATION APPROACH\n")
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
  cat("- Users:", n, " | Domains:", m, " | Budget:", budget, "\n")
  cat("- Target: Minimize ‖X'u‖² (X-aligned error component)\n")
  cat("- Adult domains:", length(adult_domains), "\n\n")
  
  # Ground truth
  lm_true <- lm(y_true ~ X_matrix[, -1])
  beta_true <- coef(lm_true)
  
  cat("Ground truth coefficients:\n")
  print(round(beta_true, 4))
  
  # Compare methods
  methods <- list(
    "Standard_HT" = function() {
      weight_info <- robust_domain_weights(cij, X_matrix)
      pi_j <- robust_inclusion_probabilities(weight_info$weights, budget)
      selected <- which(runif(m) < pi_j)
      list(selected_domains = selected, inclusion_probs = pi_j)
    },
    
    "Hybrid_40_60" = function() {
      hybrid_approach_xu(cij, X_matrix, adult_domains, budget, top_k_fraction = 0.4, verbose = FALSE)
    },
    
    "Hybrid_60_40" = function() {
      hybrid_approach_xu(cij, X_matrix, adult_domains, budget, top_k_fraction = 0.6, verbose = FALSE)
    }
  )
  
  results <- list()
  
  for (method_name in names(methods)) {
    cat("\n", rep("-", 50), "\n")
    cat("METHOD:", method_name, "\n")
    
    set.seed(2024)  # For reproducible comparison
    method_result <- methods[[method_name]]()
    selected_domains <- method_result$selected_domains
    pi_j <- method_result$inclusion_probs
    
    if (length(selected_domains) == 0) {
      cat("No domains selected\n")
      next
    }
    
    cat("Selected", length(selected_domains), "domains\n")
    
    # X'u analysis
    xu_analysis <- compute_xu_directly(cij, X_matrix, adult_domains, 
                                      selected_domains, pi_j, n_bootstrap = 25, verbose = TRUE)
    
    # Per-coefficient bias analysis
    if (xu_analysis$valid_bootstraps > 0) {
      coef_analysis <- per_coefficient_bias_analysis(X_matrix, xu_analysis$xu_matrix, verbose = TRUE)
      
      # Standard regression comparison
      y_mean_estimate <- colMeans(xu_analysis$y_bootstrap, na.rm = TRUE)
      lm_estimated <- lm(y_mean_estimate ~ X_matrix[, -1])
      beta_estimated <- coef(lm_estimated)
      
      bias <- beta_estimated - beta_true
      pct_bias <- 100 * abs(bias) / abs(beta_true)
      pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
      
      max_bias <- max(pct_bias)
      
      cat("\nSTANDARD REGRESSION RESULTS:\n")
      cat("  Max coefficient bias:", round(max_bias, 1), "%\n")
      cat("  Y correlation:", round(cor(y_mean_estimate, y_true, use = "complete.obs"), 3), "\n")
      
      # Overall assessment based on X-aligned error
      xu_norm_mean <- xu_analysis$xu_norm_mean
      max_coef_bias_var <- if (!is.null(coef_analysis)) max(coef_analysis$bias_variances, na.rm = TRUE) else NA
      
      if (max_bias < 25 && xu_norm_mean < 0.5) {
        status <- "✅ EXCELLENT: Low bias, small X'u"
      } else if (max_bias < 50 && xu_norm_mean < 1.0) {
        status <- "👍 GOOD: Reasonable performance"
      } else if (max_bias < 100) {
        status <- "⚠️ MODERATE: Some issues remain"
      } else {
        status <- "❌ POOR: High bias, large X'u"
      }
      
      cat("  Status:", status, "\n")
      
      results[[method_name]] <- list(
        domains = length(selected_domains),
        max_bias_pct = max_bias,
        xu_norm_mean = xu_norm_mean,
        max_coef_bias_var = max_coef_bias_var,
        status = status
      )
    }
  }
  
  # Summary comparison
  if (length(results) > 0) {
    cat("\n", rep("=", 75), "\n")
    cat("X'u MINIMIZATION COMPARISON\n")
    cat(rep("=", 75), "\n")
    
    cat(sprintf("%-15s %8s %10s %12s %15s\n", 
                "Method", "Domains", "MaxBias%", "‖X'u‖_mean", "Status"))
    cat(rep("-", 75), "\n")
    
    for (name in names(results)) {
      r <- results[[name]]
      cat(sprintf("%-15s %8d %10.1f %12.4f %15s\n",
                  name, r$domains, r$max_bias_pct, r$xu_norm_mean, r$status))
    }
    
    cat("\n💡 KEY INSIGHT:\n")
    cat("Methods that achieve smaller ‖X'u‖ should have lower coefficient bias.\n")
    cat("This directly targets the component of error that matters for regression.\n")
  }
  
  return(results)
}

# Run the test
if (!interactive()) {
  cat("🚀 TESTING COMPLETE X-ALIGNED ERROR MINIMIZATION\n\n")
  
  # Test multiple budget levels
  for (budget_frac in c(0.2, 0.25, 0.3)) {
    cat("\n", rep("=", 60), "\n")
    cat("BUDGET:", round(100 * budget_frac), "% OF DOMAINS\n") 
    cat(rep("=", 60), "\n")
    results <- test_xu_minimization(budget_fraction = budget_frac)
  }
  
  cat("\n✅ X'u MINIMIZATION TESTING COMPLETE!\n")
  cat("This approach directly targets ‖X'u‖² - the source of coefficient bias.\n")
}