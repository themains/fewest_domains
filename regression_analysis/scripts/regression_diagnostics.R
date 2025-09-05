# Regression Diagnostics for Domain Selection
# Tests if domain selection methods preserve regression validity

source("../scripts/domain_optimization.R") # Main domain selection methods
source("synthetic_data.R")

# Test coefficient recovery after domain selection
test_coefficient_recovery <- function(data, target_se = 0.1, method = "optimal") {
  
  cat("=== TESTING COEFFICIENT RECOVERY ===\n")
  cat("Method:", method, "| Target SE:", target_se, "\n")
  
  # Extract data
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  beta_true <- data$beta_true
  
  n_respondents <- nrow(cij)
  
  # Step 1: Estimate regression on TRUE data (ground truth)
  lm_true <- lm(y_true ~ X_matrix[,-1]) # Remove intercept column (already in X_matrix)
  beta_true_estimated <- coef(lm_true)
  
  cat("\nTRUE DATA REGRESSION:\n")
  print(summary(lm_true)$coefficients[,1:2])
  
  # Step 2: Apply domain selection
  if (method == "naive") {
    selection_result <- naive_method(cij, target_se, verbose = FALSE)
  } else if (method == "optimal") {
    selection_result <- optimal_method(cij, target_se, verbose = FALSE)  
  } else if (method == "core_tail") {
    selection_result <- core_tail_method(cij, target_se, verbose = FALSE)
  } else {
    stop("Unknown method: ", method)
  }
  
  selected_domains <- selection_result$selected_domains
  
  cat("\nDOMAIN SELECTION:\n")
  cat("- Selected domains:", length(selected_domains), "/", ncol(cij), "\n")
  cat("- Constraints satisfied:", selection_result$constraints_satisfied, "\n")
  
  # Step 3: Estimate proportions using selected domains
  y_estimated <- numeric(n_respondents)
  
  for (i in 1:n_respondents) {
    visits_selected <- sum(cij[i, selected_domains])
    
    if (visits_selected > 0) {
      # For testing, assume we know adult domains (in practice would code manually)
      adult_domains <- which(data$domain_types == "adult")
      adult_visits_selected <- sum(cij[i, intersect(selected_domains, adult_domains)])
      
      y_estimated[i] <- adult_visits_selected / visits_selected
    } else {
      y_estimated[i] <- 0
    }
  }
  
  # Step 4: Estimate regression on SAMPLED data
  lm_sampled <- lm(y_estimated ~ X_matrix[,-1])
  beta_sampled <- coef(lm_sampled)
  
  cat("\nSAMPLED DATA REGRESSION:\n") 
  print(summary(lm_sampled)$coefficients[,1:2])
  
  # Step 5: Compare coefficients
  cat("\n", rep("=", 60), "\n")
  cat("COEFFICIENT COMPARISON\n")
  cat(rep("=", 60), "\n")
  
  coef_names <- names(beta_true_estimated)
  comparison <- data.frame(
    Coefficient = coef_names,
    True_Data = beta_true_estimated,
    Sampled_Data = beta_sampled,
    Difference = beta_sampled - beta_true_estimated,
    Pct_Change = 100 * (beta_sampled - beta_true_estimated) / abs(beta_true_estimated),
    stringsAsFactors = FALSE
  )
  
  print(comparison)
  
  # Step 6: Test for selection bias
  selection_errors <- y_estimated - y_true
  
  # Test if errors correlate with covariates
  error_regression <- lm(selection_errors ~ X_matrix[,-1])
  
  cat("\n", rep("=", 60), "\n") 
  cat("SELECTION BIAS DIAGNOSTICS\n")
  cat(rep("=", 60), "\n")
  
  cat("Selection error statistics:\n")
  cat("- Mean error:", round(mean(selection_errors), 6), "\n")
  cat("- RMS error:", round(sqrt(mean(selection_errors^2)), 6), "\n")
  cat("- Max |error|:", round(max(abs(selection_errors)), 6), "\n")
  
  # F-test for overall correlation
  f_stat <- summary(error_regression)$fstatistic
  if (!is.null(f_stat)) {
    f_pvalue <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    cat("\nError-covariate correlation test:\n")
    cat("- F-statistic:", round(f_stat[1], 3), "\n")
    cat("- p-value:", round(f_pvalue, 6), "\n")
    cat("- Significant correlation:", ifelse(f_pvalue < 0.05, "YES (problematic)", "NO (good)"), "\n")
    
    if (f_pvalue < 0.05) {
      cat("\n⚠️  WARNING: Selection errors correlate with covariates!\n")
      cat("This indicates potential regression bias.\n")
      
      # Show which covariates are problematic
      error_coefs <- summary(error_regression)$coefficients
      sig_vars <- which(error_coefs[,4] < 0.05)[-1] # Exclude intercept
      
      if (length(sig_vars) > 0) {
        cat("Problematic covariates:\n")
        for (var_idx in sig_vars) {
          var_name <- rownames(error_coefs)[var_idx + 1]
          coef_val <- error_coefs[var_idx + 1, 1]
          p_val <- error_coefs[var_idx + 1, 4]
          cat("-", var_name, ": coef =", round(coef_val, 6), ", p =", round(p_val, 4), "\n")
        }
      }
    }
  }
  
  # Step 7: Overall assessment
  max_coef_change <- max(abs(comparison$Pct_Change), na.rm = TRUE)
  
  cat("\n", rep("=", 60), "\n")
  cat("OVERALL ASSESSMENT\n") 
  cat(rep("=", 60), "\n")
  
  cat("Domain selection efficiency:\n")
  cat("- Domains used:", length(selected_domains), "/", ncol(cij), 
      "(", round(100 * length(selected_domains) / ncol(cij), 1), "%)\n")
  
  cat("\nRegression validity:\n")
  cat("- Max coefficient change:", round(max_coef_change, 1), "%\n")
  cat("- Error-covariate correlation p-value:", round(f_pvalue, 6), "\n")
  
  valid_regression <- f_pvalue >= 0.05 && max_coef_change < 10
  cat("- Regression validity:", ifelse(valid_regression, "✅ PASS", "❌ FAIL"), "\n")
  
  return(list(
    method = method,
    domains_selected = length(selected_domains),
    domains_total = ncol(cij),
    efficiency = 100 * length(selected_domains) / ncol(cij),
    coefficient_comparison = comparison,
    max_coefficient_change = max_coef_change,
    selection_errors = selection_errors,
    error_correlation_pvalue = f_pvalue,
    regression_valid = valid_regression,
    constraints_satisfied = selection_result$constraints_satisfied
  ))
}

# Compare multiple methods for regression validity
compare_methods_regression <- function(data, target_se = 0.1) {
  
  cat("🔍 COMPARING METHODS FOR REGRESSION VALIDITY\n")
  cat(rep("=", 70), "\n")
  
  methods <- c("naive", "optimal", "core_tail")
  results <- list()
  
  for (method in methods) {
    cat("\n")
    results[[method]] <- test_coefficient_recovery(data, target_se, method)
  }
  
  # Summary comparison
  cat("\n", rep("=", 70), "\n")
  cat("METHOD COMPARISON SUMMARY\n")
  cat(rep("=", 70), "\n")
  
  cat(sprintf("%-12s %8s %15s %12s %12s\n", 
              "Method", "Domains", "Efficiency%", "MaxCoefΔ%", "RegValid"))
  cat(rep("-", 65), "\n")
  
  for (method in methods) {
    r <- results[[method]]
    cat(sprintf("%-12s %8d %15.1f %12.1f %12s\n",
                method, r$domains_selected, 100 - r$efficiency, 
                r$max_coefficient_change, ifelse(r$regression_valid, "✅", "❌")))
  }
  
  return(results)
}

# Run diagnostics if script executed directly
if (!interactive()) {
  cat("📊 REGRESSION DIAGNOSTICS FOR DOMAIN SELECTION\n")
  cat("Testing if domain selection preserves regression validity...\n\n")
  
  # Generate test data
  data <- generate_regression_data(n_respondents = 150, n_domains = 300)
  
  # Test all methods
  comparison_results <- compare_methods_regression(data)
  
  cat("\n✅ REGRESSION DIAGNOSTICS COMPLETE!\n")
  cat("Analysis shows whether domain selection introduces regression bias.\n")
}