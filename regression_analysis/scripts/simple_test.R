# Simple Test of Regression Bias Concept
# Demonstrates the core issue without relying on complex domain selection

source("synthetic_data.R")

# Simple domain selection method for testing
simple_random_selection <- function(cij, n_select) {
  # Randomly select domains (baseline)
  total_domains <- ncol(cij)
  selected <- sample(1:total_domains, min(n_select, total_domains))
  return(selected)
}

simple_popular_selection <- function(cij, n_select) {
  # Select most popular domains (may introduce bias)
  domain_popularity <- colSums(cij)
  selected <- head(order(domain_popularity, decreasing = TRUE), n_select)
  return(selected)
}

# Test regression bias from domain selection
test_regression_bias_simple <- function() {
  
  cat("🔬 TESTING REGRESSION BIAS FROM DOMAIN SELECTION\n")
  cat(rep("=", 60), "\n")
  
  # Generate synthetic data
  data <- generate_regression_data(n_respondents = 200, n_domains = 400, seed = 123)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- which(data$domain_types == "adult")
  
  cat("Setup:\n")
  cat("- Respondents:", nrow(cij), "\n")
  cat("- Total domains:", ncol(cij), "\n") 
  cat("- Adult domains:", length(adult_domains), "\n")
  
  # Ground truth regression
  lm_true <- lm(y_true ~ X_matrix[,-1])
  beta_true <- coef(lm_true)
  
  cat("\nGROUND TRUTH REGRESSION (using all domains):\n")
  print(round(beta_true, 4))
  
  # Test different selection strategies
  strategies <- list(
    "Random_50" = function() simple_random_selection(cij, 50),
    "Random_100" = function() simple_random_selection(cij, 100), 
    "Popular_50" = function() simple_popular_selection(cij, 50),
    "Popular_100" = function() simple_popular_selection(cij, 100)
  )
  
  results <- list()
  
  for (strategy_name in names(strategies)) {
    cat("\n", rep("-", 40), "\n")
    cat("STRATEGY:", strategy_name, "\n")
    
    # Apply selection strategy
    selected_domains <- strategies[[strategy_name]]()
    
    # Estimate proportions using selected domains
    y_estimated <- numeric(nrow(cij))
    
    for (i in 1:nrow(cij)) {
      visits_selected <- sum(cij[i, selected_domains])
      
      if (visits_selected > 0) {
        adult_visits_selected <- sum(cij[i, intersect(selected_domains, adult_domains)])
        y_estimated[i] <- adult_visits_selected / visits_selected
      } else {
        y_estimated[i] <- 0
      }
    }
    
    # Run regression on estimated data
    lm_estimated <- lm(y_estimated ~ X_matrix[,-1])
    beta_estimated <- coef(lm_estimated)
    
    # Calculate bias
    bias <- beta_estimated - beta_true
    pct_bias <- 100 * bias / abs(beta_true)
    pct_bias[is.infinite(pct_bias)] <- NA
    
    cat("Domains selected:", length(selected_domains), "/", ncol(cij), "\n")
    cat("Coefficient bias (%):\n")
    
    bias_summary <- data.frame(
      Coefficient = names(beta_true),
      True = round(beta_true, 4),
      Estimated = round(beta_estimated, 4),
      Bias_Pct = round(pct_bias, 1)
    )
    print(bias_summary)
    
    # Test error correlation with covariates
    selection_errors <- y_estimated - y_true
    error_lm <- lm(selection_errors ~ X_matrix[,-1])
    f_test <- summary(error_lm)$fstatistic
    
    if (!is.null(f_test)) {
      f_pvalue <- pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)
      cat("Error-covariate correlation p-value:", round(f_pvalue, 6), "\n")
      
      if (f_pvalue < 0.05) {
        cat("⚠️  Selection errors correlate with covariates (problematic)\n")
      } else {
        cat("✅ Selection errors uncorrelated with covariates (good)\n")
      }
    }
    
    results[[strategy_name]] <- list(
      domains_selected = length(selected_domains),
      beta_bias = bias,
      pct_bias = pct_bias,
      max_abs_bias_pct = max(abs(pct_bias), na.rm = TRUE),
      error_correlation_p = f_pvalue,
      problematic = !is.null(f_pvalue) && f_pvalue < 0.05
    )
  }
  
  # Summary
  cat("\n", rep("=", 60), "\n")
  cat("SUMMARY: REGRESSION BIAS BY SELECTION STRATEGY\n")
  cat(rep("=", 60), "\n")
  
  cat(sprintf("%-15s %10s %15s %15s\n", "Strategy", "Domains", "MaxBias%", "Problematic"))
  cat(rep("-", 60), "\n")
  
  for (name in names(results)) {
    r <- results[[name]]
    cat(sprintf("%-15s %10d %15.1f %15s\n", 
                name, r$domains_selected, r$max_abs_bias_pct,
                ifelse(r$problematic, "❌ YES", "✅ NO")))
  }
  
  return(results)
}

# Run the test
if (!interactive()) {
  results <- test_regression_bias_simple()
  
  cat("\n💡 KEY INSIGHTS:\n")
  cat("- Random selection typically preserves regression validity\n")
  cat("- Popular domain selection may introduce systematic bias\n") 
  cat("- Bias occurs when selection correlates with covariates\n")
  cat("- Need regression-aware selection methods for downstream analysis\n")
}