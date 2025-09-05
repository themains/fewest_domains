# Demonstration: How Small Row-Level Errors Create Large Regression Bias
# Shows the mathematical mechanism behind bias amplification

source("synthetic_data.R")
source("main_methods_interface.R")

demonstrate_bias_mechanism <- function() {
  
  cat("🔬 DEMONSTRATING: How Small Errors → Large Regression Bias\n")
  cat(rep("=", 80), "\n")
  
  # Generate clean test data
  data <- generate_regression_data(n_respondents = 200, n_domains = 300, seed = 999)
  
  cij <- data$cij
  X_matrix <- data$X_matrix
  y_true <- data$y_true
  adult_domains <- which(data$domain_types == "adult")
  
  # Ground truth regression
  lm_true <- lm(y_true ~ X_matrix[,-1])
  beta_true <- coef(lm_true)
  
  cat("GROUND TRUTH:\n")
  print(round(beta_true, 4))
  
  # Apply optimal method
  selected_domains <- optimal_method_simple(cij, 0.1)
  
  cat("\nOPTIMAL METHOD RESULTS:\n")
  cat("- Selected", length(selected_domains), "/", ncol(cij), "domains\n")
  cat("- Efficiency:", round(100 * length(selected_domains) / ncol(cij), 1), "%\n")
  
  # Calculate estimated proportions
  y_estimated <- numeric(nrow(cij))
  row_errors <- numeric(nrow(cij))
  
  for (i in 1:nrow(cij)) {
    visits_selected <- sum(cij[i, selected_domains])
    
    if (visits_selected > 0) {
      adult_visits_selected <- sum(cij[i, intersect(selected_domains, adult_domains)])
      y_estimated[i] <- adult_visits_selected / visits_selected
    } else {
      y_estimated[i] <- 0
    }
    
    row_errors[i] <- y_estimated[i] - y_true[i]
  }
  
  # Show row-level errors
  cat("\nROW-LEVEL ERROR ANALYSIS:\n")
  cat("- Mean absolute error per row:", round(mean(abs(row_errors)), 4), "\n")
  cat("- Max absolute error per row:", round(max(abs(row_errors)), 4), "\n") 
  cat("- Standard deviation of errors:", round(sd(row_errors), 4), "\n")
  
  # The key: correlation with covariates
  cat("\nCORRELATION ANALYSIS (the smoking gun!):\n")
  
  age_col <- which(grepl("age", colnames(X_matrix)))[1]
  tech_col <- which(grepl("tech", colnames(X_matrix)))[1] 
  privacy_col <- which(grepl("privacy", colnames(X_matrix)))[1]
  
  if (length(age_col) > 0) {
    age_cor <- cor(row_errors, X_matrix[, age_col], use = "complete.obs")
    cat("- Error correlation with age:", round(age_cor, 4), "\n")
  }
  
  if (length(tech_col) > 0) {
    tech_cor <- cor(row_errors, X_matrix[, tech_col], use = "complete.obs") 
    cat("- Error correlation with tech savviness:", round(tech_cor, 4), "\n")
  }
  
  if (length(privacy_col) > 0) {
    privacy_cor <- cor(row_errors, X_matrix[, privacy_col], use = "complete.obs")
    cat("- Error correlation with privacy concern:", round(privacy_cor, 4), "\n")
  }
  
  # Regression analysis
  lm_estimated <- lm(y_estimated ~ X_matrix[,-1])
  beta_estimated <- coef(lm_estimated)
  
  cat("\nREGRESSION COEFFICIENT COMPARISON:\n")
  bias <- beta_estimated - beta_true
  pct_bias <- 100 * bias / abs(beta_true)
  pct_bias[is.infinite(pct_bias) | is.na(pct_bias)] <- 0
  
  comparison_df <- data.frame(
    Coefficient = names(beta_true),
    True_Beta = round(beta_true, 4),
    Estimated_Beta = round(beta_estimated, 4),
    Bias = round(bias, 4),
    Percent_Bias = round(pct_bias, 1)
  )
  
  print(comparison_df)
  
  cat("\n🔍 THE MATHEMATICAL MECHANISM:\n")
  cat("1. Small row errors (", round(mean(abs(row_errors)), 3), " average) seem harmless\n")
  cat("2. BUT these errors are CORRELATED with user characteristics\n")
  cat("3. When we regress y_estimated ~ X, these correlations bias coefficients\n")
  cat("4. Small systematic errors × correlation = Large coefficient bias\n")
  
  # Demonstrate with a simple example
  cat("\n📐 SIMPLIFIED DEMONSTRATION:\n")
  cat("Imagine errors are +0.01 for tech-savvy users, -0.01 for non-tech-savvy\n")
  cat("Average error is tiny, but it's perfectly correlated with tech_savvy!\n")
  cat("This creates bias in the tech_savvy coefficient.\n")
  
  # Show the specific bias pattern
  cat("\n🎯 SELECTION BIAS PATTERN ANALYSIS:\n")
  
  # Group users by key characteristics and show systematic error patterns
  if (length(age_col) > 0) {
    age_values <- X_matrix[, age_col]
    age_groups <- cut(age_values, breaks = 3, labels = c("Young", "Middle", "Old"))
    
    cat("Error by age group:\n")
    for (group in levels(age_groups)) {
      group_errors <- row_errors[age_groups == group]
      if (length(group_errors) > 0) {
        cat("  ", group, ": mean error =", round(mean(group_errors), 4), "\n")
      }
    }
  }
  
  if (length(tech_col) > 0) {
    tech_values <- X_matrix[, tech_col] 
    tech_groups <- cut(tech_values, breaks = 3, labels = c("Low_Tech", "Mid_Tech", "High_Tech"))
    
    cat("Error by tech savviness:\n")
    for (group in levels(tech_groups)) {
      group_errors <- row_errors[tech_groups == group]
      if (length(group_errors) > 0) {
        cat("  ", group, ": mean error =", round(mean(group_errors), 4), "\n")
      }
    }
  }
  
  cat("\n💡 KEY INSIGHT:\n")
  cat("The 'optimal' domain selection systematically favors domains used by certain user types.\n")
  cat("This creates a pattern where errors are NOT random - they're correlated with X.\n")
  cat("Even tiny systematic errors (0.01) can create 100%+ coefficient bias!\n")
  
  # Test with artificial correlation
  cat("\n🧪 CONTROLLED EXPERIMENT:\n")
  cat("What happens if we artificially create tiny systematic errors?\n")
  
  # Create artificial tiny errors correlated with age
  if (length(age_col) > 0) {
    artificial_errors <- 0.005 * scale(X_matrix[, age_col])[,1]  # Tiny errors!
    y_artificial <- y_true + artificial_errors
    
    lm_artificial <- lm(y_artificial ~ X_matrix[,-1])
    beta_artificial <- coef(lm_artificial)
    artificial_bias <- 100 * (beta_artificial - beta_true) / abs(beta_true)
    artificial_bias[is.infinite(artificial_bias) | is.na(artificial_bias)] <- 0
    
    cat("With artificial 0.005 systematic errors:\n")
    cat("- Age coefficient bias:", round(artificial_bias[2], 1), "%\n")
    cat("- Max coefficient bias:", round(max(abs(artificial_bias)), 1), "%\n")
    cat("- Average artificial error:", round(mean(abs(artificial_errors)), 5), "\n")
  }
  
  return(list(
    row_errors = row_errors,
    beta_bias_pct = pct_bias,
    max_bias = max(abs(pct_bias))
  ))
}

# Run the demonstration
if (!interactive()) {
  results <- demonstrate_bias_mechanism()
  
  cat("\n🎯 SUMMARY:\n")
  cat("Small systematic errors (correlated with X) create large coefficient bias.\n")
  cat("This is a fundamental property of regression analysis, not a bug!\n")
}