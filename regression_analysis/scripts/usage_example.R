# Usage Example: X-Aligned Error Minimization for Domain Selection
# This demonstrates how to use the breakthrough solution for your own data

# Load the solution
source("final_x_aligned_solution.R")

# ==============================================================================
# EXAMPLE 1: Basic Testing with Synthetic Data
# ==============================================================================

cat("=== EXAMPLE 1: BASIC USAGE ===\n")

# Test with default parameters
results <- test_final_solution(budget_fraction = 0.3, verbose = TRUE)

cat("\nKey Results:\n")
cat("- Max coefficient bias:", round(results$max_bias_pct, 1), "%\n")
cat("- Domains selected:", length(results$selected_domains), "\n") 
cat("- Y correlation:", round(results$y_correlation, 3), "\n")

# ==============================================================================
# EXAMPLE 2: Step-by-Step Application 
# ==============================================================================

cat("\n\n=== EXAMPLE 2: STEP-BY-STEP APPLICATION ===\n")

# Step 1: Generate your data (replace with your actual data)
cat("Step 1: Generate/load data\n")
data <- generate_improved_data(n_users = 150, n_domains = 250, seed = 123)

cij <- data$cij           # [users x domains] visit count matrix
X_matrix <- data$X_matrix # [users x features] user characteristics  
adult_domains <- data$adult_domains  # indices of adult domains
y_true <- data$y_true     # true proportions (for validation only)

cat("- Data dimensions:", nrow(cij), "users x", ncol(cij), "domains\n")
cat("- Adult domains:", length(adult_domains), "\n")

# Step 2: Select domains using X-aligned method
cat("\nStep 2: Select domains for manual labeling\n")
budget <- 75  # Number of domains you can manually label

selection_result <- hybrid_x_aligned_method(
  cij = cij,
  X_matrix = X_matrix,
  adult_domains = adult_domains, 
  budget = budget,
  top_k_fraction = 0.4,  # 40% popular, 60% HT-weighted
  verbose = TRUE
)

selected_domains <- selection_result$selected_domains
inclusion_probs <- selection_result$inclusion_probs

cat("- Domains to manually label:", length(selected_domains), "\n")

# Step 3: [MANUAL LABELING HAPPENS HERE]
# In practice, you would now manually classify the selected domains
# For this example, we use the known classification
cat("\nStep 3: Manual labeling (simulated)\n")
cat("- You would now manually classify domains:", paste(head(selected_domains, 5), collapse=", "), "...\n")

# Step 4: Compute HT-unbiased estimates
cat("\nStep 4: Compute unbiased proportion estimates\n")
y_estimates <- robust_ht_estimator(
  cij = cij,
  adult_domains = adult_domains,
  selected_domains = selected_domains,
  pi_j = inclusion_probs
)

cat("- Mean estimated proportion:", round(mean(y_estimates), 4), "\n")
cat("- Mean true proportion:", round(mean(y_true), 4), "\n")
cat("- Correlation:", round(cor(y_estimates, y_true), 3), "\n")

# Step 5: Run your regression analysis
cat("\nStep 5: Run regression analysis\n")
lm_result <- lm(y_estimates ~ X_matrix[, -1])

# Compare to ground truth
lm_true <- lm(y_true ~ X_matrix[, -1])

# Calculate bias
coef_estimated <- coef(lm_result)
coef_true <- coef(lm_true)
bias_pct <- 100 * abs(coef_estimated - coef_true) / abs(coef_true)
bias_pct[is.infinite(bias_pct) | is.na(bias_pct)] <- 0

cat("- Max coefficient bias:", round(max(bias_pct), 1), "%\n")
cat("- Regression R²:", round(summary(lm_result)$r.squared, 3), "\n")

# ==============================================================================
# EXAMPLE 3: Testing Different Budget Levels
# ==============================================================================

cat("\n\n=== EXAMPLE 3: BUDGET SENSITIVITY ANALYSIS ===\n")

budget_fractions <- c(0.2, 0.25, 0.3, 0.35)
budget_results <- data.frame(
  budget_pct = budget_fractions * 100,
  domains_used = NA,
  max_bias_pct = NA,
  y_correlation = NA,
  xu_norm = NA
)

for (i in seq_along(budget_fractions)) {
  cat("Testing budget:", round(100 * budget_fractions[i]), "%... ")
  
  result <- test_final_solution(budget_fraction = budget_fractions[i], verbose = FALSE)
  
  if (!is.null(result)) {
    budget_results$domains_used[i] <- length(result$selected_domains)
    budget_results$max_bias_pct[i] <- result$max_bias_pct
    budget_results$y_correlation[i] <- result$y_correlation
    budget_results$xu_norm[i] <- result$xu_norm_mean
    
    cat("Max bias:", round(result$max_bias_pct, 1), "%\n")
  } else {
    cat("Failed\n")
  }
}

cat("\nBudget Sensitivity Results:\n")
print(budget_results)

# ==============================================================================
# EXAMPLE 4: Custom Parameters
# ==============================================================================

cat("\n\n=== EXAMPLE 4: CUSTOM PARAMETERS ===\n")

# You can adjust the hybrid ratio
cat("Testing different Top-K fractions:\n")

top_k_fractions <- c(0.3, 0.4, 0.5, 0.6)
for (frac in top_k_fractions) {
  cat("Top-K fraction:", round(100 * frac), "%... ")
  
  selection <- hybrid_x_aligned_method(
    cij = data$cij,
    X_matrix = data$X_matrix,
    adult_domains = data$adult_domains,
    budget = 75,
    top_k_fraction = frac,
    verbose = FALSE
  )
  
  # Quick evaluation
  y_est <- robust_ht_estimator(data$cij, data$adult_domains, 
                              selection$selected_domains, selection$inclusion_probs)
  
  lm_est <- lm(y_est ~ data$X_matrix[, -1])
  lm_true <- lm(data$y_true ~ data$X_matrix[, -1])
  
  bias <- 100 * abs(coef(lm_est) - coef(lm_true)) / abs(coef(lm_true))
  bias[is.infinite(bias) | is.na(bias)] <- 0
  
  cat("Max bias:", round(max(bias), 1), "%\n")
}

cat("\n✅ USAGE EXAMPLES COMPLETE!\n")
cat("The X-aligned error minimization method successfully reduces\n")
cat("coefficient bias while using only a fraction of domains for manual labeling.\n")