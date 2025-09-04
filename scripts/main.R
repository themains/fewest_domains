# Main Entry Point for Domain Selection Optimization
# Cheap Precision: Finding the Fewest Domains to Code

source("scripts/data_generation.R")
source("scripts/corrected_methods.R")
source("scripts/validation.R")

# Quick demo of the domain selection problem
run_demo <- function(n_respondents = 15, n_domains = 150, target_se = 0.1, seed = 42) {
  cat("", rep("=", 70), "\n")
  cat("DOMAIN SELECTION OPTIMIZATION DEMO\n")
  cat(rep("=", 70), "\n")
  cat("Problem: Find minimum domains to manually code\n")
  cat("Goal: Standard error ≤", target_se, "for each respondent\n\n")
  
  # Generate realistic browsing data
  cat("Generating realistic browsing data...\n")
  data <- generate_realistic_browsing_data(n_respondents, n_domains, seed)
  cij <- data$cij
  
  cat("Data characteristics:\n")
  cat("- Top 10 domains:", data$characteristics$top_10_pct, "% of visits\n")
  cat("- Average domains per person:", data$characteristics$avg_domains_per_person, "\n")
  cat("- User overlap in top domains:", data$characteristics$avg_overlap_top20, "%\n\n")
  
  # Run both methods
  cat("", rep("-", 50), "\n")
  cat("NAIVE METHOD (per-respondent sampling)\n")
  cat(rep("-", 50), "\n")
  naive_result <- naive_corrected(cij, target_se, verbose = TRUE)
  
  cat("\n", rep("-", 50), "\n")
  cat("OPTIMAL METHOD (shared domain selection)\n")
  cat(rep("-", 50), "\n")
  optimal_result <- optimal_corrected(cij, target_se, verbose = TRUE)
  
  # Summary
  domains_saved <- naive_result$total_unique_domains - optimal_result$total_unique_domains
  improvement_pct <- round(100 * domains_saved / naive_result$total_unique_domains, 1)
  
  cat("\n", rep("=", 70), "\n")
  cat("RESULTS SUMMARY\n")
  cat(rep("=", 70), "\n")
  cat("Naive method:   ", naive_result$total_unique_domains, " domains\n")
  cat("Optimal method: ", optimal_result$total_unique_domains, " domains\n")
  cat("Improvement:    ", domains_saved, " domains saved (", improvement_pct, "% reduction)\n")
  cat("Both methods:   ", ifelse(naive_result$constraints_satisfied & optimal_result$constraints_satisfied, 
                                 "✓ Meet all constraints", "✗ Some constraints failed"), "\n")
  
  cat("\nCOST SAVINGS:\n")
  cat("- Manual coding reduced by", improvement_pct, "%\n")
  cat("- Domain classification workload:", improvement_pct, "% smaller\n")
  cat("- Statistical precision maintained for all respondents\n")
  
  return(list(
    data = data,
    naive = naive_result,
    optimal = optimal_result,
    improvement = improvement_pct
  ))
}

# Run demo if script executed directly
if (!interactive()) {
  cat("🎯 DOMAIN SELECTION OPTIMIZATION\n")
  cat("Finding the fewest domains to manually classify while maintaining statistical precision\n\n")
  
  result <- run_demo()
  
  cat("\n💡 TIP: Run 'source(\"scripts/robust_evaluation.R\")' for comprehensive testing\n")
  cat("📊 TIP: Modify parameters in run_demo() for different scenarios\n")
}