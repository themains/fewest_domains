# Comparison of Naive vs Optimal Domain Selection Methods
# Evaluates performance on realistic browsing data with heavy skew

source("scripts/data_generation.R")
source("scripts/naive_method.R") 
source("scripts/optimal_method.R")

run_comparison <- function(n_respondents = 25, n_domains = 300, target_se = 0.1, seed = 42) {
  
  cat("", rep("=", 70), "\n")
  cat("DOMAIN SELECTION METHOD COMPARISON\n")
  cat(rep("=", 70), "\n")
  cat("Problem: Find minimum domains to code across all respondents\n")
  cat("Constraint: Standard error <=", target_se, "for each respondent\n")
  cat("Data: Heavy-skew browsing with shared popular domains + long tails\n\n")
  
  # Generate realistic browsing data
  cat("Generating browsing data...\n")
  data <- generate_realistic_browsing_data(n_respondents, n_domains, seed)
  cij <- data$cij
  
  cat("\n", rep("-", 70), "\n")
  
  # Run naive method
  cat("Running naive method (per-respondent sampling)...\n")
  naive_result <- naive_domain_selection(cij, target_se, verbose = TRUE)
  naive_validation <- validate_naive_solution(cij, naive_result, verbose = TRUE)
  
  cat("\n", rep("-", 70), "\n")
  
  # Run optimal method  
  cat("Running optimal method (shared domain selection)...\n")
  optimal_result <- optimal_domain_selection(cij, target_se, verbose = TRUE)
  optimal_validation <- validate_optimal_solution(cij, optimal_result, verbose = TRUE)
  
  # Comparison analysis
  cat("\n", rep("=", 70), "\n")
  cat("COMPARISON RESULTS\n")
  cat(rep("=", 70), "\n")
  
  cat("NAIVE METHOD:\n")
  cat("- Total unique domains needed:", naive_result$total_unique_domains, "\n")
  cat("- Domains per respondent:", naive_result$domains_per_respondent, "\n") 
  cat("- Success rate:", sum(naive_validation$meets_target), "/", n_respondents, 
      "(", round(100 * mean(naive_validation$meets_target), 1), "%)\n")
  cat("- Average SE:", round(mean(naive_validation$standard_error), 4), "\n")
  
  cat("\nOPTIMAL METHOD:\n")
  cat("- Total unique domains needed:", optimal_result$total_unique_domains, "\n")
  cat("- Success rate:", sum(optimal_validation$meets_target), "/", n_respondents,
      "(", round(100 * mean(optimal_validation$meets_target), 1), "%)\n") 
  cat("- Average SE:", round(mean(optimal_validation$standard_error[is.finite(optimal_validation$standard_error)]), 4), "\n")
  
  # Calculate improvement
  domains_saved <- naive_result$total_unique_domains - optimal_result$total_unique_domains
  improvement_pct <- round(100 * domains_saved / naive_result$total_unique_domains, 1)
  
  cat("\nIMPROVEMENT:\n")
  if (domains_saved > 0) {
    cat("- Domains saved:", domains_saved, "\n")
    cat("- Percentage reduction:", improvement_pct, "%\n")
    cat("- Coding cost reduction: ", improvement_pct, "% fewer domains to manually code\n")
  } else {
    cat("- Optimal method requires", abs(domains_saved), "MORE domains\n") 
    cat("- This suggests the naive method benefits from randomization\n")
  }
  
  # Domain overlap analysis
  if (naive_result$total_unique_domains > 0 && optimal_result$total_unique_domains > 0) {
    overlap <- length(intersect(naive_result$unique_domains, optimal_result$unique_domains))
    naive_only <- length(setdiff(naive_result$unique_domains, optimal_result$unique_domains))
    optimal_only <- length(setdiff(optimal_result$unique_domains, naive_result$unique_domains))
    
    cat("\nDOMAIN SELECTION OVERLAP:\n")
    cat("- Domains selected by both methods:", overlap, "\n")
    cat("- Domains only in naive:", naive_only, "\n") 
    cat("- Domains only in optimal:", optimal_only, "\n")
    cat("- Overlap percentage:", round(100 * overlap / 
        min(naive_result$total_unique_domains, optimal_result$total_unique_domains), 1), "%\n")
  }
  
  # Data characteristics impact
  cat("\nDATA CHARACTERISTICS IMPACT:\n")
  cat("- Top 10 domains capture", data$characteristics$top_10_pct, "% of visits\n")
  cat("- Top 50 domains capture", data$characteristics$top_50_pct, "% of visits\n")
  cat("- Average domains per person:", data$characteristics$avg_domains_per_person, "\n")
  cat("- Top domain overlap:", data$characteristics$avg_overlap_top20, "% of users\n")
  
  # Performance analysis
  total_visits <- sum(cij)
  if (optimal_result$total_unique_domains > 0) {
    optimal_coverage <- sum(colSums(cij)[optimal_result$unique_domains])
    cat("- Optimal method covers", round(100 * optimal_coverage / total_visits, 1), 
        "% of all visits with", optimal_result$total_unique_domains, "domains\n")
  }
  
  # Return results for further analysis
  return(list(
    data = data,
    naive_result = naive_result,
    optimal_result = optimal_result,
    naive_validation = naive_validation,
    optimal_validation = optimal_validation,
    comparison = list(
      domains_saved = domains_saved,
      improvement_pct = improvement_pct,
      naive_success_rate = mean(naive_validation$meets_target),
      optimal_success_rate = mean(optimal_validation$meets_target)
    )
  ))
}

# Sensitivity analysis across different parameters
run_sensitivity_analysis <- function() {
  cat("\n", rep("=", 70), "\n")
  cat("SENSITIVITY ANALYSIS\n")
  cat(rep("=", 70), "\n")
  
  # Test different standard error targets
  se_targets <- c(0.05, 0.1, 0.15, 0.2)
  results_by_se <- list()
  
  for (target_se in se_targets) {
    cat("\n--- Testing SE target =", target_se, "---\n")
    result <- run_comparison(n_respondents = 20, n_domains = 250, 
                           target_se = target_se, seed = 42)
    results_by_se[[as.character(target_se)]] <- result$comparison
  }
  
  # Summarize sensitivity results
  cat("\nSENSITIVITY SUMMARY:\n")
  cat("SE_Target\tNaive_Domains\tOptimal_Domains\tImprovement_%\n")
  for (se in names(results_by_se)) {
    comp <- results_by_se[[se]]
    naive_domains <- comp$domains_saved + 
      (if (is.null(comp$optimal_domains)) 0 else comp$optimal_domains)
    cat(se, "\t\t", naive_domains, "\t\t", 
        naive_domains - comp$domains_saved, "\t\t", 
        comp$improvement_pct, "%\n")
  }
  
  return(results_by_se)
}

# Main execution
if (!interactive()) {
  # Run main comparison
  main_result <- run_comparison(n_respondents = 25, n_domains = 400, 
                               target_se = 0.1, seed = 42)
  
  # Run sensitivity analysis
  sensitivity_results <- run_sensitivity_analysis()
}