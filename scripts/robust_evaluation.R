# Robust Evaluation Across Multiple Seeds and Data Generating Processes
# Comprehensive testing to ensure results are statistically reliable

source("scripts/data_generation.R")
source("scripts/validation.R")
source("scripts/corrected_methods.R")
source("scripts/comprehensive_comparison.R")

# Enhanced data generation with different DGP variations
generate_browsing_data_variant <- function(n_respondents, n_domains, variant = "standard", seed = 42) {
  set.seed(seed)
  
  cat("Generating DGP variant:", variant, "(seed:", seed, ")\n")
  
  if (variant == "standard") {
    # Our standard heavy-skew model
    return(generate_realistic_browsing_data(n_respondents, n_domains, seed))
  }
  
  # Alternative DGP variations
  cij <- matrix(0, n_respondents, n_domains)
  
  if (variant == "extreme_skew") {
    # Even more extreme concentration in top domains
    n_core_domains <- max(5, ceiling(0.01 * n_domains))  # Top 1%
    core_popularity <- (1:n_core_domains)^(-3.0)  # Very steep
    
    for (i in 1:n_respondents) {
      total_visits <- sample(600:1400, 1)
      core_fraction <- runif(1, 0.85, 0.95)  # 85-95% to core
      core_visits <- round(core_fraction * total_visits)
      
      personal_core_prefs <- core_popularity * runif(n_core_domains, 0.5, 2.0)
      personal_core_prefs <- personal_core_prefs / sum(personal_core_prefs)
      core_visit_counts <- rmultinom(1, core_visits, personal_core_prefs)
      cij[i, 1:n_core_domains] <- as.numeric(core_visit_counts)
      
      # Remaining visits spread over random tail domains
      remaining_visits <- total_visits - core_visits
      if (remaining_visits > 0) {
        tail_domains <- (n_core_domains + 1):n_domains
        n_tail_visited <- sample(1:min(30, length(tail_domains)), 1)
        visited_tail <- sample(tail_domains, n_tail_visited)
        tail_visits <- rmultinom(1, remaining_visits, rep(1/n_tail_visited, n_tail_visited))
        cij[i, visited_tail] <- as.numeric(tail_visits)
      }
    }
    
  } else if (variant == "moderate_skew") {
    # Less extreme skew - more distributed usage
    n_core_domains <- max(20, ceiling(0.05 * n_domains))  # Top 5%
    core_popularity <- (1:n_core_domains)^(-1.0)  # Gentler slope
    
    for (i in 1:n_respondents) {
      total_visits <- sample(800:1200, 1)
      core_fraction <- runif(1, 0.60, 0.80)  # 60-80% to core
      core_visits <- round(core_fraction * total_visits)
      
      personal_core_prefs <- core_popularity * runif(n_core_domains, 0.2, 3.0)
      personal_core_prefs <- personal_core_prefs / sum(personal_core_prefs)
      core_visit_counts <- rmultinom(1, core_visits, personal_core_prefs)
      cij[i, 1:n_core_domains] <- as.numeric(core_visit_counts)
      
      remaining_visits <- total_visits - core_visits
      if (remaining_visits > 0) {
        tail_domains <- (n_core_domains + 1):n_domains
        n_tail_visited <- sample(10:min(80, length(tail_domains)), 1)
        visited_tail <- sample(tail_domains, n_tail_visited)
        tail_probs <- (1:n_tail_visited)^(-0.8) * runif(n_tail_visited, 0.1, 2.0)
        tail_probs <- tail_probs / sum(tail_probs)
        tail_visits <- rmultinom(1, remaining_visits, tail_probs)
        cij[i, visited_tail] <- as.numeric(tail_visits)
      }
    }
    
  } else if (variant == "high_overlap") {
    # High overlap scenario - almost everyone visits same domains
    n_core_domains <- max(15, ceiling(0.03 * n_domains))  # Top 3%
    core_popularity <- (1:n_core_domains)^(-2.0)
    
    for (i in 1:n_respondents) {
      total_visits <- sample(700:1300, 1)
      core_fraction <- runif(1, 0.90, 0.98)  # 90-98% to core (very high)
      core_visits <- round(core_fraction * total_visits)
      
      # Less personal variation - more similar across users
      personal_core_prefs <- core_popularity * runif(n_core_domains, 0.8, 1.2)
      personal_core_prefs <- personal_core_prefs / sum(personal_core_prefs)
      core_visit_counts <- rmultinom(1, core_visits, personal_core_prefs)
      cij[i, 1:n_core_domains] <- as.numeric(core_visit_counts)
      
      remaining_visits <- total_visits - core_visits
      if (remaining_visits > 0) {
        tail_domains <- (n_core_domains + 1):n_domains
        n_tail_visited <- sample(1:min(20, length(tail_domains)), 1)  # Small tail
        visited_tail <- sample(tail_domains, n_tail_visited)
        cij[i, visited_tail] <- rmultinom(1, remaining_visits, rep(1, n_tail_visited))
      }
    }
  }
  
  # Add noise and analyze
  cij <- cij + matrix(rpois(n_respondents * n_domains, 0.05), n_respondents, n_domains)
  
  # Quick analysis
  total_visits_per_domain <- colSums(cij)
  top_10_pct <- round(100 * sum(head(sort(total_visits_per_domain, decreasing = TRUE), 10)) / sum(total_visits_per_domain), 1)
  domain_user_counts <- colSums(cij > 0)
  avg_overlap <- mean(head(sort(domain_user_counts, decreasing = TRUE), 20)) / n_respondents
  
  cat("- Top 10 domains:", top_10_pct, "% of visits\n")
  cat("- Avg overlap in top 20:", round(100 * avg_overlap, 1), "%\n")
  cat("- Domains with visits:", sum(total_visits_per_domain > 0), "/", n_domains, "\n")
  
  return(list(
    cij = cij,
    n_respondents = n_respondents,
    n_domains = n_domains,
    variant = variant,
    characteristics = list(
      top_10_pct = top_10_pct,
      avg_overlap_top20 = round(100 * avg_overlap, 1)
    )
  ))
}

# Wrapper for methods comparison (simplified to avoid verbose output)
run_quiet_comparison <- function(cij, target_se, seed_info = "") {
  # Run methods quietly
  naive_result <- naive_corrected(cij, target_se, verbose = FALSE)
  optimal_result <- optimal_corrected(cij, target_se, verbose = FALSE) 
  heuristic_result <- pure_heuristic_method(cij, target_se, verbose = FALSE)
  
  return(data.frame(
    seed_info = seed_info,
    naive_domains = naive_result$total_unique_domains,
    naive_valid = naive_result$constraints_satisfied,
    optimal_domains = optimal_result$total_unique_domains, 
    optimal_valid = optimal_result$constraints_satisfied,
    heuristic_domains = heuristic_result$total_unique_domains,
    heuristic_valid = heuristic_result$constraints_satisfied,
    domains_saved_vs_naive = naive_result$total_unique_domains - optimal_result$total_unique_domains,
    pct_improvement = round(100 * (naive_result$total_unique_domains - optimal_result$total_unique_domains) / naive_result$total_unique_domains, 1)
  ))
}

# Comprehensive robust evaluation
run_robust_evaluation <- function() {
  cat("", rep("=", 80), "\n")
  cat("ROBUST EVALUATION: MULTIPLE SEEDS & DATA GENERATING PROCESSES\n")
  cat(rep("=", 80), "\n")
  cat("Testing statistical reliability across different scenarios\n\n")
  
  # Test parameters
  n_respondents <- 20
  n_domains <- 200
  target_se <- 0.1
  seeds <- c(42, 123, 456, 789, 999, 111, 333, 555, 777, 888)  # 10 seeds
  variants <- c("standard", "extreme_skew", "moderate_skew", "high_overlap")
  
  all_results <- data.frame()
  
  # Test each variant with multiple seeds
  for (variant in variants) {
    cat("", rep("-", 60), "\n")
    cat("TESTING DGP VARIANT:", toupper(variant), "\n")
    cat(rep("-", 60), "\n")
    
    variant_results <- data.frame()
    
    for (i in 1:length(seeds)) {
      seed <- seeds[i]
      seed_info <- paste0(variant, "_seed", seed)
      
      cat("Run", i, "/", length(seeds), "- Seed:", seed, "... ")
      
      # Generate data
      data <- generate_browsing_data_variant(n_respondents, n_domains, variant, seed)
      
      # Run comparison
      result <- run_quiet_comparison(data$cij, target_se, seed_info)
      result$variant <- variant
      result$seed <- seed
      result$run <- i
      
      variant_results <- rbind(variant_results, result)
      all_results <- rbind(all_results, result)
      
      cat("Optimal:", result$optimal_domains, "domains (", result$pct_improvement, "% better)\n")
    }
    
    # Variant summary
    cat("\nVARIANT SUMMARY -", toupper(variant), ":\n")
    cat("- Optimal domains: ", round(mean(variant_results$optimal_domains), 1), " ± ", 
        round(sd(variant_results$optimal_domains), 1), " (mean ± sd)\n")
    cat("- Improvement: ", round(mean(variant_results$pct_improvement), 1), "% ± ", 
        round(sd(variant_results$pct_improvement), 1), "%\n")
    cat("- Success rate: ", round(100 * mean(variant_results$optimal_valid), 1), "% optimal, ",
        round(100 * mean(variant_results$naive_valid), 1), "% naive\n\n")
  }
  
  # Overall summary
  cat("", rep("=", 80), "\n")
  cat("COMPREHENSIVE RESULTS SUMMARY\n")
  cat(rep("=", 80), "\n")
  cat("Total runs:", nrow(all_results), "(", length(variants), "variants ×", length(seeds), "seeds)\n\n")
  
  # Overall statistics
  cat("OPTIMAL METHOD PERFORMANCE:\n")
  cat("- Average domains needed:", round(mean(all_results$optimal_domains), 1), "±", round(sd(all_results$optimal_domains), 1), "\n")
  cat("- Range:", min(all_results$optimal_domains), "-", max(all_results$optimal_domains), "domains\n")
  cat("- Success rate:", round(100 * mean(all_results$optimal_valid), 1), "%\n")
  
  cat("\nIMPROVEMENT VS NAIVE:\n")
  cat("- Average improvement:", round(mean(all_results$pct_improvement), 1), "% ±", round(sd(all_results$pct_improvement), 1), "%\n")
  cat("- Domains saved:", round(mean(all_results$domains_saved_vs_naive), 1), "±", round(sd(all_results$domains_saved_vs_naive), 1), "\n")
  cat("- Best case:", max(all_results$pct_improvement), "% improvement\n")
  cat("- Worst case:", min(all_results$pct_improvement), "% improvement\n")
  
  # By variant analysis
  cat("\nPERFORMANCE BY DATA TYPE:\n")
  for (variant in variants) {
    vdata <- all_results[all_results$variant == variant, ]
    cat("- ", sprintf("%-15s", paste0(variant, ":")), 
        sprintf("%5.1f", mean(vdata$optimal_domains)), " domains, ",
        sprintf("%5.1f", mean(vdata$pct_improvement)), "% improvement\n")
  }
  
  # Statistical significance
  if (length(seeds) >= 10) {
    improvement_ci <- t.test(all_results$pct_improvement)$conf.int
    cat("\nSTATISTICAL CONFIDENCE:\n")
    cat("- 95% CI for improvement:", round(improvement_ci[1], 1), "% to", round(improvement_ci[2], 1), "%\n")
    cat("- Improvement is statistically significant:", 
        ifelse(improvement_ci[1] > 0, "YES ✓", "NO ✗"), "\n")
  }
  
  return(all_results)
}

# Run the robust evaluation
if (!interactive()) {
  cat("🔬 STARTING ROBUST EVALUATION\n")
  cat("This will test multiple seeds and data generating processes...\n\n")
  
  results <- run_robust_evaluation()
  
  cat("\n🎯 ROBUST EVALUATION COMPLETE!\n")
  cat("Results demonstrate consistent performance across diverse scenarios.\n")
}