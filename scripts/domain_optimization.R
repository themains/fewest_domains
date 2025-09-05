# Domain Selection Optimization Methods
# Core insight: Popular domains hit diminishing returns, real optimization is in tail allocation

source("scripts/data_generation.R")
source("scripts/validation.R")

# Naive Method: Per-respondent weighted sampling
naive_method <- function(cij, target_se, verbose = TRUE) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  required_eff_sample <- 0.25 / (target_se^2)
  
  if (verbose) {
    cat("=== NAIVE METHOD ===\n")
    cat("Per-respondent domain selection\n")
    cat("Target SE:", target_se, "| Required effective sample:", round(required_eff_sample, 1), "\n")
  }
  
  all_selected <- c()
  
  for (i in 1:n) {
    visit_props <- cij[i, ] / sum(cij[i, ])
    available_domains <- which(cij[i, ] > 0)
    
    if (length(available_domains) == 0) next
    
    # Greedy selection by visit frequency
    selected <- c()
    current_eff_sample <- 0
    
    sorted_domains <- available_domains[order(visit_props[available_domains], decreasing = TRUE)]
    
    for (domain in sorted_domains) {
      selected <- c(selected, domain)
      coverage <- sum(visit_props[selected])
      current_eff_sample <- coverage * length(selected)
      
      if (current_eff_sample >= required_eff_sample) break
    }
    
    all_selected <- c(all_selected, selected)
  }
  
  unique_domains <- unique(all_selected)
  
  if (verbose) {
    cat("Total unique domains:", length(unique_domains), "\n")
  }
  
  # Validate
  validation <- validate_domain_solution(cij, unique_domains, target_se, "Naive", verbose = verbose)
  
  return(list(
    method = "Naive",
    total_domains = length(unique_domains),
    selected_domains = unique_domains,
    target_se = target_se,
    validation = validation,
    constraints_satisfied = validation$summary$all_constraints_met
  ))
}

# Optimal Method: Shared domain selection with greedy algorithm
optimal_method <- function(cij, target_se, verbose = TRUE) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  required_eff_sample <- 0.25 / (target_se^2)
  
  if (verbose) {
    cat("=== OPTIMAL METHOD ===\n") 
    cat("Shared domain selection across all respondents\n")
    cat("Target SE:", target_se, "| Required effective sample:", round(required_eff_sample, 1), "\n")
  }
  
  visit_props <- t(apply(cij, 1, function(x) x / sum(x)))
  selected_domains <- c()
  
  # Greedy: keep adding domains until all respondents satisfied
  repeat {
    all_satisfied <- TRUE
    unsatisfied <- c()
    
    for (i in 1:n) {
      if (length(selected_domains) == 0) {
        eff_sample <- 0
      } else {
        coverage <- sum(visit_props[i, selected_domains])
        eff_sample <- coverage * length(selected_domains)
      }
      
      if (eff_sample < required_eff_sample) {
        all_satisfied <- FALSE
        unsatisfied <- c(unsatisfied, i)
      }
    }
    
    if (all_satisfied) break
    if (length(selected_domains) >= m) break
    
    # Find domain that helps the most unsatisfied respondents
    best_benefit <- 0
    best_domain <- NA
    
    for (j in 1:m) {
      if (j %in% selected_domains) next
      
      benefit <- 0
      for (i in unsatisfied) {
        current_coverage <- if (length(selected_domains) == 0) 0 else sum(visit_props[i, selected_domains])
        current_eff <- current_coverage * length(selected_domains)
        
        new_coverage <- current_coverage + visit_props[i, j]
        new_eff <- new_coverage * (length(selected_domains) + 1)
        
        improvement <- new_eff - current_eff
        deficit <- required_eff_sample - current_eff
        benefit <- benefit + improvement * (1 + deficit/required_eff_sample)
      }
      
      if (benefit > best_benefit) {
        best_benefit <- benefit
        best_domain <- j
      }
    }
    
    if (is.na(best_domain)) break
    
    selected_domains <- c(selected_domains, best_domain)
    
    if (verbose && length(selected_domains) %% 10 == 0) {
      cat("Selected", length(selected_domains), "domains,", length(unsatisfied), "respondents remaining\n")
    }
  }
  
  if (verbose) {
    cat("Total domains:", length(selected_domains), "\n")
  }
  
  # Validate
  validation <- validate_domain_solution(cij, selected_domains, target_se, "Optimal", verbose = verbose)
  
  return(list(
    method = "Optimal",
    total_domains = length(selected_domains),
    selected_domains = selected_domains,
    target_se = target_se,
    validation = validation,
    constraints_satisfied = validation$summary$all_constraints_met
  ))
}

# Core-Tail Method: Explicit decomposition showing optimization structure
core_tail_method <- function(cij, target_se, core_threshold = 0.8, verbose = TRUE) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  required_eff_sample <- 0.25 / (target_se^2)
  
  if (verbose) {
    cat("=== CORE-TAIL METHOD ===\n")
    cat("Explicit core (p≈1) + optimized tail allocation\n")
    cat("Target SE:", target_se, "| Core threshold:", core_threshold*100, "%\n")
  }
  
  # Step 1: Identify core domains (diminishing returns threshold)
  domain_popularity <- colSums(cij)
  domain_order <- order(domain_popularity, decreasing = TRUE)
  
  total_visits <- sum(cij)
  cumulative_visits <- cumsum(domain_popularity[domain_order])
  core_coverage_point <- which(cumulative_visits >= core_threshold * total_visits)[1]
  
  core_domains <- domain_order[1:core_coverage_point]
  tail_domains <- setdiff(1:m, core_domains)
  
  if (verbose) {
    core_pct <- round(100 * sum(domain_popularity[core_domains]) / total_visits, 1)
    cat("Core domains:", length(core_domains), "covering", core_pct, "% of visits\n")
    cat("Tail domains:", length(tail_domains), "\n")
  }
  
  # Step 2: Always select core domains (p=1)
  selected_domains <- core_domains
  
  # Step 3: Calculate remaining needs after core
  respondent_needs <- numeric(n)
  for (i in 1:n) {
    visit_props <- cij[i, ] / sum(cij[i, ])
    core_coverage <- sum(visit_props[core_domains])
    core_eff_sample <- core_coverage * length(core_domains)
    respondent_needs[i] <- max(0, required_eff_sample - core_eff_sample)
  }
  
  # Step 4: Optimal tail allocation (this is where real optimization happens)
  if (verbose) {
    still_need <- sum(respondent_needs > 0.1)
    cat("After core selection:", still_need, "respondents still need tail domains\n")
  }
  
  # Greedy tail selection based on marginal utility
  while (sum(respondent_needs > 0.1) > 0) {
    best_value <- 0
    best_tail_domain <- NA
    
    for (j in tail_domains) {
      if (j %in% selected_domains) next
      
      value <- 0
      for (i in 1:n) {
        if (respondent_needs[i] > 0.1 && cij[i, j] > 0) {
          visit_props <- cij[i, ] / sum(cij[i, ])
          marginal_contribution <- visit_props[j] * 1
          urgency_weight <- respondent_needs[i] / required_eff_sample
          value <- value + marginal_contribution * urgency_weight
        }
      }
      
      if (value > best_value) {
        best_value <- value
        best_tail_domain <- j
      }
    }
    
    if (is.na(best_tail_domain)) break
    
    selected_domains <- c(selected_domains, best_tail_domain)
    
    # Recalculate needs
    for (i in 1:n) {
      visit_props <- cij[i, ] / sum(cij[i, ])
      current_coverage <- sum(visit_props[selected_domains])
      current_eff_sample <- current_coverage * length(selected_domains)
      respondent_needs[i] <- max(0, required_eff_sample - current_eff_sample)
    }
  }
  
  n_tail_selected <- length(selected_domains) - length(core_domains)
  
  if (verbose) {
    cat("Final selection: Core =", length(core_domains), "+ Tail =", n_tail_selected, 
        "= Total =", length(selected_domains), "\n")
  }
  
  # Validate
  validation <- validate_domain_solution(cij, selected_domains, target_se, "Core-Tail", verbose = verbose)
  
  return(list(
    method = "Core-Tail",
    total_domains = length(selected_domains),
    selected_domains = selected_domains,
    core_domains = core_domains,
    tail_domains = setdiff(selected_domains, core_domains),
    core_threshold = core_threshold,
    target_se = target_se,
    validation = validation,
    constraints_satisfied = validation$summary$all_constraints_met
  ))
}

# Compare all three methods
compare_methods <- function(cij, target_se, verbose = TRUE) {
  if (verbose) {
    cat("\n", rep("=", 70), "\n")
    cat("DOMAIN OPTIMIZATION METHOD COMPARISON\n")
    cat(rep("=", 70), "\n")
  }
  
  # Run all methods
  naive_result <- naive_method(cij, target_se, verbose = verbose)
  cat("\n")
  optimal_result <- optimal_method(cij, target_se, verbose = verbose)  
  cat("\n")
  core_tail_result <- core_tail_method(cij, target_se, verbose = verbose)
  
  # Summary comparison
  if (verbose) {
    cat("\n", rep("-", 50), "\n")
    cat("SUMMARY:\n")
    cat(rep("-", 50), "\n")
    
    cat(sprintf("%-15s %8s %12s\n", "Method", "Domains", "Satisfied"))
    cat(rep("-", 40), "\n")
    
    methods_list <- list(naive_result, optimal_result, core_tail_result)
    
    for (result in methods_list) {
      sat_str <- ifelse(result$constraints_satisfied, "YES", "NO")
      cat(sprintf("%-15s %8d %12s\n", result$method, result$total_domains, sat_str))
    }
    
    # Efficiency gains
    naive_domains <- naive_result$total_domains
    optimal_domains <- optimal_result$total_domains
    improvement_pct <- round(100 * (naive_domains - optimal_domains) / naive_domains, 1)
    
    cat("\nEFFICIENCY GAIN:\n")
    cat("- Naive method:", naive_domains, "domains\n")
    cat("- Optimal methods:", optimal_domains, "domains\n")
    cat("- Improvement:", improvement_pct, "% fewer domains\n")
    
    # Core-tail insights
    if (!is.null(core_tail_result$core_domains)) {
      cat("\nCORE-TAIL DECOMPOSITION:\n")
      cat("- Core domains (p≈1):", length(core_tail_result$core_domains), "\n")
      cat("- Tail domains (optimized):", length(core_tail_result$tail_domains), "\n")
      cat("- Real optimization happens in tail allocation\n")
    }
  }
  
  return(list(
    naive = naive_result,
    optimal = optimal_result,
    core_tail = core_tail_result
  ))
}

# Test the methods
if (!interactive()) {
  cat("🎯 DOMAIN OPTIMIZATION METHODS\n")
  cat("Testing naive, optimal, and core-tail approaches\n\n")
  
  # Generate test data
  data <- generate_realistic_browsing_data(15, 200, seed = 123)
  cij <- data$cij
  target_se <- 0.1
  
  # Compare methods
  results <- compare_methods(cij, target_se)
  
  cat("\n✅ TESTING COMPLETE!\n")
  cat("Core insight: Popular domains hit diminishing returns quickly.\n")
  cat("Real optimization is in the tail allocation.\n")
}