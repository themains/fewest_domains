# Optimal Domain Selection Method
# Minimize total unique domains across all respondents while meeting SE constraints

source("scripts/data_generation.R")
source("scripts/validation.R")

optimal_domain_selection <- function(cij, target_se, verbose = TRUE) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  if (verbose) {
    cat("=== OPTIMAL METHOD ===\n")
    cat("Target standard error:", target_se, "\n")
    cat("Minimizing total domains across", n, "respondents\n")
  }
  
  # Convert visits to proportions (frequency weights)
  visit_props <- t(apply(cij, 1, function(x) {
    total <- sum(x)
    if (total == 0) rep(0, length(x)) else x / total
  }))
  
  # Greedy selection: iteratively pick the domain that helps the most
  selected_domains <- c()
  
  repeat {
    # Check if current selection satisfies all respondents
    all_satisfied <- TRUE
    unsatisfied_respondents <- c()
    
    for (i in 1:n) {
      if (length(selected_domains) == 0) {
        eff_sample <- 0
      } else {
        # Effective sample is the coverage-weighted number of domains
        coverage <- sum(visit_props[i, selected_domains])
        eff_sample <- coverage * length(selected_domains)
      }
      
      se <- if (eff_sample > 0) sqrt(0.25 / eff_sample) else Inf
      
      if (se > target_se) {
        all_satisfied <- FALSE
        unsatisfied_respondents <- c(unsatisfied_respondents, i)
      }
    }
    
    if (all_satisfied) break
    if (length(selected_domains) >= m) {
      if (verbose) cat("Warning: All domains selected, some respondents still unsatisfied\n")
      break
    }
    
    # Find domain that provides maximum benefit to unsatisfied respondents
    best_score <- 0
    best_domain <- NA
    
    for (j in 1:m) {
      if (j %in% selected_domains) next
      
      # Calculate how much this domain helps unsatisfied respondents
      score <- 0
      
      for (i in unsatisfied_respondents) {
        # Current effective sample for respondent i
        if (length(selected_domains) == 0) {
          current_coverage <- 0
          current_eff <- 0
        } else {
          current_coverage <- sum(visit_props[i, selected_domains])
          current_eff <- current_coverage * length(selected_domains)
        }
        
        # New effective sample if we add domain j
        new_coverage <- current_coverage + visit_props[i, j]
        new_eff <- new_coverage * (length(selected_domains) + 1)
        
        # SE improvement
        current_se <- if (current_eff > 0) sqrt(0.25 / current_eff) else Inf
        new_se <- if (new_eff > 0) sqrt(0.25 / new_eff) else Inf
        
        se_improvement <- if (is.finite(current_se) && is.finite(new_se)) {
          max(0, current_se - new_se)
        } else if (is.finite(new_se)) {
          1.0  # Big improvement from Inf to finite
        } else {
          0
        }
        
        # Weight by visit frequency to prioritize high-traffic domains
        visit_weight <- visit_props[i, j]
        
        # Score combines SE improvement with visit frequency
        score <- score + se_improvement * (1 + 10 * visit_weight)
      }
      
      if (score > best_score) {
        best_score <- score
        best_domain <- j
      }
    }
    
    if (is.na(best_domain) || best_score <= 1e-12) {
      if (verbose) cat("Warning: No beneficial domain found\n")
      break
    }
    
    selected_domains <- c(selected_domains, best_domain)
    
    if (verbose && length(selected_domains) %% 5 == 0) {
      cat("Selected", length(selected_domains), "domains,", 
          length(unsatisfied_respondents), "respondents still unsatisfied\n")
    }
  }
  
  if (verbose) {
    cat("Total domains selected:", length(selected_domains), "\n")
    
    # Analyze selected domains
    if (length(selected_domains) > 0) {
      domain_visits <- colSums(cij)[selected_domains]
      domain_users <- colSums(cij > 0)[selected_domains]
      
      cat("Selected domain characteristics:\n")
      cat("- Average visits per selected domain:", round(mean(domain_visits)), "\n")
      cat("- Average users per selected domain:", round(mean(domain_users)), "\n")
      cat("- Most popular selected domain has:", max(domain_visits), "visits\n")
      cat("- Least popular selected domain has:", min(domain_visits), "visits\n")
    }
  }
  
  # Final validation with rigorous constraint checking
  final_validation <- validate_domain_solution(cij, selected_domains, target_se, 
                                               "Optimal Method", verbose = verbose)
  
  if (!final_validation$summary$all_constraints_met && verbose) {
    cat("WARNING: Solution does not meet all constraints!\n")
    cat("This indicates a bug in the optimization algorithm.\n")
  }
  
  return(list(
    method = "Optimal (shared domain selection)",
    total_unique_domains = length(selected_domains),
    unique_domains = selected_domains,
    target_se = target_se,
    unsatisfied_count = length(unsatisfied_respondents),
    validation = final_validation,
    constraints_satisfied = final_validation$summary$all_constraints_met
  ))
}

# Validation function for optimal method
validate_optimal_solution <- function(cij, solution, verbose = TRUE) {
  n <- nrow(cij)
  selected_domains <- solution$unique_domains
  
  visit_props <- t(apply(cij, 1, function(x) {
    total <- sum(x)
    if (total == 0) rep(0, length(x)) else x / total
  }))
  
  results <- data.frame(
    respondent = 1:n,
    coverage_pct = numeric(n),
    effective_sample = numeric(n),
    standard_error = numeric(n),
    meets_target = logical(n)
  )
  
  for (i in 1:n) {
    if (length(selected_domains) == 0) {
      coverage <- 0
      eff_sample <- 0
    } else {
      # Coverage is sum of visit proportions to selected domains
      coverage <- sum(visit_props[i, selected_domains])
      # Effective sample size
      eff_sample <- coverage * length(selected_domains)
    }
    
    # Standard error (worst case p=0.5)
    se <- if (eff_sample > 0) sqrt(0.25 / eff_sample) else Inf
    
    results$coverage_pct[i] <- 100 * coverage
    results$effective_sample[i] <- eff_sample
    results$standard_error[i] <- se
    results$meets_target[i] <- se <= solution$target_se
  }
  
  if (verbose) {
    cat("\n=== OPTIMAL METHOD VALIDATION ===\n")
    cat("Respondents meeting target SE:", sum(results$meets_target), "/", n, "\n")
    cat("Average coverage:", round(mean(results$coverage_pct), 1), "%\n")
    cat("Average effective sample:", round(mean(results$effective_sample), 1), "\n")
    cat("Average standard error:", round(mean(results$standard_error), 4), "\n")
    cat("Max standard error:", round(max(results$standard_error[is.finite(results$standard_error)]), 4), "\n")
    
    if (any(!results$meets_target)) {
      failed <- which(!results$meets_target)
      cat("Failed respondents:", paste(failed, collapse = ", "), "\n")
    }
  }
  
  return(results)
}

# Test the method
if (!interactive()) {
  cat("=== TESTING OPTIMAL METHOD ===\n")
  
  # Generate test data
  data <- generate_realistic_browsing_data(15, 200)
  cij <- data$cij
  
  # Test with different SE targets
  target_se <- 0.1
  
  # Run optimal method
  solution <- optimal_domain_selection(cij, target_se)
  
  # Validate
  validation <- validate_optimal_solution(cij, solution)
  
  cat("\nSample of validation results:\n")
  print(head(validation))
}