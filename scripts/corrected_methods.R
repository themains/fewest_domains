# Corrected Domain Selection Methods
# Properly implementing the constraint: SE = sqrt(0.25/effective_sample) <= target_se

source("scripts/data_generation.R")
source("scripts/validation.R")

# Corrected Naive Method
naive_corrected <- function(cij, target_se, verbose = TRUE) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  # Required effective sample size: effective_sample >= 0.25/target_se^2
  required_eff_sample <- 0.25 / (target_se^2)
  
  if (verbose) {
    cat("=== CORRECTED NAIVE METHOD ===\n")
    cat("Target standard error:", target_se, "\n")
    cat("Required effective sample per respondent:", round(required_eff_sample, 2), "\n")
  }
  
  all_selected <- c()
  respondent_domains <- list()
  
  for (i in 1:n) {
    # For this respondent, we need to select enough domains
    # such that effective_sample >= required_eff_sample
    
    visit_props <- cij[i, ] / sum(cij[i, ])
    available_domains <- which(cij[i, ] > 0)
    
    if (length(available_domains) == 0) {
      # Edge case: no visits
      respondent_domains[[i]] <- integer(0)
      next
    }
    
    # Greedy selection: keep adding domains until effective sample is sufficient
    selected <- c()
    current_eff_sample <- 0
    
    # Sort domains by visit frequency (descending)
    sorted_domains <- available_domains[order(visit_props[available_domains], decreasing = TRUE)]
    
    for (domain in sorted_domains) {
      selected <- c(selected, domain)
      # Effective sample = sum of visit proportions * number of domains
      coverage <- sum(visit_props[selected])
      current_eff_sample <- coverage * length(selected)
      
      if (current_eff_sample >= required_eff_sample) break
    }
    
    # If we still don't have enough, add random domains
    while (current_eff_sample < required_eff_sample && length(selected) < m) {
      remaining <- setdiff(1:m, selected)
      if (length(remaining) == 0) break
      
      additional <- sample(remaining, 1)
      selected <- c(selected, additional)
      coverage <- sum(visit_props[selected])
      current_eff_sample <- coverage * length(selected)
    }
    
    respondent_domains[[i]] <- selected
    all_selected <- c(all_selected, selected)
  }
  
  unique_domains <- unique(all_selected)
  
  if (verbose) {
    cat("Total unique domains selected:", length(unique_domains), "\n")
  }
  
  # Validate the solution
  validation <- validate_domain_solution(cij, unique_domains, target_se, 
                                        "Corrected Naive", verbose = verbose)
  
  return(list(
    method = "Corrected Naive",
    total_unique_domains = length(unique_domains),
    unique_domains = unique_domains,
    respondent_domains = respondent_domains,
    target_se = target_se,
    validation = validation,
    constraints_satisfied = validation$summary$all_constraints_met
  ))
}

# Corrected Optimal Method
optimal_corrected <- function(cij, target_se, verbose = TRUE) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  required_eff_sample <- 0.25 / (target_se^2)
  
  if (verbose) {
    cat("=== CORRECTED OPTIMAL METHOD ===\n")
    cat("Target standard error:", target_se, "\n")
    cat("Required effective sample per respondent:", round(required_eff_sample, 2), "\n")
  }
  
  visit_props <- t(apply(cij, 1, function(x) x / sum(x)))
  selected_domains <- c()
  
  # Keep adding domains until all respondents meet the constraint
  repeat {
    # Check current effective samples
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
    if (length(selected_domains) >= m) {
      if (verbose) cat("WARNING: All domains selected, constraints may not be met\n")
      break
    }
    
    # Find the domain that helps the most unsatisfied respondents
    best_benefit <- 0
    best_domain <- NA
    
    for (j in 1:m) {
      if (j %in% selected_domains) next
      
      benefit <- 0
      for (i in unsatisfied) {
        # How much would adding domain j help respondent i?
        current_coverage <- if (length(selected_domains) == 0) 0 else sum(visit_props[i, selected_domains])
        current_eff <- current_coverage * length(selected_domains)
        
        new_coverage <- current_coverage + visit_props[i, j]
        new_eff <- new_coverage * (length(selected_domains) + 1)
        
        improvement <- new_eff - current_eff
        # Weight by how much this respondent needs help
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
    
    if (verbose && length(selected_domains) %% 5 == 0) {
      cat("Selected", length(selected_domains), "domains,", 
          length(unsatisfied), "respondents still need help\n")
    }
  }
  
  if (verbose) {
    cat("Total domains selected:", length(selected_domains), "\n")
  }
  
  # Final validation
  validation <- validate_domain_solution(cij, selected_domains, target_se,
                                        "Corrected Optimal", verbose = verbose)
  
  return(list(
    method = "Corrected Optimal",
    total_unique_domains = length(selected_domains),
    unique_domains = selected_domains,
    target_se = target_se,
    validation = validation,
    constraints_satisfied = validation$summary$all_constraints_met
  ))
}

# Test both corrected methods
if (!interactive()) {
  cat("=== TESTING CORRECTED METHODS ===\n")
  
  # Generate test data
  data <- generate_realistic_browsing_data(15, 200, seed = 123)
  cij <- data$cij
  target_se <- 0.1
  
  # Test naive
  cat("\n" , rep("=", 50), "\n")
  naive_result <- naive_corrected(cij, target_se)
  
  # Test optimal
  cat("\n" , rep("=", 50), "\n")
  optimal_result <- optimal_corrected(cij, target_se)
  
  # Compare
  cat("\n" , rep("=", 50), "\n")
  cat("COMPARISON:\n")
  cat("Naive domains:", naive_result$total_unique_domains, "\n")
  cat("Optimal domains:", optimal_result$total_unique_domains, "\n")
  cat("Improvement:", naive_result$total_unique_domains - optimal_result$total_unique_domains, "domains saved\n")
  cat("Naive constraints satisfied:", naive_result$constraints_satisfied, "\n")
  cat("Optimal constraints satisfied:", optimal_result$constraints_satisfied, "\n")
}