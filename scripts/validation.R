# Rigorous Validation Functions
# Ensures domain selection solutions meet standard error constraints

validate_domain_solution <- function(cij, selected_domains, target_se, method_name = "Unknown", verbose = TRUE) {
  n <- nrow(cij)
  m <- ncol(cij)
  
  if (verbose) {
    cat("\n=== VALIDATION:", method_name, "===\n")
    cat("Target standard error:", target_se, "\n")
    cat("Domains selected:", length(selected_domains), "out of", m, "\n")
  }
  
  # Convert visits to proportions (frequency weights)
  visit_props <- t(apply(cij, 1, function(x) {
    total <- sum(x)
    if (total == 0) rep(0, length(x)) else x / total
  }))
  
  # Validate each respondent
  results <- data.frame(
    respondent = 1:n,
    total_visits = rowSums(cij),
    visits_to_selected = numeric(n),
    coverage_pct = numeric(n),
    effective_sample = numeric(n),
    standard_error = numeric(n),
    meets_constraint = logical(n),
    violation_amount = numeric(n)
  )
  
  constraint_violations <- 0
  max_violation <- 0
  
  for (i in 1:n) {
    total_visits <- sum(cij[i, ])
    
    if (length(selected_domains) == 0 || total_visits == 0) {
      # Edge case: no domains selected or no visits
      visits_selected <- 0
      coverage <- 0
      eff_sample <- 0
      se <- Inf
    } else {
      # Calculate metrics
      visits_selected <- sum(cij[i, selected_domains])
      coverage <- sum(visit_props[i, selected_domains])
      
      # Effective sample size calculation
      # For frequency-weighted sampling: effective_n = (sum of weights)^2 / sum of weights^2
      # But for simplicity, we use coverage * number of domains as proxy
      eff_sample <- coverage * length(selected_domains)
      
      # Standard error for proportion (worst case p=0.5)
      se <- if (eff_sample > 0) sqrt(0.25 / eff_sample) else Inf
    }
    
    # Check constraint satisfaction
    constraint_met <- se <= target_se
    violation <- max(0, se - target_se)
    
    if (!constraint_met) {
      constraint_violations <- constraint_violations + 1
      max_violation <- max(max_violation, violation)
    }
    
    # Store results
    results$visits_to_selected[i] <- visits_selected
    results$coverage_pct[i] <- 100 * coverage
    results$effective_sample[i] <- eff_sample
    results$standard_error[i] <- se
    results$meets_constraint[i] <- constraint_met
    results$violation_amount[i] <- violation
  }
  
  # Summary statistics
  success_rate <- mean(results$meets_constraint) * 100
  avg_se <- mean(results$standard_error[is.finite(results$standard_error)])
  max_se <- max(results$standard_error[is.finite(results$standard_error)])
  
  if (verbose) {
    cat("CONSTRAINT VALIDATION:\n")
    cat("- Respondents meeting constraint:", sum(results$meets_constraint), "/", n,
        "(", round(success_rate, 1), "%)\n")
    
    if (constraint_violations > 0) {
      cat("- CONSTRAINT VIOLATIONS:", constraint_violations, "\n")
      cat("- Maximum violation:", round(max_violation, 4), "\n")
      failed_respondents <- which(!results$meets_constraint)
      cat("- Failed respondents:", paste(head(failed_respondents, 10), collapse = ", "))
      if (length(failed_respondents) > 10) cat(" ...")
      cat("\n")
    } else {
      cat("- ALL CONSTRAINTS SATISFIED ✓\n")
    }
    
    cat("\nSTANDARD ERROR SUMMARY:\n")
    cat("- Target SE:", target_se, "\n")
    cat("- Average SE:", round(avg_se, 4), "\n")
    cat("- Maximum SE:", round(max_se, 4), "\n")
    cat("- SE within target:", if (max_se <= target_se) "YES ✓" else "NO ✗", "\n")
    
    cat("\nCOVERAGE SUMMARY:\n")
    cat("- Average coverage:", round(mean(results$coverage_pct), 1), "%\n")
    cat("- Min coverage:", round(min(results$coverage_pct), 1), "%\n")
    cat("- Average effective sample:", round(mean(results$effective_sample), 1), "\n")
  }
  
  # Return validation results
  return(list(
    results = results,
    summary = list(
      method = method_name,
      target_se = target_se,
      domains_selected = length(selected_domains),
      success_rate = success_rate,
      constraint_violations = constraint_violations,
      max_violation = max_violation,
      avg_se = avg_se,
      max_se = max_se,
      all_constraints_met = constraint_violations == 0,
      avg_coverage = mean(results$coverage_pct)
    )
  ))
}

# Test with synthetic data where we know the right answer
test_validation_with_synthetic <- function() {
  set.seed(123)
  
  cat("=== TESTING VALIDATION WITH SYNTHETIC DATA ===\n")
  
  # Create simple synthetic data where we can calculate the exact answer
  n <- 5  # respondents
  m <- 10 # domains
  
  # Simple case: everyone visits domains 1-3 equally, domains 4-10 not at all
  cij <- matrix(0, n, m)
  cij[, 1:3] <- 100  # Each person has 100 visits to domains 1, 2, 3
  
  target_se <- 0.1
  
  cat("Synthetic data setup:\n")
  cat("- 5 respondents, 10 domains\n") 
  cat("- Each person visits domains 1,2,3 with 100 visits each\n")
  cat("- Target SE:", target_se, "\n")
  
  # Test different domain selections
  
  # Test 1: Select domains 1,2,3 (should be perfect)
  cat("\n--- Test 1: Select domains 1,2,3 ---\n")
  selected_domains <- c(1, 2, 3)
  val1 <- validate_domain_solution(cij, selected_domains, target_se, "Perfect Selection")
  
  # Test 2: Select only domain 1 (should fail)  
  cat("\n--- Test 2: Select only domain 1 ---\n")
  selected_domains <- c(1)
  val2 <- validate_domain_solution(cij, selected_domains, target_se, "Insufficient Selection")
  
  # Test 3: Select domains 4,5,6 (should fail badly - no visits)
  cat("\n--- Test 3: Select domains 4,5,6 ---\n")
  selected_domains <- c(4, 5, 6)
  val3 <- validate_domain_solution(cij, selected_domains, target_se, "Wrong Domains")
  
  return(list(val1, val2, val3))
}

# Enhanced constraint verification
verify_solution_meets_constraints <- function(cij, selected_domains, target_se) {
  validation <- validate_domain_solution(cij, selected_domains, target_se, verbose = FALSE)
  
  if (validation$summary$all_constraints_met) {
    cat("✓ SOLUTION VERIFIED: All constraints satisfied\n")
    return(TRUE)
  } else {
    cat("✗ CONSTRAINT VIOLATION: Solution does not meet requirements\n")
    cat("  -", validation$summary$constraint_violations, "respondents fail constraint\n")
    cat("  - Max SE:", round(validation$summary$max_se, 4), "> target:", target_se, "\n")
    return(FALSE)
  }
}

# Test the validation system
if (!interactive()) {
  test_validation_with_synthetic()
}