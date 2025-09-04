# Greedy Heuristic Solution for Domain Selection
# Pure R implementation without external dependencies

solve_domain_selection_heuristic <- function(cij, s) {
  n <- nrow(cij)  # respondents
  m <- ncol(cij)  # domains
  
  # Calculate minimum effective domains needed per respondent
  # From constraint: sqrt(0.25/d_i) <= s  =>  d_i >= 0.25/s^2
  min_eff_domains <- 0.25 / s^2
  
  cat("Standard error target:", s, "\n")
  cat("This requires effective domains >=", min_eff_domains, "per respondent\n")
  
  # Normalize visit weights per respondent (avoid division by zero)
  weights <- t(apply(cij, 1, function(x) {
    total <- sum(x)
    if (total == 0) rep(0, length(x)) else x / total
  }))
  
  cat("Problem size:", n, "respondents,", m, "domains\n")
  cat("Required effective domains per respondent:", round(min_eff_domains, 2), "\n")
  
  # Greedy selection
  selected_domains <- c()
  remaining_need <- rep(min_eff_domains, n)
  iteration <- 0
  
  while (any(remaining_need > 1e-6) && iteration < m) {
    iteration <- iteration + 1
    
    # Calculate coverage efficiency for each unselected domain
    best_efficiency <- 0
    best_domain <- NA
    
    for (j in 1:m) {
      if (j %in% selected_domains) next
      
      # Calculate total coverage this domain provides
      total_coverage <- 0
      for (i in 1:n) {
        coverage <- min(remaining_need[i], weights[i, j])
        total_coverage <- total_coverage + coverage
      }
      
      if (total_coverage > best_efficiency) {
        best_efficiency <- total_coverage
        best_domain <- j
      }
    }
    
    if (is.na(best_domain) || best_efficiency <= 1e-10) {
      cat("Warning: No more beneficial domains found at iteration", iteration, "\n")
      break
    }
    
    # Select the best domain
    selected_domains <- c(selected_domains, best_domain)
    
    # Update remaining needs
    for (i in 1:n) {
      covered <- min(remaining_need[i], weights[i, best_domain])
      remaining_need[i] <- remaining_need[i] - covered
    }
    
    if (iteration %% 10 == 0) {
      cat("Iteration", iteration, ": selected", length(selected_domains), "domains,",
          sum(remaining_need > 1e-6), "respondents still need coverage\n")
    }
  }
  
  return(list(
    domains = selected_domains,
    total_domains = length(selected_domains),
    remaining_need = remaining_need
  ))
}

# Validation function
validate_solution <- function(cij, selected_domains, s) {
  n <- nrow(cij)
  weights <- t(apply(cij, 1, function(x) {
    total <- sum(x)
    if (total == 0) rep(0, length(x)) else x / total
  }))
  
  results <- data.frame(
    respondent = 1:n,
    effective_domains = numeric(n),
    standard_error = numeric(n),
    meets_constraint = logical(n)
  )
  
  for (i in 1:n) {
    # Effective domains is the weighted sum of selected domains
    eff_domains <- sum(weights[i, selected_domains])
    
    # But we need to scale by total available domains to get true effective count
    # Each domain contributes proportionally to its visit weight
    total_weight_selected <- sum(weights[i, selected_domains])
    eff_domains_scaled <- length(selected_domains) * total_weight_selected
    
    std_err <- if (eff_domains_scaled > 0) sqrt(0.25 / eff_domains_scaled) else Inf
    
    results$effective_domains[i] <- eff_domains_scaled
    results$standard_error[i] <- std_err
    results$meets_constraint[i] <- std_err <= s
  }
  
  return(results)
}

# Test with realistic data
set.seed(42)
n <- 10
m <- 100
s <- 0.1  # More realistic standard error target

cat("=== GENERATING TEST DATA ===\n")
# Generate power-law distributed browsing data
cij <- matrix(0, n, m)
for (i in 1:n) {
  # Heavy-tailed visitation: most visits go to top domains
  probs <- (1:m)^(-1.5)  # Power law
  probs <- probs / sum(probs)
  visits <- rmultinom(1, 1000, probs)  # 1000 total visits per person
  cij[i, ] <- as.numeric(visits)
}

cat("Top 10 most visited domains (total visits):", 
    paste(head(order(colSums(cij), decreasing = TRUE), 10), collapse = ", "), "\n")

# Solve the problem
cat("\n=== SOLVING OPTIMIZATION PROBLEM ===\n")
solution <- solve_domain_selection_heuristic(cij, s)

# Validate results
validation <- validate_solution(cij, solution$domains, s)

# Output results
cat("\n=== SOLUTION RESULTS ===\n")
cat("Total domains selected:", solution$total_domains, "out of", m, "\n")
cat("Selected domains:", paste(head(solution$domains, 20), collapse = ", "))
if (length(solution$domains) > 20) cat(" ... (showing first 20)")
cat("\n")

cat("\n=== VALIDATION RESULTS ===\n")
print(validation)
cat("\nConstraint satisfaction summary:\n")
cat("- All constraints satisfied:", all(validation$meets_constraint), "\n")
cat("- Max standard error:", round(max(validation$standard_error), 6), "\n")
cat("- Target standard error:", s, "\n")
cat("- Respondents meeting constraint:", sum(validation$meets_constraint), "/", n, "\n")

# Domain efficiency analysis
if (solution$total_domains > 0) {
  domain_visits <- colSums(cij)[solution$domains]
  cat("\n=== DOMAIN ANALYSIS ===\n")
  cat("Selected domain visit counts (top 10):\n")
  top_domains <- head(order(domain_visits, decreasing = TRUE), 10)
  for (i in top_domains) {
    domain_idx <- solution$domains[i]
    cat("Domain", domain_idx, ": ", domain_visits[i], " visits\n")
  }
}